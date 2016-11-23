import networkx as nx
from networkx import bellman_ford, fast_gnp_random_graph, is_directed_acyclic_graph

import random
from random import randrange, choice

from syncdataflow import priorityq as pq

from collections import deque
from fractions import Fraction
from shortestpaths import bfct
from shortestpaths.dfstree import DFSTree
from shortestpaths.forest import Forest
from shortestpaths.core import *
from math import copysign

def __is_cycle( tuples ):
    firsts = [ u for u, _ in tuples ]
    seconds = [ v for _, v in tuples ]
    return firsts == seconds[-1:] + seconds[:-1] and len( set( firsts )) == len( firsts )

class tree( object ):
    def __init__(self, pred = {}):
        self._children = {}
        self._parent = {}
        self._roots = set()

        for (child, parent) in zip( pred.keys(), pred.values() ):
            if not parent in self._children:
                self._children[parent] = {child}
            else:
                self._children[parent].add(child)

            if not child in self._children:
                self._children[child] = set()

            self._parent[child] = parent
        # all parents that are no child are roots
        for parent in self._children:
            if parent not in self._parent:
                self._roots.add(parent)

    def update_parent(self, child, parent):
        """Sets parent as the new parent of child
        If either child or parent was not in the tree, it is automatically added
        """
        if child in self._parent:
            oldparent = self._parent[child]
            self._children[oldparent].remove(child)

        self._parent[child] = parent

        if parent not in self._children:
            self._children[parent] = {child}
        else:
            self._children[parent].add(child)

        if not child in self._children:
            self._children[child] = set()


    def dfs_edges(self, root):
        """Generates the edges in the tree in depth-first order
        """
        stack = [(root, iter(self._children[root]))]
        while len(stack) > 0:
            (v, it) = stack[-1]
            try:
                w = next(it)
                stack.append( (w, iter(self._children[w])) )
                yield v, w
            except StopIteration:
                stack.pop()

    def get_path(self, ancestor, grandchild):
        """Returns the path in the tree from ancestor to grandchild,
        or None if it does not exist
        """
        path = []
        parent = self._parent[grandchild]
        path.append( (parent, grandchild) )
        while parent is not None:
            if parent is ancestor:
                path.reverse()
                return path
            else:
                child = parent
                parent = self._parent[child]
                path.append( (parent, child) )
        else:
            return None

class pdistance( tuple ):
    def __new__(self, weight, tokens):
        return super().__new__(self, (weight, tokens))

    def __add__(self, value):
        return pdistance(self[0] + value[0], self[1] + value[1])
            
    def __sub__(self, value):
        return pdistance(self[0] - value[0], self[1] - value[1])

    def eval(self, ratio):
        return self[0] - self[1] * ratio

class InfeasibleException( BaseException ):
    def __init__(self, cycle):
        super().__init__()
        self.cycle = cycle

def compute_mcr( g, estimate = None ):
    maxratio, arg_cycle = None, None
    for scc in nx.strongly_connected_component_subgraphs( g ):
        root = next( scc.nodes_iter() )
        scc_mcr, cycle = compute_mcr_component( scc, root, estimate )
        if scc_mcr is None:
            continue

        if maxratio is None or scc_mcr > maxratio:
            maxratio = scc_mcr
            arg_cycle = cycle

    forest = Forest()
    for scc in nx.strongly_connected_component_subgraphs( g, False ):
        if scc.number_of_edges() == 0:
            continue

        for ( v, w, scc_data ) in scc.edges_iter( data = True ):
            data = g.get_edge_data( v, w )

            # negate weight so that we can construct a longest paths tree for the current solution
            scc_data['w'] = data.get( 'tokens', 0 ) * maxratio - data.get( 'weight', 0 )

        root = w
        lpp_tree, _ = bfct.find_shortest_paths( scc, root, arg = 'w' )
        forest.add_forest( lpp_tree )

    return maxratio, arg_cycle, forest

def compute_mcm( g, estimate = None, pweight = 'weight' ):
    maxmean, arg_cycle = None, None
    for scc in nx.strongly_connected_component_subgraphs( g ):
        root = next( scc.nodes_iter() )
        scc_mcm, cycle = compute_mcm_component( scc, root, estimate, pweight )
        if scc_mcm is None:
            continue

        if maxmean is None or scc_mcm > maxmean:
            maxmean = scc_mcm
            arg_cycle = cycle

    forest = Forest()
    for scc in nx.strongly_connected_component_subgraphs( g, False ):
        if scc.number_of_edges() == 0:
            continue

        for ( v, w, scc_data ) in scc.edges_iter( data = True ):
            data = g.get_edge_data( v, w )

            # negate weight so that we can construct a longest paths tree for the current solution
            scc_data['w'] = maxmean - data.get( 'weight', 0 )

        root = w
        lpp_tree, _ = bfct.find_shortest_paths( scc, root, arg = 'w' )
        forest.add_forest( lpp_tree )

    return maxmean, arg_cycle, forest

def compute_mcr_component( g, root, estimate = None ):
    """ Computes the maximum cycle ratio of g.
    NOTES:
        - The weight on each edge must be non-negative
        - The number of tokens on each edge must be non-negative.
        - The graph is assumed to be strongly connected.
    """

    # initialize:
    distances = {}
    queue = pq.priority_queue()

    # print("Computing MCR for graph {}".format( g.edges( data = True )))
    # determine lower bound for mcr
    # while doing so, determine vertices with outgoing tokenless 
    init_mcr = 1
    negative_tokens_or_weights = False
    for (v, w, data) in g.edges_iter(data=True):
        tokens = data.get('tokens', 0)
        weight = data.get('weight', 0)
        init_mcr = init_mcr + max(0, weight)

        data['dist'] = pdistance(weight, tokens)
        if tokens < 0 or weight < 0:
            negative_tokens_or_weights = True

    if estimate is not None:
        # compute initial tree by computing shortest paths tree
        # print("Computing initial tree from estimate {}".format(estimate))
        for (v, w, data) in g.edges_iter(data=True):
            dist = data['dist']
            data['w'] = dist[1] * estimate - dist[0]

        try:
            tree, _ = bfct.find_shortest_paths(g, root, arg = 'w')
            distances[root] = pdistance(0, 0)
            for v, i in tree.dfs( root ):
                if i == 0:
                    dv = distances[ v ]
                    for w in tree.children( v ):
                        distances[ w ] = dv + g.get_edge_data(v, w).get('dist')
        except NegativeCycleException as ex:
            # print("Infeasible solution for estimate {}, due to cycle {}".format(estimate, [(u, v, data) for u, v, data in g.edges_iter(data = True) if (u, v) in ex.cycle]))
            raise InfeasibleException( ex.cycle )
    elif True:
        # print("Computing initial tree from estimate {}".format(init_mcr))
        # compute initial tree by computing shortest paths tree
        for (v, w, data) in g.edges_iter(data=True):
            dist = data['dist']
            data['w'] = dist[1] * init_mcr - dist[0]

        try:
            tree, _ = bfct.find_shortest_paths(g, root, arg = 'w')
            distances[root] = pdistance(0, 0)
            for v, i in tree.dfs( root ):
                if i == 0:
                    dv = distances[ v ]
                    for w in tree.children( v ):
                        distances[ w ] = dv + g.get_edge_data(v, w).get('dist')
            if False:
                # verify distances
                for v in distances:
                    distv = distances[v]
                    distv = distv[0] - distv[1] * init_mcr
                    for _, w, data in g.out_edges_iter( v, True ):
                        distw = distances[w]
                        distw = distw[0] - distw[1] * init_mcr

                        edge_dist = data['dist']
                        edge_dist = edge_dist[0] - edge_dist[1] * init_mcr

                        assert distv + edge_dist <= distw

        except NegativeCycleException as ex:
            # print("Infeasible solution for estimate {}, due to cycle {}".format(init_mcr, [(u, v, data) for u, v, data in g.edges_iter(data = True) if (u, v) in ex.cycle]))
            raise InfeasibleException( ex.cycle )
    else:
        # create tree rooted at root
        tree = DFSTree( root )

        # run Dijkstra on tokens
        distances[root] = 0
        queue[ root ] = 0
        while len(queue) > 0:
            v, _ = queue.pop()
            dv = distances[ v ]
            for _, w, data in g.out_edges_iter(v, True):
                dist = data['dist']
                dw = distances.get(w, dv + dist[1] + 1)
                if dv + dist[1] - dw < 0:
                    distances[w] = dv + dist[1]
                    queue[ w ] = distances[w]
                    tree.append_child( v, w )

        # DFS on shortest paths DAG
        # (this is the DAG induced by the union of all shortest path trees)
        pre, post, index = dict(), dict(), 0
        post_order = list()
        for v, i in tree.dfs( root ):
            if i == 0:
                pre[v] = index
                index += 1
                dv = distances[v]
                for _, w, data in g.out_edges_iter(v, True):
                    tokens = data['dist'][1]
                    dw = distances[w]
                    if dv + tokens == dw and w not in pre:
                        tree.append_child( v, w )
            else:
                post[v] = index
                index += 1
                post_order.append(v)

        # scan in topological order
        distances.clear()
        distances[root] = pdistance(0, 0)
        while post_order:
            v = post_order.pop()
            dv = distances[v]
            for _, w, data in g.out_edges_iter(v, True):
                dist = data['dist']
                dw = distances.get(w, None)
                newdist = dv + data['dist']
                if dw is None or newdist[1] < dw[1]:
                    distances[w] = newdist
                    tree.append_child( v, w )
                elif (newdist[1] == dw[1] and newdist[0] - dw[0] > 0):
                    # check that (v, w) is not a back edge!
                    if post[w] >= post[v]:
                        # back edge, MCR not defined
                        prev = v
                        cycle = list()
                        if v != w:
                            for p in tree.ancestors(v):
                                cycle.append( (p, prev) )
                                if p == w: break
                                prev = p
                            cycle.reverse()
                        cycle.append( (v, w) )

                        assert __is_cycle( cycle ), "Not a cycle: {}".format( cycle )

                        # print(cycle)
                        # print("Positive cycle found: {}".format( cycle ))
                        raise InfeasibleException(cycle)
                    distances[w] = newdist
                    tree.append_child( v, w )

    def compute_node_key(node):
        maxratio, argmax = None, None
        # go over all incoming edges of the node
        for (u, v, data) in g.in_edges_iter(nbunch=[node], data=True):
            if u in distances:
                delta = distances[u] + data['dist'] - distances[v]
                # print("Delta for {} = {}".format((u, v), delta))
                if delta[1] > 0:
                    ratio = Fraction(delta[0], delta[1])
                    if argmax is None or ratio > maxratio:
                        maxratio = ratio
                        argmax = (u, v)

        # store the node key for v
        if argmax is not None:
            queue[node] = (-maxratio, argmax)
            # print("Vertex key for {} = {}, attained by edge {}".format(node, maxratio, argmax))
        elif node in queue:
            del queue[node]

        return maxratio


    # fill priority queue:
    # go over all nodes and compute their key
    # print("Distances from root {}: {}".format(root, distances))
    for v in distances:
        compute_node_key(v)

    # pivot until cycle is found
    path_changes = 0
    pivots = 0
    while len(queue) > 0:
        (node, (ratio, (v, w))) = queue.pop()

        pivots += 1
        delta = distances[v] + g.get_edge_data(v, w)['dist'] - distances[w]
        # print("Pivoting with edge ({}, {}), delta = {}".format(v, w, delta))
        for j in tree.pre_order(w):
            # update parametric distance to j
            distances[j] += delta

            path_changes += 1

            if v == j:
                cycle = list()
                if v != w:
                    child = j
                    for p in tree.ancestors(child):
                        cycle.append( (p, j) )
                        if p == w:
                            break
                        j = p

                    cycle.reverse()

                cycle.append( (v, w) )
                assert __is_cycle( cycle ), "Found wrong cycle after pivoting with back edge ({}, {})".format( v, w )
                return -ratio, cycle
            
            for (_, k, data) in g.out_edges_iter(j, True):
                # update priority of (j, k)
                ratio_k = None
                if k in queue:
                    (ratio_k, edge) = queue[k]

                delta_k = distances[j] + data['dist'] - distances[k]
                # commented out line below to allow a negative MCR
                if delta_k[1] > 0:
                    r = -Fraction(delta_k[0], delta_k[1])
                    if ratio_k is None or r < ratio_k:
                        queue[k] = (r, (j, k))
                        # print("New arc key for ({}, {}) = {}, delta = {} + {} - {} = {}".format(j, k, -r, distances[j], data['dist'], distances[k], delta_k))

            # recompute vertex key of j
            compute_node_key(j)

        tree.append_child( v, w )
    else:
        # no cycle found, any period is admissible
        # Note that this implies that the graph is acyclic
        return None, None

def compute_mcm_component( g, root, estimate = None, pweight = 'weight' ):
    """ Computes the maximum cycle mean of g.
    NOTES:
        - The weight on each edge must be non-negative
        - The number of tokens on each edge must be non-negative.
        - The graph is assumed to be strongly connected.
    """

    # initialize:
    distances = {}
    queue = pq.priority_queue()

    # determine lower bound for mcr
    init_mcr = 1
    for (v, w, data) in g.edges_iter(data=True):
        weight = data.get(pweight, 0)
        init_mcr = init_mcr + max(0, weight)
        data['dist'] = pdistance(weight, 1)

    if estimate is not None:
        # compute initial tree by computing shortest paths tree
        # print("Computing initial tree from estimate {}".format(estimate))
        for (v, w, data) in g.edges_iter(data=True):
            dist = data['dist']
            data['w'] = estimate - dist[0]

        try:
            tree, _ = bfct.find_shortest_paths(g, root, arg = 'w')
            distances[root] = pdistance(0, 0)
            for v, i in tree.dfs( root ):
                if i == 0:
                    dv = distances[ v ]
                    for w in tree.children( v ):
                        distances[ w ] = dv + g.get_edge_data(v, w).get('dist')
        except NegativeCycleException as ex:
            # print("Infeasible solution for estimate {}, due to cycle {}".format(estimate, [(u, v, data) for u, v, data in g.edges_iter(data = True) if (u, v) in ex.cycle]))
            raise InfeasibleException( ex.cycle )
    else:
        # create tree rooted at root
        tree = DFSTree( root )

        # run Dijkstra on tokens
        distances[root] = 0
        queue[ root ] = 0
        while len(queue) > 0:
            v, _ = queue.pop()
            dv = distances[ v ]
            for _, w, data in g.out_edges_iter(v, True):
                dw = distances.get(w, None)
                if (dw is None) or dv + 1 < dw:
                    distances[w] = dv + 1
                    queue[ w ] = distances[w]
                    tree.append_child( v, w )

        # DFS on shortest paths DAG
        # (this is the DAG induced by the union of all shortest path trees)
        pre, post, index = dict(), dict(), 0
        post_order = list()
        for v, i in tree.dfs( root ):
            if i == 0:
                pre[v] = index
                index += 1
                dv = distances[v]
                for _, w, data in g.out_edges_iter(v, True):
                    dw = distances[w]
                    if dv + 1 == dw and w not in pre:
                        tree.append_child( v, w )
            else:
                post[v] = index
                index += 1
                post_order.append(v)

        # scan in topological order
        distances.clear()
        distances[root] = pdistance(0, 0)
        while post_order:
            v = post_order.pop()
            dv = distances[v]
            for _, w, data in g.out_edges_iter(v, True):
                dist = data['dist']
                dw = distances.get(w, None)
                newdist = dv + data['dist']
                if dw is None or newdist[1] < dw[1]:
                    distances[w] = newdist
                    tree.append_child( v, w )
                elif (newdist[1] == dw[1] and newdist[0] - dw[0] > 0):
                    # can't have back edges
                    assert post[w] < post[v]
                    distances[w] = newdist
                    tree.append_child( v, w )

    def compute_node_key(node):
        maxratio, argmax = None, None
        # go over all incoming edges of the node
        for u, v, data in g.in_edges_iter( node, data = True ):
            if u in distances:
                delta = distances[u] + data['dist'] - distances[v]
                # print("Delta for {} = {}".format((u, v), delta))
                if delta[1] > 0:
                    ratio = Fraction(delta[0], delta[1])
                    if argmax is None or ratio > maxratio:
                        maxratio = ratio
                        argmax = (u, v)

        # store the node key for v
        if argmax is not None:
            queue[node] = (-maxratio, argmax)
        elif node in queue:
            del queue[node]

        return maxratio


    # fill priority queue:
    # go over all nodes and compute their key
    # print("Distances from root {}: {}".format(root, distances))
    for v in distances:
        compute_node_key(v)

    # pivot until cycle is found
    path_changes = 0
    while len(queue) > 0:
        (node, (ratio, (v, w))) = queue.pop()

        delta = distances[v] + g.get_edge_data(v, w)['dist'] - distances[w]
        for j in tree.pre_order(w):
            distances[j] += delta
            if v == j:
                cycle = list()
                if v != w:
                    child = j
                    for p in tree.ancestors(child):
                        cycle.append( (p, j) )
                        if p == w:
                            break
                        j = p

                    cycle.reverse()

                cycle.append( (v, w) )
                assert __is_cycle( cycle ), "Found wrong cycle after pivoting with back edge ({}, {})".format( v, w )
                return -ratio, cycle
            
            for (_, k, data) in g.out_edges_iter(j, True):
                # update priority of (j, k)
                ratio_k = None
                if k in queue:
                    (ratio_k, edge) = queue[k]

                delta_k = distances[j] + data['dist'] - distances[k]
                if delta_k[1] > 0:
                    r = -Fraction(delta_k[0], delta_k[1])
                    if ratio_k is None or r < ratio_k:
                        queue[k] = (r, (j, k))

            # recompute vertex key of j
            compute_node_key(j)

        tree.append_child( v, w )
    else:
        # no cycle found, any period is admissible
        # Note that this implies that the graph is acyclic
        return None, None

def from_weighted_digraph(g, weight = 'weight'):
    mg = nx.DiGraph()
    for v, w, data in g.edges( data = True ):
        tokens = max(data[weight], 0)
        wgt = min(data[weight], 0)
        mg.add_edge( v, w, weight = -wgt, tokens = tokens )
    return mg

def test():
    # example graph
    g = nx.DiGraph()
    g.add_edge(1, 2, weight=1, tokens=1)
    g.add_edge(2, 4, weight=1)
    g.add_edge(2, 6, weight=2)
    g.add_edge(4, 1, weight=1)
    g.add_edge(4, 6, weight=1)
    g.add_edge(5, 3, weight=4, tokens=1)
    g.add_edge(5, 4, weight=2)
    g.add_edge(6, 5, weight=1, tokens=1)
    g.add_edge(3, 1, weight=10, tokens=1)
    g.add_edge(3, 4, weight=3, tokens=1)

    # MCR of this graph should be 9/2,
    # critical cycle = [ (1, 2), (2, 6), (6, 5), (5, 3), (3, 1) ],
    #              or: [ (1, 2), (2, 4), (4, 6), (6, 5), (5, 3), (3, 1) ]
    res = compute_mcr(g)
    return res

def test2():
    g = nx.DiGraph()
    g.add_edge(1, 2, weight = 4, tokens = 1)
    g.add_edge(2, 3, weight = 1, tokens = 1)
    g.add_edge(1, 1, weight = 9, tokens = 2)
    g.add_edge(2, 2, weight = 7, tokens = 2)
    g.add_edge(3, 3, weight = 7, tokens = 2)
    g.add_edge(3, 2, weight = 5, tokens = 1)
    g.add_edge(2, 1, weight = 8, tokens = 2)
    g.add_edge(1, 3, weight = 7, tokens = 1)

    ratio, _, _ = compute_mcr(g)
    assert ratio == 5, "Failed test"

    ratio, _, _ = compute_mcr(g, estimate = 10)
    assert ratio == 5, "Failed test"

    # reverse sign of all edge weights and repeat
    for _, _, data in g.edges_iter( data = True ):
        data['weight'] *= -1

    print("\n*** test case: max cycle ratio = -3 ***")
    ratio, _, _ = compute_mcr(g)
    assert ratio == -3, "Failed test"

    print("\n*** test case: max cycle ratio = -3 ***")
    ratio, _, _ = compute_mcr(g, estimate = 0)
    assert ratio == -3, "Failed test"

    g = nx.DiGraph()
    g.add_edge(1, 2, weight = 1, tokens = 0)
    g.add_edge(2, 3, weight = 2, tokens = 1)
    g.add_edge(3, 4, weight = 3, tokens = 0)
    g.add_edge(4, 5, weight = 4, tokens = 0)
    g.add_edge(5, 6, weight = 2, tokens = 0)
    g.add_edge(6, 7, weight = 3, tokens = 1)
    g.add_edge(7, 1, weight = 1, tokens = 0)
    g.add_edge(2, 7, weight = 5, tokens = 1)
    g.add_edge(2, 8, weight = 1)
    g.add_edge(4, 8, weight = 4)
    g.add_edge(8, 6, weight = 1)
    g.add_edge(8, 3, weight = 0, tokens = 1)
    g.add_edge(8, 7, weight = 3, tokens = 1)

    print("\n*** test case: max. cycle ratio = 8 ***")
    ratio, c, _ = compute_mcr(g)
    assert ratio == 8, "Failed test: computed ratio = {} in cycle {}".format(ratio, c)

def test_random(n = 50, p = 0.1, runs = 1000, debug = None):
    for run in range(runs) if debug is None else [debug]:
        g = fast_gnp_random_graph(n, p, seed = run + 1, directed = True)

        # add source connected to all nodes
        source = 100 * (n // 100) + 200
        for v in list(g.nodes()): g.add_edge(source, v, weight = 0, tokens = 0)

        # add random weights and tokens
        wsum = 1
        for _, _, data in g.edges_iter( data = True ):
            data['weight'] = randrange(1, 10)
            data['tokens'] = randrange(-1, 8)
            wsum += data['weight']

        # create shortest path formulation for initial tree
        for _, _, data in g.edges_iter( data = True ):
            data['sp'] = data['tokens'] * wsum - data['weight']

        # ensure that the graph admits a feasible solution
        its = 0
        while True:
            try:
                its += 1
                tree, distances = bfct.find_shortest_paths(g, source, arg = 'sp')
                break
            except NegativeCycleException as ex:
                toks = sum(map(lambda vw : g.get_edge_data(*vw).get('tokens'), ex.cycle))
                edge_data = g.get_edge_data(*choice(ex.cycle))
                edge_data['tokens'] += (1 - toks)
                edge_data['sp'] = edge_data['tokens'] * wsum - edge_data['weight']

        if debug is not None:
            import pdb; pdb.set_trace()

        negative_toks = False
        for _, _, data in g.edges_iter( data = True ):
            if data['tokens'] < 0:
                negative_toks = True; break

        print("Run {}: negative tokens: {}, iterations: {}".format(run, negative_toks, its))

        ratio, cycle = compute_mcr(g, source)
        assert ratio is not None, "Deadlocked cycle found"
        if not cycle:
            # verify that the graph is acyclic
            assert is_directed_acyclic_graph(g), "[run = {}] Graph is not acyclic".format(run)
        else:
            wsum, tsum = 0, 0
            for v, w in cycle:
                data = g.get_edge_data(v, w)
                wsum += data['weight']
                tsum += data['tokens']

            assert Fraction( wsum, tsum ) == ratio, "[run = {}] computed MCR {} does not match ratio of critical cycle {}".format(run, ratio, Fraction(wsum, tsum))

            for v, w, data in g.edges_iter( data = True ):
                data['weight'] = data['tokens'] * ratio - data['weight']

            try:
                bellman_ford(g, source)
            except Exception:
                print("Exception during run {}".format(run))

if __name__ == "__main__":
    test2()

