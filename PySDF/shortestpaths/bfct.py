from networkx import fast_gnp_random_graph, bellman_ford, DiGraph
from collections import deque
from shortestpaths.forest import Forest
from shortestpaths.core import *
from fractions import Fraction

name = "BFCT"

def find_shortest_paths( graph, root, epsilon = Fraction(1, 1), arg = 'weight'):
    roots = graph.nodes_iter() if root is None else [root]
    labelled = set()
    distances = dict()

    tree = Forest()
    for r in roots:
        labelled.add(r)
        distances[r] = 0
        tree.add_root( r )

    scanned = set()

    scans = 0

    while labelled:
        v = labelled.pop()
        # scan v
        scans += 1
        if scans == graph.number_of_nodes() * graph.number_of_edges() * 2:
            print( graph.edges( data = True ))
            import pdb; pdb.set_trace()
            print( tree.pre_post_indices() )

        dv = distances.get(v)
        for _, w, data in graph.out_edges_iter( v, True ):
            weight = data.get(arg, 1)
            dw = distances.get(w, dv + weight + 1)
            delta = dw - dv - weight
            if delta > 0:
                distances[w] = dw - delta
                # print("Disassembling subtree while relaxing ({}, {})".format(v, w))
                if w in tree:
                    y = tree.after( (w, 0) )
                    while y != (w, 1):
                        if y[1] == 0:
                            # pre visit
                            if y[0] == v:
                                cycle = list()
                                child = v
                                for p in tree.ancestors(child):
                                    cycle.append( (p, child) )
                                    if p == w:
                                        break
                                    child = p

                                cycle.reverse()
                                cycle.append( (v, w) )
                                raise NegativeCycleException(cycle)
                            distances[y[0]] -= (delta - epsilon)
                            y = tree.after(y)
                        else:
                            # post visit: delete
                            z = y[0]
                            y = tree.after(y)
                            tree.cut_subtree( z )
                            if z in labelled:
                                labelled.remove( z )
                            assert tree.after( (z, 0) ) == (z, 1)
                            assert tree.before( (z, 0) ) == tree.after( (z, 1) ) == None

                labelled.add( w )
                tree.append_child( v, w )

    return tree, distances

def find_longest_paths( graph, root, epsilon = Fraction(1, 1), arg = 'weight' ):
    roots = graph.nodes_iter() if root is None else [root]
    labelled = set()
    distances = dict()
    tree = Forest()

    for r in roots:
        labelled.add(r)
        distances[r] = 0
        tree.add_root( r )

    scanned = set()

    scans = 0

    while labelled:
        v = labelled.pop()
        # scan v
        scans += 1
        if scans == graph.number_of_nodes() * graph.number_of_edges() * 2:
            print( graph.edges( data = True ))
            import pdb; pdb.set_trace()
            print( tree.pre_post_indices() )

        dv = distances.get(v)
        for _, w, data in graph.out_edges_iter( v, True ):
            weight = data.get(arg, 1)
            dw = distances.get(w, dv + weight - 1)
            delta = dw - dv - weight
            if delta < 0:
                distances[w] = dw - delta
                # print("Disassembling subtree while relaxing ({}, {})".format(v, w))
                if w in tree:
                    y = tree.after( (w, 0) )
                    while y != (w, 1):
                        if y[1] == 0:
                            # pre visit
                            if y[0] == v:
                                cycle = list()
                                child = v
                                for p in tree.ancestors(child):
                                    cycle.append( (p, child) )
                                    if p == w:
                                        break
                                    child = p

                                cycle.reverse()
                                cycle.append( (v, w) )
                                raise NegativeCycleException(cycle)
                            distances[y[0]] -= (delta + epsilon)
                            y = tree.after(y)
                        else:
                            # post visit: delete
                            z = y[0]
                            y = tree.after(y)
                            tree.cut_subtree( z )
                            if z in labelled:
                                labelled.remove( z )
                            assert tree.after( (z, 0) ) == (z, 1)
                            assert tree.before( (z, 0) ) == tree.after( (z, 1) ) == None

                labelled.add( w )
                tree.append_child( v, w )

    return tree, distances

