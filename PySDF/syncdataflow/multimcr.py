import networkx as nx
from syncdataflow import priorityq as pq
from fractions import Fraction
from shortestpaths import bfct
from syncdataflow.mcr import pdistance

__param__variable       = "var"
__param__weight         = "weight"
__param__tokens         = "tokens"
__param__coefficient    = "coef"

def compute_minimum( graph ):
    # gather variables and maximum weight of a cycle
    max_cycle_weight = 0
    variables = set()
    for u, v, data in graph.edges_iter( data = True ):
        if __param__variable in data:
            varname = data[__param__variable]
            variables.add( varname )
        max_cycle_weight += max( 0, data.get(__param__weight, 0 ))

    # compute a feasible solution
    solution = dict()
    for v in variables:
        solution[ v ] = max_cycle_weight + 1

    import pdb; pdb.set_trace()

    # main loop: while there are variables to optimise...
    while variables:
        # create graph for each parameter and compute vertex keys for each graph
        # in each graph, the parameter is substituted for tokens
        queue = pq.priority_queue()
        graphs = dict()
        trees = dict()
        for v in variables:
            graphs[ v ] = variable_graph( graph, solution, v )

            # for each graph, compute vertex keys and put them in a priority queue
            trees[ v ] = compute_vertex_keys( graphs[ v ], v, queue, solution[v] )

        # choose the smallest value from the priority queue
        (variable, _), (ratio, (v, w)) = queue.pop()
        pivot_graph = graphs[ variable ]

        # apply the smallest possible decrease to the solution:
        solution[ variable ] = -ratio
        print("Variable '{}' decreased to {}".format( variable, solution[ variable ]))

        # check if the pivot edge creates a cycle
        tree = trees[ variable ]
        for j in tree.pre_order(w):
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

                # if it does, we have found the variable's optimal value
                print("Optimal value found for variable '{}' in cycle {}".format( variable, cycle ))

                # remove the variable from the unoptimised variables
                variables.remove( variable )
    return solution

def compute_vertex_keys( graph, variable, q, parameter ):
    root = next( graph.nodes_iter() )
    # construct shortest paths tree using token = parameter
    for v, w, data in graph.edges_iter(data=True):
        tokens = data.get(__param__tokens, 0)
        weight = data.get(__param__weight, 0)
        data['dist'] = pdistance( weight, tokens )
        data['w'] = tokens * parameter - weight

    distances = dict()
    try:
        tree, _, _ = bfct.find_shortest_paths(graph, root, 'w')
        distances[root] = pdistance(0, 0)
        for v, i in tree.dfs( root ):
            if i == 0:
                dv = distances[ v ]
                for w in tree.children( v ):
                    distances[ w ] = dv + graph.get_edge_data(v, w).get('dist')

    except NegativeCycleException as ex:
        raise InfeasibleException( ex.cycle )

    # compute vertex keys
    for v in graph:
        ratio, edge = compute_vertex_key( graph, v, distances )
        q[(variable, v)] = ( -ratio, edge )

    # clean up 'dist' and 'w' parameters
    for v, w, data in graph.edges_iter( data = True ):
        del data['dist']
        del data['w']

    # return shortest paths tree that is valid for the parameter
    return tree

def compute_vertex_key( graph, vertex, distances ):
    maxratio, argmax = None, None
    # go over all incoming edges of the node
    for u, v, data in graph.in_edges_iter( data = True ):
        assert v in distances, "head of ({}, {}) has no distance".format(u, v)
        delta = distances[ u ] + data['dist'] - distances[ v ]
        if delta[1] > 0:
            ratio = Fraction(delta[0], delta[1])
            if argmax is None or ratio > maxratio:
                maxratio = ratio
                argmax = (u, v)

    return maxratio, argmax

def variable_graph( graph, sol, variable ):
    result = nx.DiGraph()
    for u, v, data in graph.edges( data = True ):
        result_data = dict(weight = data.get( __param__weight, 0 ))
        if __param__variable in data:
            varname = data[ __param__variable ]
            if variable == varname:
                # add tokens
                result_data[ __param__tokens ] = data.get( __param__coefficient, 1 )
            else:
                # subtract weight
                result_data[ __param__weight ] -= sol[ varname ]

        result.add_edge( u, v, **result_data )

    return result

