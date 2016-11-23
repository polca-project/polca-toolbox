import networkx as nx
from syncdataflow import core

def minpar(sdfg, v):
    """ Computes the minimum number of firings of actor v
    that can be executed in parallel without affecting the graph's throughput
    """

    # Visit predecessors of v depth-first
    visited = {}
    postorder = list()
    stack = [v]
    while stack:
        v = stack[-1]
        if v not in visited:
            visited[v] = 0
            for u in sdfg.predecessors_iter( v ):
                if u not in visited:
                    stack.append( u )
        else:
            stack.pop()
            if visited[v] == 0:
                # post visiting
                postorder.append(v)
                visited[v] = 1

    # push all tokens to v
    marking = {}
    for v in postorder:
        # determine min. firings
        parallel = None
        for u, _, data in sdfg.in_edges( v, True ):
            tokens = data.get('tokens', 0)
            crate = data.get('consumption', cyclic(1))
            marking[ (u, v) ] = tokens
            parallel = tokens // crate if parallel is None else min(parallel, tokens // crate)

        # consume
        for u, _, data in sdfg.in_edges( v, True ):
            crate = data.get('consumption', cyclic(1))
            marking[ (u, v) ] -= ( parallel * crate )

        # produce
        for _, w, data in sdfg.out_edges( v, True ):
            tokens = marking[ (v, w) ]
            prate = data.get('production', cyclic(1))
            marking[ (v, w) ] += ( parallel * prate )

    # clear marking
    marking.clear()

    # produce
    for _, w, data in sdfg.out_edges( v, True ):
        prate = data.get('production', cyclic(1))
        marking[ (v, w) ] = ( parallel * prate )

    # once more
    for v in postorder:
        # determine min. firings
        parallel = None
        for u, _, data in sdfg.in_edges( v, True ):
            tokens = data.get('tokens', 0)
            crate = data.get('consumption', cyclic(1))
            marking[ (u, v) ] = tokens
            parallel = tokens // crate if parallel is None else min(parallel, tokens // crate)

        # consume
        for u, _, data in sdfg.in_edges( v, True ):
            crate = data.get('consumption', cyclic(1))
            marking[ (u, v) ] -= ( parallel * crate )

        # produce
        for _, w, data in sdfg.out_edges( v, True ):
            tokens = marking[ (v, w) ]
            prate = data.get('production', cyclic(1))
            marking[ (v, w) ] += ( parallel * prate )



def analyse_cycles(sdfg):
    vectors = core.check_consistency( sdfg )
    s = vectors['s']
    q = vectors['q']
    print("HSDF graph size: {}".format( sum(q.values()) ))
    par = {}
    for cycle in nx.simple_cycles( sdfg ):
        edges = [ (cycle[i - 1], cycle[i]) for i in range(len(cycle)) ]
        wtsum = 0
        multiple = 1
        z = {}
        for v, w in edges:
            data = sdfg.get_edge_data( v, w )
            tokens = data.get('tokens', 0)
            prates = data.get('production', core.cyclic(1))

            wtsum += s[ (v, w) ] * tokens
            z[v] = prates.sum() * s[ (v, w) ]
            multiple = core.lcm( multiple, z[v] )

        if wtsum % multiple == 0:
            for v in cycle:
                parv = wtsum // z[ v ]
                par[v] = parv if v not in par else min(par[v], parv)

        print("Cycle {}: tokens = {:.3f}, integral: {}".format( cycle, wtsum / multiple, wtsum % multiple == 0 ))

    for v in par:
        if q[v] % par[v] == 0:
            q[v] = q[v] // par[v]
        elif par[v] % q[v] == 0:
            q[v] = 1
    
    print("New HSDF graph size: {}".format( sum(q.values()) ))


