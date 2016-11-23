from syncdataflow import core, transform, mcr, randomsdf
from syncdataflow.core import lcm
from functools import reduce
from fractions import gcd, Fraction
import networkx as nx
import random
from random import randint, uniform, sample, randrange
import math
from shortestpaths import bfct

def make_marked_graph(hsdfg):
    for v, data in hsdfg.nodes(True):
        if 'wcet' not in data:
            raise SDFTransformError("Actor {} has no attribute 'wcet'".format(v))

        wcet = data['wcet']
        try:
            # assume wcet is iterable
            wcet = wcet[0]
            try:
                wcet = int(wcet)
            except ValueError:
                raise SDFTransformError("Actor {} has an invalid attribute 'wcet'".format(v))
        except IndexError:
            raise SDFTransformError("Actor {} has an empty attribute 'wcet'".format(v))
        except TypeError:
            # assume wcet can be interpreted as an integer
            try:
                wcet = int(wcet)
            except ValueError:
                raise SDFTransformError("Actor {} has an invalid attribute 'wcet'".format(v))
        
        for u, v, uv_data in hsdfg.in_edges_iter( v, True ):
            uv_data['weight'] = wcet

    return hsdfg

def mcr_apx(g, upper= True, estimate_mcr = None):
    assert g.is_consistent()
    assert nx.is_strongly_connected(g)
    # mrsdfg = transform.multi_rate_equivalent(g)
    pess = transform.single_rate_apx(g, upper)
    mg = make_marked_graph( pess )
    #print("size of HSDFG: {}".format(mg.number_of_nodes()))
    try:
        ratio, cycle, _ = mcr.compute_mcr( mg, estimate = Fraction( estimate_mcr, g.tpi ) if estimate_mcr is not None else None)
    except mcr.InfeasibleException as ex:
        ratio, cycle = None, ex.cycle

    if ratio is None:
        return None, cycle
    else:
        return ratio * g.tpi, cycle

def analyse_sdf(g):
    vectors = core.check_consistency(g)
    hsdfg = transform.single_rate_equivalent(g)
    mg = make_marked_graph(hsdfg)
    period, cycle, forest = mcr.compute_mcr(mg)

    # make weighted digraph
    wg = nx.DiGraph()
    for u, v, data in mg.edges( data = True ):
        wg.add_edge( u, v, weight = data.get('weight') - data.get('tokens', 0) * period )

    # compute longest paths
    _, distances = bfct.find_longest_paths( wg, cycle[0][0] )
    m = min(distances.values())
    for v in distances:
        distances[v] -= m

    return period, cycle, distances

def mcr_incremental(g):
    assert g.is_consistent()
    mcr_lo = None
    mcr_up = None
    unfolding = dict()

    h = transform.unfold( g, unfolding )
    while True:
        upper_bound, upper_cycle = mcr_apx( h, True, mcr_up )
        lower_bound, lower_cycle = mcr_apx( h, False, mcr_lo )
        assert upper_bound is None or mcr_up is None or upper_bound <= mcr_up

        gd = None
        for v, w in upper_cycle:
            qv = h.q[v]
            gd = qv if gd is None else gcd( h.q[v], gd )

        arg_min = None
        min_q = None
        for v, w in upper_cycle:
            qv = h.q[v] // gd
            if qv > 1 and (min_q is None or qv < min_q):
                min_q = qv // gd
                arg_min = v

        assert upper_bound is not None
        assert lower_bound is not None
        mcr_up = upper_bound if mcr_up is None else min( upper_bound, mcr_up )
        mcr_lo = lower_bound if mcr_lo is None else max( lower_bound, mcr_lo )

        quality = mcr_up / mcr_lo
        print("Lower: {:.3f}, upper: {:.3f}. Quality: {}. size: {:.3f}".format( float(mcr_lo), float(mcr_up), float(quality ), h.number_of_nodes() / sum(h.q.values())))
        if quality < 1.01:
            break

        assert min_q > 1
        unfolding[ arg_min ] = unfolding.get( arg_min, 1 ) * min_q
        print("Unfolding {} into {} actors...".format( arg_min, min_q ), end = '')
        h = transform.unfold( g, unfolding )
        print("done")
        pruned = set()
        prev = None
        for scc in nx.strongly_connected_components( h ):
            if prev is not None:
                pruned.update( prev )
            prev = scc

        if pruned:
            print("Pruning {} actors".format(len(pruned)))

        for v in pruned:
            h.remove_node( v )
                    
    return h

    
def find_cycle(g, cycle):
    visited = set()
    factor = reduce(gcd, [g.q[v] for v, w in cycle])

    i = 0
    while i not in visited:
        visited.add( i )
        for v, w in reversed( cycle ):
            i = core.predecessor(i, **g.get_edge_data( v, w ))
            qv = g.q[v] // factor
            i = i % qv

    v = cycle[0][0]
    if i == 0: i = qv

    # (v, i) lies on the critical cycle
    # compute cycle's weight and tokens
    # maintain parallel groups
    groups = dict([ (v, set()) for v, w in cycle ])
    weight = 0; toks = 0
    visited.clear()
    while i not in visited:
        visited.add(i)
        for v, w in reversed( cycle ):
            i = core.predecessor(i, **g.get_edge_data( v, w ))
            qv = g.q[v] // factor
            toks += (qv - i) // qv
            i = i % qv
            if i == 0: i = qv
            weight += g.node[v].get('wcet')[i]
            groups[ v ].add( i )

    return Fraction( factor * weight, toks) if toks != 0 else None, groups

def test_hsdf_analysis(n = 15, p = 0.2, runs = 10000, debug = None):
    for run in range(runs) if debug is None else [debug]:
        sdfg = random_sdf_graph(n, p, seed = run)
        vectors = core.check_consistency(sdfg)
        assert nx.is_strongly_connected( sdfg )

        # Analysis of single-rate equivalent
        hsdfg = transform.single_rate_equivalent( sdfg, vectors['q'] )
        mg = make_marked_graph(hsdfg)

        # HSDF graph may not be strongly connected, compute its strongly connected components
        condensed = nx.DiGraph()
        idx = 0
        scc_idx = {}
        graph_mcr = None
        for scc in nx.strongly_connected_components(mg):
            idx += 1
            for v in scc:
                scc_idx[v] = idx

            cycletime, _, _ = mcr.compute_mcr(nx.subgraph(mg, scc), next(iter(scc)))
            condensed.add_node(idx, mcr = cycletime, size = len(scc))
            graph_mcr = cycletime if graph_mcr is None else max(cycletime, graph_mcr)

        for v, w in mg.edges_iter():
            if scc_idx[v] != scc_idx[w]:
                condensed.add_edge( scc_idx[v], scc_idx[w] )

        critical_size = mg.number_of_nodes()
        for v, data in condensed.nodes(data = True):
            d_in = condensed.in_degree(v)
            d_out = condensed.out_degree(v)
            if d_in == 0:
                critical_size = data['size']
            if d_in == 0 and data['mcr'] < graph_mcr:
                raise AssertionError("Run {}: SCC {} has MCR {} < {}".format(run, v, data['mcr'], graph_mcr))

            if d_in > 1 or d_out > 1:
                pass
                # raise AssertionError("Run {}: SCC {}: in-degree = {}, out-degree = {}".format(run, v, d_in, d_out))

        if debug:
            import pdb; pdb.set_trace()

        unfolded, _, _ = transform.unfold_depth_first( sdfg, vectors['q'], vectors['s'], vectors['q'] )
        assert nx.is_strongly_connected( unfolded )

        print("Run {:05}: MCR = {}, condensed: {} nodes. HSDF size: {}. Compression: {:.3f}".format(run, graph_mcr, condensed.number_of_nodes(), hsdfg.number_of_nodes(), hsdfg.number_of_nodes() / critical_size))


