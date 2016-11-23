from functools import reduce
from fractions import gcd, Fraction
from syncdataflow import core, integers as imath, randomsdf
import networkx as nx

def analyse_cycle_multiplicity(g, cycle):
    visited = {}
    factor = reduce(gcd, [g.q[v] for v, w in cycle])

    root = cycle[0][0]
    multiplicity = 0
    for j in range( g.q[root] // factor ):
        idx = 0
        v = root
        i = j
        while (v, i) not in visited:
            visited[ (v, i) ] = j
            idx = (idx - 1) % len(cycle)
            i = core.predecessor(i, **g.get_edge_data(*cycle[idx]))
            v = cycle[idx][0]
            qv = g.q[v] // factor
            i = ((i - 1) % qv) + 1

        if visited[(v, i)] == j:
            multiplicity += 1

    return multiplicity

def analyse_cycle(g, cycle):
    maxgain, arg = Fraction(0, 1), None
    for idx in range(len(cycle)):
        data = g.get_edge_data(* cycle[idx] )
        pr = data.get('production', core.cyclic(1))
        cr = data.get('consumption', core.cyclic(1))
        toks = data.get('tokens', 0)
        gain = Fraction(pr.sum(), cr.sum())
        if gain > maxgain:
            maxgain = gain
            arg = idx

    data = g.get_edge_data(* cycle[arg] )
    pr = data.get('production', core.cyclic(1))
    cr = data.get('consumption', core.cyclic(1))
    toks = data.get('tokens', 0)

    if len(pr) == len(cr) == 1:
        _gcd = gcd(pr.sum(), cr.sum())
        pr = pr.sum() // _gcd
        cr = cr.sum() // _gcd
        toks = toks // _gcd
        # solve  toks + i * pr = 0 (mod cr)
        i = (-toks * imath.modinv( pr, cr)) % cr
        return find_cycle(g, cycle, arg, i)

    return find_cycle(g, cycle, 0)

def find_cycle(g, cycle, idx = 0, firing = None):
    visited = set()
    factor = reduce(gcd, [g.q[v] for v, w in cycle])

    v = cycle[idx][0]
    i = g.q[v] // factor if firing is None else (firing - 1) % (g.q[v] // factor) + 1
    while (v, i) not in visited:
        visited.add( (v, i) )

        idx = (idx - 1) % len(cycle)
        i = core.predecessor(i, **g.get_edge_data(*cycle[idx]))
        v = cycle[idx][0]
        qv = g.q[v] // factor
        i = ((i - 1) % qv) + 1

    # (v, i) lies on the critical cycle
    # compute cycle's weight and tokens
    groups = dict([ (v, set()) for v, w in cycle ])
    idx = (idx - 1) % len(cycle)
    j = core.predecessor(i, **g.get_edge_data( *cycle[idx] ))
    w = cycle[idx][0]
    qw = g.q[w] // factor
    toks = (1 + (0 - j) // qw)
    j = ((j - 1) % qw) + 1
    weight = g.node[w].get('wcet')[j]
    groups[w].add(j)

    cyclelen = 1
    while (w, j) != (v, i):
        cyclelen += 1
        idx = (idx - 1) % len(cycle)
        j = core.predecessor(j, **g.get_edge_data( *cycle[idx] ))
        w = cycle[idx][0]
        qw = g.q[w] // factor
        toks += (1 + (0 - j) // qw)
        j = ((j - 1) % qw) + 1
        weight += g.node[w].get('wcet')[j]
        groups[w].add(j)

    return Fraction( factor * weight, toks) if toks != 0 else None, groups

def test(debug = None):
    from syncdataflow.core import SDFGraph, check_consistency
    g = SDFGraph()
    g.add_node(1, wcet = 1)
    g.add_node(2, wcet = 1)
    g.add_node(3, wcet = 1)
    g.add_edge(1, 2, production = 3, consumption = 5)
    g.add_edge(2, 3, production = 5, consumption = 2)
    g.add_edge(3, 1, production = 2, consumption = 3, tokens = 17)

    vectors = check_consistency(g)
    cycletime, parallelism, retime = analyse_cycle(g, vectors['q'], [(3, 1), (1, 2), (2, 3)])
    assert cycletime == Fraction(6, 1)
    print(parallelism, retime)

    # generate random cycle
    for run in range(100) if debug is None else [debug]:
        n = 3
        g = nx.DiGraph()
        cycle = [(i, (i + 1) % n ) for i in range(n)]
        for v, w in cycle:
            g.add_edge(v, w)

        g = randomsdf.make_sdf_graph(g, (2, 5), (1, 1), seed = run)
        vectors = check_consistency(g)
        print("run {}: ".format(run), end="")
        if debug is not None:
            print()
            import pdb; pdb.set_trace()

        analyse_cycle(g, vectors['q'], cycle)
        print("")


