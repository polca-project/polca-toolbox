import networkx as nx
from syncdataflow import core, transform
from shortestpaths import bfct
from shortestpaths.core import NegativeCycleException
import random
from random import randint, uniform, sample, randrange
from fractions import gcd, Fraction
import math

def random_vector(vector_sum, vector_len):
    vector = [None] * vector_len
    for i in range(vector_len):
        vector[i] = uniform(0, 1)

    scale = vector_sum / sum(vector)
    vector = list(map( lambda x: math.floor(scale * x), vector ))
    plus_one = sample( range(vector_len), vector_sum - sum(vector) )
    for i in plus_one:
        vector[i] += 1

    return vector

def make_sdf_graph(g, sumrange = (1, 1), phaserange = (1, 3), wcetrange = (1, 1), seed = None):
    """ Creates a consistent SDF graph from a given graph struture.

    Parameters
    ----------

    g:          a networkx.DiGraph() that specifies the structure of the graph
    sumrange:   a tuple of ints that gives the minimum and maximum sums of rate vectors
    phaserange: a tuple of ints that gives the min and max lengths of rate vectors
    wcetrange:  a tuple of ints that gives the min and max execution time per actor phase

    NOTE: the returned graph contains no tokens
    """
    sdfg = core.SDFGraph()
    if seed is not None:
        random.seed(seed)

    q = {}
    for v in g.nodes_iter():
        q[v] = (randint(*sumrange), randint(*phaserange))
        _, phases = q[v]
        sdfg.add_node(v, wcet = [ randint(*wcetrange) for i in range(phases) ])

    for u, v in g.edges_iter():
        ru, pu = q[u]
        rv, pv = q[v]
        d = gcd(ru, rv)
        prates = random_vector( rv // d, pu )
        crates = random_vector( ru // d, pv )
        sdfg.add_edge( u, v, production = prates, consumption = crates )

    return sdfg

def ensure_liveness(g, factor = 100):
    """ Assigns tokens to channels of an SDF graph such that a minimum throughput is attained.
    Works by finding cycles of positive weight in the graph's pessimistic approximation
    and resolving them by adding sufficient tokens.

    Parameters:
    ----------

    g:      the SDF graph
    factor: indicates the maximum number of time units a single graph iteration may take,
            multiplied by the length of a fully sequential iteration.
            Lower numbers result in higher throughputs
    """

    assert g.is_consistent()
    # assert nx.is_strongly_connected( g )
    seqperiod = 0
    for v, data in g.nodes_iter( data = True ):
        wcet = data.get('wcet').sum()
        # periods = g.q[v] // data.get('phases')
        periods = 1
        seqperiod += ( wcet * periods )

    target_mcr = Fraction( seqperiod, g.tpi ) / factor

    added_tokens = 0
    m = 4 * g.number_of_edges()
    while m > 0:
        m = m - 1
        # transform the graph to its single-rate approximation
        pess = transform.single_rate_apx( g )

        # create graph where each token represents a weight of -factor
        lpg = nx.DiGraph()
        for u, v, data in pess.edges( data = True ):
            wcet = pess.node[v].get('wcet')
            tokens = data.get('tokens', 0)
            lpg.add_edge( u, v, weight = tokens * target_mcr - wcet )

        # find positive cycle
        if m == 0:
            import pdb; pdb.set_trace()
        try:
            bfct.find_shortest_paths( lpg, u )
            break
        except NegativeCycleException as ex:
            # find edge with heaviest weight
            weight, tokens = 0, 0
            max_weight = 0
            for u, v in ex.cycle:
                weight += pess.node[ v ].get('wcet')
                tokens += pess.get_edge_data(u, v).get('tokens', 0)
                suv = g.s[ (u, v) ]
                if suv > max_weight:
                    max_weight = suv
                    arg_max = (u, v)

            delta = Fraction( Fraction(weight, target_mcr) - tokens, max_weight ).__ceil__()
            edge_data = g.get_edge_data( *arg_max )
            edge_data['tokens'] = edge_data.get('tokens', 0) + delta #math.ceil(extra_factor * delta)
            added_tokens += delta
            # print("Added {} tokens".format( delta ))
    else:
        assert False, "too many iterations"
            
    return added_tokens

def random_sdf_graph(n = 5, m = 15, sumrange = (2, 5), phaserange = (1, 3), wcetrange = (1, 1), seed = None):
    p = m / (n * (n - 1))
    g = nx.fast_gnp_random_graph(n, p, seed, directed = True)
    cycle = [scc.pop() for scc in nx.strongly_connected_components(g)]
    if len(cycle) > 1:
        g.add_edge( cycle[-1], cycle[0] )
        for i in range(1, len(cycle)):
            g.add_edge( cycle[i - 1], cycle[i] )

    return make_sdf_graph(g, sumrange, phaserange, wcetrange)

def random_mrsdf_graph(n = 5, m = 15, qrange = (2, 15), wcetrange = (2,10), seed = None):
    return random_sdf_graph(n, m, qrange, (1,1), wcetrange, seed)

def random_sdf_cycle(n = 5, sumrange = (2, 5), phaserange = (1, 3), wcetrange = (1, 1), seed = None):
    if seed is not None:
        random.seed(seed)

    g = nx.DiGraph()

    g.add_edge( n, 1 )
    for v in range(1, n):
        g.add_edge( v, v + 1 )

    return make_sdf_graph(g, sumrange, phaserange, wcetrange)

def random_mrsdf_cycle(n = 5, seed = None):
    return random_sdf_cycle(n, sumrange = (2, 5), phaserange = (1, 1), seed = seed)

def random_cycle(length = 10):
    # determine normalised rates
    zs = list()
    for _ in range(length):
        zs.append( randint( 2, 10 ))

    g = core.SDFGraph()

    for i in range(length):
        g.add_node(i, wcet = 1)

    for i in range(length):
        g.add_edge( i, (i + 1) % length, production = zs[i], consumption = zs[(i + 1) % len(zs)] )

    assert g.is_consistent()
    return g

def is_live(g):
    firings = { v : 0 for v in g.nodes_iter() }
    tokens = dict()
    pending = set(g.nodes())
    
    for u, v, data in g.edges_iter( data = True ):
        tokens[ (u, v) ] = data.get( 'tokens', 0 )

    while pending:
        total = 0
        for v in g: 
            enabled = None
            assert g.node[v]['phases'] == 1

            for u, _, data in g.in_edges_iter( v, True ):
                local_enabled = tokens[ (u, v) ] // data.get('consumption')[0]
                enabled = local_enabled if enabled is None else min( enabled, local_enabled )

            # consume
            for u, _, data in g.in_edges_iter( v, True ):
                tokens[ (u, v) ] -= enabled * data.get('consumption')[0]

            # produce
            for _, w, data in g.out_edges_iter( v, True ):
                tokens[ (v, w) ] += enabled * data.get('production')[0]

            total += enabled
            firings[ v ] += enabled
            if firings[ v ] >= g.q[ v ] and v in pending:
                pending.remove( v )

        if total == 0:
            return False

    return True


def random_live_cycle(length = 10):
    g = random_cycle(length)
    while not is_live( g ):
        # pick a random edge of g
        u, v = choice( g.edges() )
        data = g.get_edge_data(u, v)
        prates = data['production']
        crates = data['consumption']
        d = gcd(prates.sum(), crates.sum())
        data['tokens'] = data.get('tokens', 0) + d

    return g

def distort(g, factor):
    h = g.copy()
    while True:
        try:
            gen_cycles = nx.simple_cycles( h )
            cycle = next( gen_cycles )
            edges = list( zip( cycle, cycle[1:] ))
            edges.append( ( cycle[-1], cycle[0] ))
            for u, v in edges:
                data = g.get_edge_data( u, v )
                data['tokens'] *= factor

            h.remove_edges_from( edges )
        except StopIteration:
            break

