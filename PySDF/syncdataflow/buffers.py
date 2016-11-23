import networkx as nx
import math
from syncdataflow import core, transform, mcr, primes
from shortestpaths import bfct
from shortestpaths.core import *
from fractions import Fraction, gcd
from syncdataflow import priorityq as pq

__PARAM_VAR__       = 'var'
__PARAM_COEF__      = 'coef'
__PARAM_COST__      = 'cost'
__PARAM_WCET__      = 'wcet'
__PARAM_FLOW__      = 'flow'
__PARAM_CAPACITY__  = 'cap'
__PARAM_TOKENS__    = 'tokens'
__PARAM_PRATES__    = 'production'
__PARAM_CRATES__    = 'consumption'

def gather_variables( csdfg ):
    variables = dict()
    for u, v, data in csdfg.edges_iter( data = True ):
        if __PARAM_VAR__ in data:
            var = data[ __PARAM_VAR__ ]
            assert var not in variables, "Each variable can only occur once"

            # gather moduli
            prates = data.get( __PARAM_PRATES__, core.cyclic(1))
            crates = data.get( __PARAM_CRATES__, core.cyclic(1))
            tokens = data.get( __PARAM_TOKENS__, 0 )

            moduli = set()
            guv = gcd( prates.sum(), crates.sum())
            for prate in prates:
                for crate in crates:
                    moduli.add( guv - (tokens % guv))
                    tokens = tokens - crate
                tokens = tokens + prate + crates.sum()

            variables[ var ] = ( guv, sorted(moduli) )

    return variables

def optimise_tokens( csdfg, target_mcr ):
    # gather variables
    variables = gather_variables( csdfg )
    residues = dict()
    for v in variables:
        g, ms = variables[ v ]
        residues[ v ] = (g, ms[0])

    solution = dict()

    # pessimistic approximation
    pess = transform.single_rate_apx( csdfg, True, residues )

    # compute lower bound on solution for pessimistic approximation
    solution = optimise_tokens_relaxed( pess, Fraction( target_mcr ) / Fraction( csdfg.tpi ))

    # round solution down
    for v in solution:
        g, m = residues[ v ]
        value = g * ((solution[ v ] + m) // g)
        solution[ v ] = value

    # find optimal solution
    solution = find_optimal_solution( csdfg, variables, solution, target_mcr )

    # unfold: find an actor to unfold, exposing parallelism
    factors = dict()
    queue = pq.priority_queue()
    for v in csdfg:
        factors[ v ] = 1
        qv = csdfg.q[ v ]
        if qv > 1:
            queue[v] = -qv

    import pdb; pdb.set_trace()
    while queue:
        actor, factor = queue.pop()
        factor = -factor
        # compute unfolding: smallest prime factor in max_factor
        for p in primes.primes():
            if factor % p == 0: break

        factors[ actor ] *= p
        qv = csdfg.q[ actor ] // factors.get( actor, 1 )
        if qv > 1:
            queue[ actor ] = -qv

        unfolded = transform.unfold( csdfg, **factors )
        print("Unfolded: {}".format(factors))

        # compute lower bound
        residues.clear()
        for v in variables:
            g, _ = variables[ v ]
            residues[ v ] = (g, solution[ v ] % g)

        pess = transform.single_rate_apx( unfolded, True, residues )
        solution = compute_lowerbound( pess, variables, Fraction(target_mcr, csdfg.tpi), solution )

        # round lowerbound down
        for v in solution:
            g, m = residues[ v ]
            value = g * ((solution[ v ] + m) // g)
            solution[ v ] = value

        # find optimal solution
        solution = find_optimal_solution( unfolded, variables, solution, target_mcr )
        print("Solution: {}".format(solution))

    return solution

def test_feasible( csdfg, bindings, target_mcr ):
    # compute pessimistic approximation
    pess = transform.single_rate_apx( csdfg, bindings = bindings )

    # set path weights
    for u, v, data in pess.edges_iter( data = True ):
        tokens = data.get( __PARAM_TOKENS__, 0 )
        weight = pess.node[ v ].get( __PARAM_WCET__ )
        data['weight'] = tokens * target_mcr - weight

    # compute longest paths
    try:
        bfct.find_shortest_paths( pess, None )
        return None
    except NegativeCycleException as ex:
        return ex.cycle

def find_optimal_solution( csdfg, variables, lowerbounds, target_mcr ):
    # find optimal solution
    optimum = None
    optimal_solution = None
    idx = 0
    queue = pq.priority_queue()
    queue[ idx ] = ( sum(lowerbounds.values()), lowerbounds ); idx += 1
    while queue:
        _, (_, solution) = queue.pop()

        # find critical cycle
        cycle = test_feasible( csdfg, solution, Fraction( target_mcr ) / Fraction(csdfg.tpi) )

        if cycle is None:
            # no cycle found: solution is feasible and optimal
            return solution
        else:
            print("Infeasible solution: {}".format( solution ))
            # construct candidate solutions
            for u, v in cycle:
                data = csdfg.get_edge_data(u, v)
                if __PARAM_VAR__ in data:
                    var = data[ __PARAM_VAR__ ]

                    # increase value of var to next residue
                    g, ms = variables[ var ]
                    base = g * (solution[ var ] // g)
                    new_value = base
                    i = 0
                    while new_value <= solution[ var ]:
                        new_value = base + ms[i]; i += 1

                    # add solution
                    candidate = solution.copy()
                    candidate[ var ] = new_value
                    queue[ idx ] = ( sum(candidate.values()), candidate ); idx += 1

    assert False
    return None

def compute_lowerbound( hsdfg, variables, target_mcr, feasible_solution ):
    lowerbounds = dict()
    # go over variables
    for var in variables:
        # create weighted digraph for MCR computation
        g = nx.DiGraph()
        for u, v, data in hsdfg.edges_iter( data = True ):
            g_data = dict()
            tokens = data.get( __PARAM_TOKENS__, 0 )
            weight = hsdfg.node[ v ].get( __PARAM_WCET__ )

            if __PARAM_VAR__ in data:
                varname = data[ __PARAM_VAR__ ]
                coef = data.get( __PARAM_COEF__, 1 )
                if varname == var:
                    g_data[ __PARAM_TOKENS__ ] = coef
                else:
                    tokens += coef * feasible_solution[ varname ]

            g_data[ 'weight' ] = weight - tokens * target_mcr
            g.add_edge( u, v, **g_data )

        ratio, _, _ = mcr.compute_mcr( g, feasible_solution[ var ] * target_mcr )
        lowerbounds[ var ] = Fraction( ratio, target_mcr )

    return lowerbounds

def optimise_tokens_relaxed( hsdfg, target_mcr, partial_solution = dict() ):
    # make flow graph
    flow_graph = make_flow_graph( hsdfg, partial_solution, target_mcr )
    flow = dict()

    # remove self-loops
    for u, u in flow_graph.selfloop_edges():
        data = hsdfg.get_edge_data( u, u )
        w = hsdfg.node[u].get('wcet', 0) - data.get('tokens') * target_mcr
        if w > 0:
            return None

    flow_graph.remove_edges_from( ((u, u) for u in flow_graph.nodes_iter()))

    # saturate 2-cycles
    if not eliminate_two_cycles( flow_graph, flow ):
        return None

    # make residual graph
    res = residual_graph( flow_graph, flow )

    # compute max cycle mean
    mean, cycle, _ = mcr.compute_mcm( res, pweight = __PARAM_COST__ )

    # while max.cycle.mean is positive:
    while mean > 0:
        # saturate critical cycle
        # - find smallest capacity of cycle
        bottleneck = None
        for u, v in cycle:
            data = res.get_edge_data( u, v )
            cap = data.get( __PARAM_CAPACITY__, None )
            if cap is not None:
                bottleneck = cap if bottleneck is None else min( bottleneck, cap )

        # - increase flow by bottleneck
        if bottleneck is None:
            # unbounded flow --> primal is infeasible
            return None

        for u, v in cycle:
            if flow_graph.has_edge( u, v ):
                # forward edge: increase flow
                flow[ (u, v) ] += bottleneck
            else:
                assert flow_graph.has_edge( v, u )
                # backward edge: decrease flow
                flow[ (v, u) ] -= bottleneck
            
        # make residual graph
        res = residual_graph( flow_graph, flow )

        # compute max cycle mean
        mean, cycle, _ = mcr.compute_mcm( res, pweight = __PARAM_COST__ )

    solution = dict()
    # saturated edges: set variables
    # unsaturated edges: reset variables to zero
    saturated = set()
    g = nx.DiGraph()
    for u, v, data in hsdfg.edges( data = True ):
        tokens = data.get( __PARAM_TOKENS__, 0 ) 
        var = data.get( __PARAM_VAR__, None )
        coef = data.get( __PARAM_COEF__, 1 )
        if var is not None:
            tokens = tokens + coef * partial_solution.get( var, 0 )

        edge_weight = hsdfg.node[ v ].get( __PARAM_WCET__, 0) - tokens * target_mcr
        if var is not None and var not in partial_solution:
            cap = Fraction( 1, coef )
            f = flow[ (u, v) ]
            if f < cap:
                solution[ var ] = 0
                g.add_edge( u, v, weight = -edge_weight )
            else:
                saturated.add( (-cap, (u, v)))
        else:
            g.add_edge( u, v, weight = -edge_weight )

    # sort saturated edges by flow and add them one by one.
    # add edges with higher capacity before edges with lower capacity are added
    for _, edge in sorted(saturated):
        u, v = edge
        data = hsdfg.get_edge_data( u, v )
        var = data.get( __PARAM_VAR__, 1 )
        coef = data.get( __PARAM_COEF__, 1 )
        tokens = data.get( __PARAM_TOKENS__, 0 ) 
        edge_weight = hsdfg.node[ v ].get( __PARAM_WCET__, 0) - tokens * target_mcr

        w = path_weight( g, v, u )
        if w is not None:
            solution[ var ] = Fraction( edge_weight - w, coef ) / target_mcr
            g.add_edge( u, v, weight = -w )
        else:
            solution[ var ] = 0
            g.add_edge( u, v, weight = -edge_weight )

    return solution

def path_weight( graph, start, end ):
    # compute shortest paths
    _, dists, _ = bfct.find_shortest_paths( graph, start )
    return dists.get( end, None )
    
def eliminate_two_cycles( graph, flow ):
    # find 2-cycles
    cycles = set()
    for u, v in ((u, v) for u, v in graph.edges_iter() if graph.has_edge(v, u)):
        # found a cycle
        if (v, u) not in cycles:
            cycles.add( (u, v) )

    for u, v in cycles:
        data_uv = graph.get_edge_data( u, v )
        data_vu = graph.get_edge_data( v, u )
        cost = data_uv.get( __PARAM_COST__ ) + data_vu.get( __PARAM_COST__ )
        if cost < 0:
            # assign flow of zero, remove either edge
            flow[ (u, v) ] = flow[ (v, u) ] = 0
            graph.remove_edge( u, v )
        else:
            # increase flow as much as possible
            cap_uv = data_uv.get( __PARAM_CAPACITY__, None )
            cap_vu = data_vu.get( __PARAM_CAPACITY__, None )
            if cap_uv is None and cap_vu is None:
                # unbounded flow --> primal is infeasible
                return False

            if cap_uv is None: cap_uv = cap_vu + 1
            if cap_vu is None: cap_vu = cap_uv + 1

            if cap_uv > cap_vu:
                # saturate cycle
                flow[ (u, v) ] = flow[ (v, u) ] = cap_vu

                # remove edge (v, u) from graph
                graph.remove_edge( v, u )

            else:
                assert cap_uv is not None, "Unbounded flow on cycle [{}, {}, {}]".format(u, v, u)
                # saturate cycle
                flow[ (u, v) ] = flow[ (v, u) ] = cap_uv

                # remove edge (u, v) from graph
                graph.remove_edge( u, v )
    return True

def make_flow_graph(hsdfg, bindings, parameter):
    # each variable may occur only once (no shared capacities)
    result = nx.DiGraph()

    # graph must be HSDF
    for u, v, data in hsdfg.edges_iter( data = True ):
        flow_data = dict()
        tokens = data.get( __PARAM_TOKENS__, 0 )
        if __PARAM_VAR__ in data:
            var = data[__PARAM_VAR__]
            coef = data.get(__PARAM_COEF__, 1)

            if var in bindings:
                tokens += coef * bindings[ var ]
            else:
                # assign capacity
                flow_data[ __PARAM_CAPACITY__ ] = Fraction( 1, coef )

        flow_data[ __PARAM_COST__ ] = hsdfg.node[ v ].get( __PARAM_WCET__, 0) - tokens * parameter
        result.add_edge( u, v, **flow_data )

    return result

def residual_graph( g, flows = dict() ):
    result = nx.DiGraph()
    for u, v, data in g.edges_iter( data = True ):
        flow = flows.get( (u, v), 0 )
        capacity = data.get( __PARAM_CAPACITY__, None )

        # check if the edge has residual capacity
        if (capacity or (flow + 1)) > flow:
            assert not result.has_edge( u, v ), "Graph has a 2-cycle; can't create residual graph"
            res_data = dict()
            if capacity is not None:
                res_data[ __PARAM_CAPACITY__ ] = capacity - flow
            result.add_edge( u, v, cost = data.get( __PARAM_COST__ ), **res_data)

        # check if flow can be pushed back
        if flow > 0:
            assert not result.has_edge( v, u ), "Graph has a 2-cycle; can't create residual graph"
            res_data = dict()
            res_data[ __PARAM_CAPACITY__ ] = flow
            result.add_edge( v, u, cost = -data.get( __PARAM_COST__ ), **res_data)

    return result

def test():
    g = core.load_sdf('mp3-csdf.sdfg')
    g.remove_edge('mp3', 'mp3')
    g.remove_edge('dac', 'dac')
    # g.add_node( 'mp3', wcet = 1603621 )
    # g.add_node( 'src', wcet = n * [136577, 133824, 133760, 133750, 133748, 133863, 133844, 133955, 133882, 133862] )
    # g.add_edge( 'src', 'src', tokens = 1, production = n * 10 * [1], consumption = n * 10 * [1] )
    # g.add_edge( 'mp3', 'src', production = 1152, consumption = n * 10 * [48], capacity ='d1' )
    # hh.add_node( 'src', wcet = 136577 )
    # hh.add_edge( 'src', 'src', tokens = 1, production = 24 * [1], consumption = 24 * [1] )
    # hh.add_edge( 'mp3', 'src', production = 1152, consumption = 24 * [48], capacity ='d1' )
    assert g.is_consistent()
    sol = optimise_tokens( g, 5292 * 5000)
    print(sol)

    g = core.load_sdf('bipartite.sdfg')
    sol = optimise_tokens( g, 5 )
    print(sol)

    g = core.load_sdf('acyclic.sdfg')
    sol = optimise_tokens( g, 5 )
    print(sol)

if __name__ == "__main__":
    test()

