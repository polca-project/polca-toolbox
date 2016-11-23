import networkx as nx
import syncdataflow.core as core
import syncdataflow.mcr as mcr

from syncdataflow.core import cyclic, predecessor, check_consistency
from fractions import gcd, Fraction
from syncdataflow.integers import xgcd, lcm

def apply_vars(g, bindings):
    result = core.SDFGraph()
    for v, data in g.nodes_iter( data = True ):
        result.add_node( v, wcet = data['wcet'] )

    for v, w, data in g.edges_iter( data = True ):
        prates = data.get('production', cyclic(1))
        crates = data.get('consumption', cyclic(1))
        tokens = data.get('tokens', 0)
        if 'var' in data:
            var = data.get('var')
            tokens = tokens + bindings.get(var, 0)

        result.add_edge( v, w,
            production = prates,
            consumption = crates,
            tokens = tokens)

    return result        

def remove_sinks(g):
    trash = set()
    for v in g:
        if g.out_degree(v) == 0:
            trash.add(v)

    for v in trash:
        g.remove_node(v)

    return len(trash)

def sample_prates(vector, offset, period):
    """ Returns the production rates of unfolded actor [offset]
    if the producing actor is unfolded period times.
    NOTE: offset is 0-based
    """
    vector = cyclic(vector)
    pattern = [None] * (len(vector) // gcd( len(vector), period ))
    for i in range(len(pattern)):
        start = offset + i * period
        end = start + period
        pattern[i] = vector.sum(start, end)

    token_delta = vector.sum(stop = offset)
    return token_delta, cyclic(pattern)

def sample_crates(vector, offset, period):
    """ Returns the consumption rates of actor[offset]
    if the consuming actor is unfolded period times.
    NOTE: offset is 0-based
    """
    vector = cyclic(vector)
    pattern = [None] * (len(vector) // gcd( len(vector), period ))
    for i in range(len(pattern)):
        start = offset + 1 + (i - 1) * period
        end = start + period
        pattern[i] = vector.sum(start, end)

    token_delta = pattern[0] - vector.sum(stop = offset + 1)
    return token_delta, cyclic(pattern)

def token_predecessor_bounds( g, uv, vw ):
    assert uv[1] == vw[0]
    assert g.is_consistent()
    u, v = uv
    _, w = vw
    suv = g.s[ uv ]
    svw = g.s[ vw ]

    arg = suv * g.get_edge_data(u, v).get('tokens', 0)
    prates = g.get_edge_data(v, w).get('production', cyclic(1))
    crates = g.get_edge_data(u, v).get('consumption', cyclic(1))

    minarg, maxarg = None, None
    for i in range( g.node[v].get('phases')):
        maxarg = max( maxarg, arg ) if maxarg is not None else arg
        arg -= suv * crates[i]
        minarg = min( minarg, arg + svw * prates.sum()) if minarg is not None else arg + svw * prates.sum()
        arg += svw * prates[i]

    return minarg, maxarg

def tt_predecessor( g, u, v, w, k ):
    tokens = g.get_edge_data(u, v).get('tokens', 0)
    prates = g.get_edge_data(v, w).get('production', cyclic(1))
    crates = g.get_edge_data(u, v).get('consumption', cyclic(1))
    # determine psi
    phi = g.node[v].get('phases')
    minval = None
    for i in range( phi ):
        value = (k - sum(prates[:i]) + prates.sum() - 1) // prates.sum()
        value = value * phi + i
        minval = value if minval is None else min(value, minval)

    result1 = sum(crates[:minval]) - tokens

    minval = None
    for i in range( phi ):
        value = (k - sum(prates[:i]) + prates.sum() - 1) // prates.sum()
        value = value * crates.sum() + sum(crates[:i]) - tokens
        minval = value if minval is None else min(value, minval)

    result2 = minval

    maxval = None
    for i in range( phi ):
        value = (k - 1 - sum(prates[:i])) // prates.sum()
        value = value * crates.sum() + sum(crates[:i + 1]) - tokens
        maxval = value if maxval is None else max(value, maxval)

    result3 = maxval

    return result1, result2, result3

def lin_bounds(g, v, w):
    tokens = g.get_edge_data(v, w).get('tokens', 0)
    prates = g.get_edge_data(v, w).get('production', cyclic(1))
    crates = g.get_edge_data(v, w).get('consumption', cyclic(1))
    phi_v = g.node[v].get('phases')
    phi_w = g.node[w].get('phases')
    g_vw = gcd( prates.sum(), crates.sum() )
    qv = g.q[v]
    qw = g.q[w]
    minval = None
    maxval = None

    for i in range( phi_v ):
        for j in range( phi_w ):
            value = Fraction( g_vw * phi_v * (tokens + sum(prates[:i]) - sum(crates[:j])) // g_vw, prates.sum()) - i + j * Fraction(qv, qw)
            maxval = value if maxval is None else max(value, maxval)
            value = Fraction( g_vw * phi_v * (tokens + g_vw + sum(prates[:i]) - sum(crates[:j])) // g_vw, prates.sum()) - i + j * Fraction(qv, qw)
            minval = value if minval is None else min(value, minval)

    return minval - 1, maxval

def tt_bounds( g, u, v, w ):
    assert g.is_consistent()
    suv = g.s[(u, v)]
    svw = g.s[(v, w)]
    tokens = g.get_edge_data(u, v).get('tokens', 0)
    prates = g.get_edge_data(v, w).get('production', cyclic(1))
    crates = g.get_edge_data(u, v).get('consumption', cyclic(1))
    phi = g.node[v].get('phases')
    slope = Fraction( crates.sum(), prates.sum() )
    
    minval = None
    maxval = None
    for i in range( phi ):
        value = svw * sum(prates[:i]) + suv * (tokens - sum(crates[:i + 1]))
        minval = value if minval is None else min(value, minval)

        value = svw * sum(prates[:i]) + suv * (tokens - sum(crates[:i]))
        maxval = value if maxval is None else max(value, maxval)

    return minval + svw, maxval

def pessimistic_bottlenecks(**kwargs):
    prates = kwargs.get('production', cyclic(1))
    crates = kwargs.get('consumption', cyclic(1))
    tokens = kwargs.get('tokens', 0)
    g = gcd(prates.sum(), crates.sum())
    avg_prate = Fraction( prates.sum(), len(prates) )
    avg_crate = Fraction( crates.sum(), len(crates) )
    minval = None
    maxval = None
    arg_i = None
    arg_j = None
    delta = tokens
    for i in range(len(prates)):
        for j in range(len(crates)):
            val_ij = g * (delta // g) - i * avg_prate + j * avg_crate

            if minval is None or val_ij < minval:
                minval = val_ij
                arg_i = i + 1
                arg_j = j + 1
            delta -= crates[j]
        delta = delta + prates[i] + crates.sum()

    return minval + g - avg_prate, arg_i, arg_j

def predecessor_lin_bounds(moduli = dict(), bindings = dict(), **kwargs):
    prates = kwargs.get('production', cyclic(1))
    crates = kwargs.get('consumption', cyclic(1))
    tokens = kwargs.get('tokens', 0)
    g = gcd(prates.sum(), crates.sum())
    avg_prate = Fraction( prates.sum(), len(prates) )
    avg_crate = Fraction( crates.sum(), len(crates) )
    minval = None
    maxval = None
    delta = tokens
    if 'var' in kwargs:
        varname = kwargs['var']
        if varname in bindings:
            delta = delta + bindings[ varname ]
        else:
            _, modulus = moduli.get(varname, (1, 0))
            delta = delta + modulus

    for i in range(len(prates)):
        for j in range(len(crates)):
            val_ij = g * (delta // g) - i * avg_prate + j * avg_crate

            minval = val_ij if minval is None else min( minval, val_ij )
            maxval = val_ij if maxval is None else max( maxval, val_ij )
            delta -= crates[j]
        delta = delta + prates[i] + crates.sum()

    return maxval, minval + g - avg_prate

def parallelise(g, vectors = None, **kwargs):
    vectors = vectors or kwargs
    result = core.SDFGraph()
    for v, data in g.nodes_iter( data = True ):
        parallelism = core.cyclic(vectors.get(v, [1]))
        wcet = data.get('wcet')
        assert len(parallelism) > 0, "Invalid parallelism for {}: {} (vectors: {})".format(v, parallelism, vectors)
        result.add_node( v, wcet = wcet * len( parallelism ))

    for v, w, data in g.edges_iter( data = True ):
        prates = data.get('production', core.cyclic(1))
        crates = data.get('consumption', core.cyclic(1))
        tokens = data.get('tokens', 0)

        parallelism_v = core.cyclic(vectors.get(v, 1))
        parallelism_w = core.cyclic(vectors.get(w, 1))

        s = 0
        new_prates = list()
        for i in range( (len( prates ) * len( parallelism_v )) // gcd( len( prates ), sum( parallelism_v ))):
            new_prates.append( prates.sum( s, s + parallelism_v[ i ] ))
            s = s + parallelism_v[ i ]

        s = 0
        new_crates = list()
        for i in range( (len( crates ) * len( parallelism_w )) // gcd( len( crates ), sum( parallelism_w ))):
            new_crates.append( crates.sum( s, s + parallelism_w[ i ] ))
            s = s + parallelism_w[ i ]

        result.add_edge( v, w,
            production = new_prates,
            consumption = new_crates,
            tokens = tokens )

    return result

def unfold( sdfg, dct = None, **periods ):
    if dct is not None:
        periods.update( dct )

    result = core.SDFGraph()
    for v, data in sdfg.nodes_iter( data = True ):
        Tv = periods.get(v, 1)
        for i in range(Tv):
            result.add_node( (v, i + 1) if Tv > 1 else v, wcet = data.get('wcet', core.cyclic(0))[i::Tv])

    for v, w, data in sdfg.edges_iter( data = True ):
        Tv = periods.get(v, 1)
        Tw = periods.get(w, 1)
        prates = data.get('production', core.cyclic(1))
        crates = data.get('consumption', core.cyclic(1))
        tokens = data.get('tokens', 0)
        plen = len(prates)

        # consuming actor in unfolded graph has a single phase
        # and consumption rate equal to periods_w * crates.sum()
        csum = crates.sum() * Tw // gcd(Tw, len( crates ))

        # sum of production rates in multi-rate equivalent of unfolded graph
        psum = prates.sum() * Tv // gcd(Tv, len( prates ))

        gm = gcd( csum, psum )
        gvw = gcd( csum, prates.sum() )

        # multiplicative inverse of (psum // gvw) modulo (gm // gvw)
        g_cd, mulinv, _ = xgcd( prates.sum() // gvw, gm // gvw )
        assert g_cd == 1

        # iterate consuming actors
        for j in range(Tw):
            # determine which incoming channels must be added
            incoming = set()

            # see Algorithm ? in PhD thesis
            sols = set()
            for n0 in range( len( crates ) // gcd( len( crates ), Tw )):
                for i0 in range( plen ):
                    delta_i_n = tokens + prates.sum(stop = i0 + 1) - crates.sum(stop = j + 1 + n0 * Tw )
                    delta_i1_n = tokens + prates.sum(stop = i0) - crates.sum(stop = j + 1 + n0 * Tw )
                    sol_min = (gvw - delta_i_n - 1) // gvw
                    sol_max = (gvw - delta_i1_n - 1) // gvw

                    for sol in range(sol_min, sol_max):
                        s = (i0 + sol * mulinv * plen) % gcd(Tv, plen * gm // gvw)
                        sols.add( s )

            for i_residue in sols:
                for i0 in range(i_residue, Tv, gcd(Tv, plen * gm // gvw)):
                    incoming.add( i0 )

            # compute consumption rates for incoming channels
            tokens_c, incoming_crates = sample_crates( crates, j, Tw )

            # add incoming channels:
            for i in incoming:
                # compute production rates
                tokens_p, incoming_prates = sample_prates( prates, i, Tv )

                extra_data = dict()
                if 'var' in data:
                    extra_data['var'] = data['var']

                # add channel
                vi = (v, i + 1) if Tv > 1 else v
                wj = (w, j + 1) if Tw > 1 else w
                result.add_edge( vi, wj,
                    production = incoming_prates,
                    consumption = incoming_crates,
                    tokens = tokens + tokens_c + tokens_p,
                    **extra_data)

    return result

def multi_rate_equivalent( sdfg ):
    result = core.SDFGraph()
    for v, data in sdfg.nodes_iter( data = True ):
        phi = data.get('phases', 1)
        for i in range(phi):
            result.add_node((v, i + 1), wcet = data.get('wcet', core.cyclic(0))[i])

    for v, w, data in sdfg.edges_iter( data = True ):
        Tv = sdfg.node[v].get('phases', 1)
        Tw = sdfg.node[w].get('phases', 1)
        prates = data.get('production', core.cyclic(1))
        crates = data.get('consumption', core.cyclic(1))
        tokens = data.get('tokens', 0)
        plen = len(prates)
        assert Tv == len(prates)
        assert Tw == len(crates)

        # consuming actor in unfolded graph has a single phase
        # and consumption rate equal to crates.sum()
        csum = crates.sum()

        # sum of production rates in multi-rate equivalent of unfolded graph
        psum = prates.sum()

        gvw = gcd( csum, psum )

        # iterate consuming actors
        for j in range(Tw):
            # determine which incoming channels must be added
            # see Algorithm ? in PhD thesis
            for i in range( plen ):
                delta_i_n = tokens + prates.sum(stop = i + 1) - crates.sum(stop = j + 1 )
                delta_i1_n = tokens + prates.sum(stop = i) - crates.sum(stop = j + 1 )
                sol_min = (gvw - delta_i_n - 1) // gvw
                sol_max = (gvw - delta_i1_n - 1) // gvw

                if sol_min < sol_max:
                    # add channel
                    extra_data = dict()
                    if 'var' in data:
                        extra_data['var'] = data['var']

                    result.add_edge( (v, i + 1), (w, j + 1),
                        production = psum,
                        consumption = csum,
                        tokens = tokens + prates.sum(stop = i) + crates.sum(start = j + 1),
                        **extra_data)

    return result

def single_rate_equivalent( sdfg ):
    if not sdfg.is_consistent():
        raise ValueError('Inconsistent SDF graph')

    hsdfg = nx.DiGraph()
    for v in sdfg.q:
        assert 'wcet' in sdfg.node[v], "Missing 'wcet' attribute for node {}".format(v)
        wcets = cyclic(sdfg.node[v]['wcet'])

        for i in range(sdfg.q[v]):
            hsdfg.add_node( (v, i + 1), wcet = wcets[i % len(wcets)] )

        for u, _, data in sdfg.in_edges_iter( v, True ):
            prates = data.get('production', cyclic(1))
            crates = data.get('consumption', cyclic(1))
            tokens = data.get('tokens', 0)
            for j in range(sdfg.q[v]):
                extra_data = dict()
                i = predecessor(j + 1, **data)
                hsdfg.add_edge( (u, (i - 1) % sdfg.q[u] + 1), (v, j + 1), tokens = (sdfg.q[u] - i) // sdfg.q[u], **extra_data )

    assert hsdfg.number_of_nodes() == sum(sdfg.q.values()), "Size of HSDF graph is equal to sum of repetition vector"
    return hsdfg

def single_rate_apx( sdfg, is_pessimistic = True, moduli = dict(), bindings = dict() ):
    if not sdfg.is_consistent():
        raise ValueError('Inconsistent SDF graph')

    hsdfg = nx.DiGraph()
    for v, data in sdfg.nodes(data = True):
        wcet = data['wcet']
        hsdfg.add_node(v, wcet = max(wcet) if is_pessimistic else min(wcet))

    for v, w, data in sdfg.edges(data = True):
        (lo, up) = predecessor_lin_bounds( moduli = moduli, bindings = bindings, **data )
        delay = up if is_pessimistic else lo
        toks = sdfg.s[ (v, w) ] * delay
        assert toks.denominator == 1, "delay({}, {}) = {}, s[({}, {})] = {}".format(v, w, delay, v, w, sdfg.s[ (v, w) ])
        h_data = dict( tokens = toks.numerator )
        if 'var' in data:
            varname = data['var']
            if varname not in bindings:
                h_data['var'] = varname
                h_data['coef'] = sdfg.s[ (v, w) ]

        hsdfg.add_edge( v, w, **h_data )
    return hsdfg

def fire( sdfg, firings_dict = None, **attr ):
    """ Fires actors in the SDF graph sdfg.
    Which actors to fire how many times is specified either as keyword arguments, or through
    the dictionary firings_dict.
    Keys specify which actors to fire, values specify how many times.
    Negative values "rewind" firings.

    NOTE: rate vectors are updated accordingly: if an actor with production rate vector [a,b,c]
    fires once, then the production rate vector will have changed into [b,c,a]
    """
    if firings_dict is not None:
        attr.update( firings_dict )

    for v in attr:
        firings = attr[ v ]
        if firings > 0:
            for _, w, data in sdfg.out_edges_iter( v, True ):
                prates = data.get('production', cyclic(1))
                data['tokens'] = data.get('tokens', 0) + prates.sum(0,firings)
                data['production'] = prates[firings:]

            for v, _, data in sdfg.in_edges_iter( v, True ):
                crates = data.get('consumption', cyclic(1))
                data['tokens'] = data.get('tokens', 0) - crates.sum(0, firings)
                data['consumption'] = crates[firings:]

        elif firings < 0:
            # negative indices run from -1 to and including firings
            # subtract one to have correct bounds for the slices used below
            for _, w, data in sdfg.out_edges_iter( v, True ):
                prates = data.get('production', cyclic(1))
                data['tokens'] = data.get('tokens', 0) - prates.sum(firings, 0)
                data['production'] = prates[firings:]

            for v, _, data in sdfg.in_edges_iter( v, True ):
                crates = data.get('consumption', cyclic(1))
                data['tokens'] = data.get('tokens', 0) + crates.sum(firings, 0)
                data['consumption'] = crates[firings:]

def channel_tokens( np, nc, sdfg = None, edge = None, **kwargs ):
    """
    Computes the number of tokens that are on the specified channel after np productions
    and nc consumptions.
    """
    data = sdfg.get_edge_data(*edge) if sdfg is not None else kwargs
    prates = data.get('production', cyclic(1))
    crates = data.get('consumption', cyclic(1))
    tokens = data.get('tokens', 0)

    return tokens + sum(prates[:np]) - sum(crates[:nc])

def fceil(a, b):
    return (a + b - 1) // b

def to_weighted_graph(g, parameter = 1):
    result = nx.DiGraph()
    for u, v, data in g.edges_iter( data = True ):
        w = g.node[v].get('wcet', 0)
        result_data = dict( weight = Fraction(w, parameter) - data.get('tokens', 0) )
        if 'var' in data:
            result_data['var'] = data['var']
            result_data['coef'] = data.get('coef', 1)

        result.add_edge( u, v, **result_data )

    return result

def retime_vectorise(g, a, b):
    """ Reimes and vectorises actor b, such that its throughput-critical parallelism is enforced.
    NOTE: Assumes that the execution time of actor b is constant.

    Parameters:
    g       core.SDFGraph
    a       source actor
    b       target actor

    Returns a tuple (r, v), where r is the number of firings of b that were simulated, and
    v is the blocking vector used in the vectorisation.
    """

    data = g.get_edge_data(a, b)
    prates = data.get('production', cyclic(1))
    crates = data.get('consumption', cyclic(1))
    tokens = data.get('tokens', 0)

    periods = (crates.sum() // gcd( prates.sum(), crates.sum())) * len(prates)
    cons_q = (prates.sum() // gcd( prates.sum(), crates.sum())) * len(crates)
    firings = 0
    while (tokens % crates.sum(0, cons_q)) >= crates.sum(0, firings + 1):
        firings = firings + 1

    j = firings + ( tokens // crates.sum(0, cons_q )) * cons_q
    blocking_vector = list()
    blocking_a = list()
    i0 = 0
    for i in range(periods):
        k = j
        while sum(crates[:k + 1]) - sum(prates[:i + 1]) <= tokens:
            k = k + 1
        if k > j:
            blocking_vector.append( k - j )
            j = k
            blocking_a.append( i + 1 - i0 )
            i0 = i + 1

    return firings, blocking_a, blocking_vector

def max_error(g):
    if not g.is_consistent():
        raise ValueError("Graph is not consistent")

    apx_opt = single_rate_apx( g, False )
    apx_pess = single_rate_apx( g, True )

    result = 0
    argmax = None
    tokens_lo, tokens_hi, weight = 0, 0, 0
    for a, b in g.edges_iter():
        max_tokens = apx_opt.get_edge_data( a, b ).get('tokens', 0)
        min_tokens = apx_pess.get_edge_data( a, b ).get('tokens', 0)
        diff = max_tokens - min_tokens
        if diff > result:
            result = diff
            argmax = a, b
        tokens_lo = tokens_lo + min_tokens
        tokens_hi = tokens_hi + max_tokens
        weight = weight + apx_pess.node[ b ].get('wcet', 0)

    print("\nMCR Bounds: [ {}, {} ]".format( g.tpi * weight / tokens_hi, g.tpi * weight / tokens_lo ))
    return argmax, result

def par_edge(g, cycle, a, b):
    if not g.is_consistent():
        raise ValueError("Graph is not consistent")

    r, va, vb = retime_vectorise( g, a, b )
    print("Vectorised {} -> vector = {}, {}, retiming = {}".format( (a, b), va, vb, r))
    fire(g, {b: r})
    g = parallelise( g, {a: vb, b: vb} )

    apx_opt = single_rate_apx( g, False )
    apx_pess = single_rate_apx( g, True )

    tokens_lo, tokens_hi, weight = 0, 0, 0
    for a, b in cycle:
        max_tokens = apx_opt.get_edge_data( a, b ).get('tokens', 0)
        min_tokens = apx_pess.get_edge_data( a, b ).get('tokens', 0)
        tokens_lo = tokens_lo + min_tokens
        tokens_hi = tokens_hi + max_tokens
        weight = weight + apx_pess.node[ b ].get('wcet', 0)

    assert g.is_consistent()
    print("\nMCR Bounds: [ {}, {} ]".format( g.tpi * weight / tokens_lo, g.tpi * weight / tokens_hi ))
    return g

def incremental_cycle_analysis(g, cycle):
    if not g.is_consistent():
        raise ValueError("Graph is not consistent")

    # while there are channels with non-linear predecessor functions, retime & vectorise
    while True:
        updated = False
        apx_opt = single_rate_apx( g, False )
        apx_pess = single_rate_apx( g, True )

        for a, b in cycle:
            max_tokens = apx_opt.get_edge_data( a, b ).get('tokens', 0)
            min_tokens = apx_pess.get_edge_data( a, b ).get('tokens', 0)
            data = g.get_edge_data( a, b )
            prates = data.get('production', cyclic(1))
            crates = data.get('consumption', cyclic(1))
            tokens = data.get('tokens', 0)
            if prates.sum() * len(crates) >= crates.sum() * len(prates) and min_tokens < max_tokens:

                r, va, vb = retime_vectorise( g, a, b )
                print("Vectorised {} -> vector = {}, {}, retiming = {}".format( (a, b), va, vb, r))
                fire(g, {b: r})
                g = parallelise( g, {a: vb, b: vb} )

                apx_opt = single_rate_apx( g, False )
                apx_pess = single_rate_apx( g, True )

                updated = True
                break

        tokens_lo, tokens_hi, weight = 0, 0, 0
        for a, b in cycle:
            max_tokens = apx_opt.get_edge_data( a, b ).get('tokens', 0)
            min_tokens = apx_pess.get_edge_data( a, b ).get('tokens', 0)
            tokens_lo = tokens_lo + min_tokens
            tokens_hi = tokens_hi + max_tokens
            weight = weight + apx_pess.node[ b ].get('wcet', 0)

        assert g.is_consistent()
        print("\nMCR Bounds: [ {}, {} ]".format( g.tpi * weight / tokens_lo, g.tpi * weight / tokens_hi ))

        if not updated:
            break

def undo_vectorisation( g, actor, vec ):
    result = core.SDFGraph()
    for v, data in g.nodes_iter( data = True ):
        if v == actor:
            wcet = data.get('wcet')[0]
        else:
            wcet = data.get('wcet')

        result.add_node( v, wcet = wcet )

    for v, w, data in g.edges_iter( data = True ):
        prates = data.get('production', core.cyclic(1))
        crates = data.get('consumption', core.cyclic(1))
        tokens = data.get('tokens', 0)

        if v == actor:
            assert len( vec ) == len( prates )
            assert prates[0] % vec[0] == 0
            rate = prates[ 0 ] // vec[ 0 ]
            for a, b in zip( vec, prates ):
                assert b % a == 0 and b // a == rate
            prates = core.cyclic( rate )

        elif w == actor:
            assert len( vec ) == len( crates )
            assert crates[0] % vec[0] == 0
            rate = crates[ 0 ] // vec[ 0 ]
            for a, b in zip( vec, crates ):
                assert b % a == 0 and b // a == rate
            crates = core.cyclic( rate )

        result.add_edge( v, w,
            production = prates,
            consumption = crates,
            tokens = tokens )

    return result

def as_marked_graph(hsdfg):
    result = nx.DiGraph()
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
        
        for u, _, uv_data in hsdfg.in_edges_iter( v, True ):
            result.add_edge( u, v, weight = wcet, tokens = uv_data.get('tokens', 0) )

    return result

def incremental_graph_analysis( g ):
    if not g.is_consistent():
        raise ValueError("Graph is not consistent")

    blocking_vectors = dict()
    while True:
        assert g.is_consistent()
        # compute single-rate approximations
        print("Constructing single-rate approximations...", end = "")
        apx_opt = single_rate_apx( g, False )
        apx_pess = single_rate_apx( g, True )
        print("done")

        # find critical cycle in pessimistic approximation
        print("Analysing pessimistic approximation...", end = "")
        mg = as_marked_graph( apx_pess )
        try:
            ratio, cycle, _ = mcr.compute_mcr( mg )
        except mcr.InfeasibleException as ex:
            ratio, cycle = None, ex.cycle

        # resolve conflicts in parallelism
        print("done. Critical cycle: {}".format( cycle ))
        for a, b in cycle:
            if b in blocking_vectors:
                v, r, bv = blocking_vectors[ b ]
                if v != a:
                    # undo vectorisation
                    print("Undoing vectorisation of {} by vector {}".format(b, bv))
                    g = undo_vectorisation( g, b, bv )

                    # undo retiming
                    print("Retiming {} by simulating {} firings".format(b, -r))
                    fire( g, {b: -r})

                    # unfold a
                    print("Unfolding {} into {} actors".format(b, sum(bv)))
                    g = unfold( g, {b: sum(bv )})

                    del blocking_vectors[ b ]
                    break
        else:
            # perform incremental cycle analysis, stepwise
            for a, b in cycle:
                data = g.get_edge_data( a, b )
                prates = data.get('production', cyclic(1))
                crates = data.get('consumption', cyclic(1))
                if (prates.sum() * len( crates ) < crates.sum() * len( prates )):
                    # gain is smaller than one -> can't vectorise
                    continue

                max_tokens = apx_opt.get_edge_data( a, b ).get('tokens', 0)
                min_tokens = apx_pess.get_edge_data( a, b ).get('tokens', 0)
                if (max_tokens > min_tokens):
                    print("Channel ({}, {}): {} --> {}, norm.token-diff = {}".format(a, b, prates, crates, g.s[(a, b)] * ( max_tokens - min_tokens )))

            for a, b in cycle:
                data = g.get_edge_data( a, b )
                prates = data.get('production', cyclic(1))
                crates = data.get('consumption', cyclic(1))
                if (prates.sum() * len( crates ) < crates.sum() * len( prates )):
                    # gain is smaller than one -> can't vectorise
                    continue

                max_tokens = apx_opt.get_edge_data( a, b ).get('tokens', 0)
                min_tokens = apx_pess.get_edge_data( a, b ).get('tokens', 0)
                if (max_tokens > min_tokens):
                    print("Channel ({}, {}): production = {}, consumption = {}, token-diff = {}, vectorising consumer".format(a, b, prates, crates, max_tokens - min_tokens ))
                    r, v = retime_vectorise( g, a, b )
                    if r > 0:
                        print("Retiming {} by simulating {} firings".format(b, r))

                    print("Vectorising {} by vector {}...".format(b, v), end = "")
                    blocking_vectors[ b ] = a, r, v
                    g = parallelise( g, {b: v} )
                    print("done")
                    break
            else:
                # approximation is exact
                break

    return g

def lin_bounds_special( pvec, cvec, tokens ):
    prates = core.cyclic( pvec )
    crates = core.cyclic( cvec )

    hi, lo = predecessor_lin_bounds( production = prates, consumption = crates, tokens = tokens )

    avgprate = Fraction( prates.sum(), len(prates))
    avgcrate = Fraction( crates.sum(), len(crates))
    g = gcd( prates.sum(), crates.sum() )

    gstep = g * len(prates) * len(crates)
    istep = prates.sum() * len(crates)
    jstep = crates.sum() * len(prates)

    i = 0
    base = gstep * (tokens // g)
    argmin = (0, 0)
    argmax = (0, 0)
    minval = maxval = 0
    import pdb; pdb.set_trace()
    for j in range(0, len(crates)):
        while tokens % g + prates.sum(0, i) - crates.sum(0, j) < g:
            val = ((tokens + prates.sum(0, i) - crates.sum(0, j)) // g) * gstep - i * istep + j * jstep
            if val < minval:
                minval = val
                argmin = (i, j)
            i = i + 1

    j = 0
    for i in range(0, len(prates)):
        while tokens % g + prates.sum(0, i) - crates.sum(0, j) >= 0:
            val = ((tokens + prates.sum(0, i) - crates.sum(0, j)) // g) * gstep - i * istep + j * jstep
            if val > maxval:
                maxval = val
                argmax = (i, j)
            j = j + 1

    minval = Fraction(base + minval, len(prates) * len(crates))
    maxval = Fraction(base + maxval, len(prates) * len(crates))
    assert minval + g - avgprate == lo, "{} != {}".format(minval + g - avgprate, lo)
    assert maxval == hi, "{} != {}".format(maxval, hi)

    return argmin, argmax, lo, hi

def test_lin_bounds_special( s = 200, a = 2, b = 40, dbg = None ):
    from syncdataflow.randomsdf import random_vector
    from random import randint, seed
    import pdb
    for k in [dbg] if dbg else range(1, 10000):
        seed(k)
        prates = core.cyclic( random_vector( s, randint(a, b)))
        crates = core.cyclic( random_vector( s, randint(a, b)))
        tokens = randint( 0, 100 )

        hi, lo = predecessor_lin_bounds( production = prates, consumption = crates, tokens = tokens )

        avgprate = Fraction( prates.sum(), len(prates))
        avgcrate = Fraction( crates.sum(), len(crates))
        g = gcd( prates.sum(), crates.sum() )
        if dbg: pdb.set_trace()

        i = 0
        minval = maxval = g * (tokens // g)
        for j in range(0, len(crates)):
            while tokens % g + prates.sum(0, i) - crates.sum(0, j) < g:
                val = ((tokens + prates.sum(0, i) - crates.sum(0, j)) // g) * g - i * avgprate + j * avgcrate
                minval = min( minval, val )
                i = i + 1

        j = 0
        for i in range(0, len(prates)):
            while tokens % g + prates.sum(0, i) - crates.sum(0, j) >= 0:
                val = ((tokens + prates.sum(0, i) - crates.sum(0, j)) // g) * g - i * avgprate + j * avgcrate
                maxval = max( maxval, val )
                j = j + 1

        assert minval + g - avgprate == lo, "{} != {}. seed = {}".format(minval + g - avgprate, lo, k)
        assert maxval == hi, "{} != {}. seed = {}".format(maxval, hi, k)

    print("PASS")

def normalise_channels( g ):
    result = core.SDFGraph()
    for v, data in g.nodes_iter( data = True ):
        result.add_node( v, **data )

    for u, v, data in g.edges_iter( data = True ):
        prates = data.get('production', cyclic(1))
        crates = data.get('consumption', cyclic(1))
        tokens = data.get('tokens', 0)

        if len(prates) == len(crates) == 1:
            g = gcd( prates[0], crates[0] )
            norm_data = dict( production = prates[0] // g, consumption = crates[0] // g, tokens = tokens // g )
            result.add_edge( u, v, **norm_data )
        else:
            result.add_edge( u, v, **data )

    return result

