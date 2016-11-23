import heapq
import networkx as nx

def mplus(a, b):
    if a is None:
        return b
    if b is None:
        return a
    return max(a, b)

def mtimes(a, b):
    return None if a is None or b is None else a + b

def vpp( vector ):
    cols = set()
    for c in vector:
        cols.add(c)
    
    for c in sorted( cols ):
        val = vector.get( c, None )
        print("{:>5}: {:^7}".format( c, val or '-'), end = '')
    print()

def mpp( matrix ):
    """ Pretty prints the given matrix
    """
    rows = set()
    for r, _ in matrix:
        rows.add(r)

    cols = set()
    for _, c in matrix:
        cols.add(c)

    print(" ".ljust(8), end='')
    for c in sorted( cols ):
        print("{:^7}".format( c ), end = '')
    print()

    for r in sorted( rows ):
        print("{:>6s} [".format(r), end ='')
            
        for c in sorted( cols ):
            val = matrix.get( (r, c), None )
            print("{:^7}".format( val or '-'), end = '')
        print(']')

def names( matrix_or_vector ):
    result = set()
    for v in matrix_or_vector:
        try:
            for u in iter(v):
                result.add( u )
        except TypeError:
            result.add( v )

    return result

def unit_vector( names ):
    """ Creates a vector filled with zeroes (e.g. 0), in the form of a dictionary,
    using the elements of names as keys for the dictionary
    """
    result = dict()
    for name in names:
        result[ name ] = 0

    return result

def unit_matrix( names ):
    """ Creates a vector filled with zeroes (e.g. 0), in the form of a dictionary,
    using the elements of names as keys for the dictionary
    """
    result = dict()
    for name in names:
        result[ (name, name) ] = 0

    return result

def matrix_multiply( a, b ):
    row_keys, col_keys_a, col_keys_b = set(), set(), set()
    for r, k in a:
        row_keys.add( r )
        col_keys_a.add( k )

    for _, c in b:
        col_keys_b.add( c )

    result = dict()
    for r in row_keys:
        for c in col_keys_b:
            # compute max-plus sum of row r of a, and column c of b
            mp_sum = None
            for k in col_keys_a:
                val = mtimes( a.get( (r, k), None ), b.get( (k, c), None ))
                mp_sum = mplus( mp_sum, val )
            if mp_sum is not None:
                result[ (r, c) ] = mp_sum

    return result

def mat_vec_multiply( mat, vec ):
    row_keys, col_keys_mat, col_keys_vec = set(), set(), set()
    for r, k in mat:
        row_keys.add( r )
        col_keys_mat.add( k )

    for c in vec:
        col_keys_vec.add( c )

    result = dict()
    for r in row_keys:
        # compute max-plus sum of row r of a, and column c of b
        mp_sum = None
        for c in col_keys_vec:
            val = mtimes( mat.get( (r, c), None ), vec.get( c, None ))
            mp_sum = mplus( mp_sum, val )

        if mp_sum is not None:
            result[ r ] = mp_sum

    return result

def matrix_power( a, n ):
    result = a
    for _ in range(n - 1):
        result = matrix_multiply( result, a )

    return result

def marked_graph_to_digraph( g, param_value ):
    result = nx.DiGraph()
    for u, v, data in g.edges( data = True ):
        result.add_edge( u, v, weight = data.get('weight', 0) - data.get('tokens', 0) * param_value )

    return result

def hsdfg_to_marked_graph( g ):
    result = nx.DiGraph()
    for u, v, data in g.edges( data = True ):
        try:
            w = g.node[u].get('wcet', 0)[0]
        except TypeError:
            w = g.node[u].get('wcet', 0)

        toks = data.get('tokens', 0)
        if toks == 0:
            result.add_edge( u, v, weight = w )
        else:
            result.add_edge( u, v, weight = w, tokens = toks )

    return result

def system_to_marked_graph( system ):
    """ Transforms a system into a marked graph
    """
    g = nx.DiGraph()
    for m in system:
        matrix = system[ m ]
        for v, u in matrix:
            val = matrix[ (v, u) ]
            if val is not None:
                if m == 0:
                    g.add_edge( u, v, weight = val )
                else:
                    g.add_edge( u, v, weight = val, tokens = m )
    return g

def first_order_system( ho_system ):
    """ Transforms a higher order max-plus system into a first-order system

    ho_system:      the higher-order system, as a dictionary

    returns:        a first-order system, as {1: matrix}
    """
    all_names = set()
    for k in ho_system:
        all_names.update( names( ho_system[ k ]))

    # introduce auxiliary nodes
    order = max( ho_system.keys() )
    a0 = kleene_star( ho_system[ 0 ]) if 0 in ho_system else unit_matrix( all_names )

    aux_count = 0
    m1 = ho_system.get( 1, dict() ).copy()
    for m, matrix in [(m, ho_system[ m ]) for m in ho_system if m not in (0, 1)]:
        for v, u in matrix:
            # create (m - 1) additional actors
            prev = u
            for aux in range( m - 1 ):
                aux_count += 1
                aux_name = 'aux{}'.format(aux_count)
                m1[ (aux_name, prev) ] = 0
                a0[ (aux_name, aux_name) ] = 0
                prev = aux_name

            m1[ (v, prev) ] = matrix[ (v, u) ]
            
    return {1: matrix_multiply(a0, m1)}

def higher_order_system( g, key_weight = 'weight', key_tokens = 'tokens' ):
    """ Constructs a higher order max-plus system from a directed graph
    Uses key_weight and key_tokens to retrieve weight and tokens of edges
    """
    matrices = dict()

    for u, v, data in g.edges_iter( data = True ):
        tokens = int(data.get( key_tokens, 0 ))

        if tokens < 0:
            raise ValueError('Negative tokens not allowed')

        if tokens not in matrices:
            matrices[ tokens ] = dict()

        deps = matrices[ tokens ]
        if (v, u) in deps:
            deps[ (v, u) ] = mplus( deps[ (v, u) ], data[ key_weight ])
        else:
            deps[ (v, u) ] = data[ key_weight ]

    return matrices

def kleene_star( matrix ):
    """ Computes the Kleene star of the given matrix
    For this, the graph representation of the matrix must be acyclic
    """
    g = nx.DiGraph()
    distances = dict()
    for v, u in matrix:
        if matrix[ (v, u) ] is not None:
            g.add_edge( u, v )

    try:
        top_order = nx.topological_sort(g)
        for v in top_order:
            if v not in distances:
                distances[ v ] = 0

            dv = distances[ v ]

            for u, _ in g.in_edges_iter( v ):
                distances[ v ] = mplus(
                    dv,
                    mtimes( distances[ u ], matrix[ (v, u) ]))

        result = dict()
        for v in g:
            result[ (v, v) ] = 0
            for u in nx.ancestors( g, v ):
                result[ (v, u) ] = distances[ v ] - distances[ u ]

        return result
    except nx.NetworkXUnfeasible:
        raise ValueError("Kleene star does not exist - communication graph of matrix is cyclic")
    
def simulate( matrices, sequence = list(), k = 1):
    """ Computes the next state of the system, given a sequence of previous states.

    matrices is assumed to a dictionary, mapping integers to matrices, a matrix
    is a dictionary that maps tuples to numbers
    """
    if 0 in matrices:
        m0 = matrices[ 0 ]
        g = nx.DiGraph()
        for v, u in m0:
            g.add_edge( u, v )

        top_order = nx.topological_sort(g)

    for _ in range(k):
        result = dict()
        for m in matrices:
            if m != 0:
                matrix = matrices[ m ]
                try:
                    vector = sequence[ -m ]
                except IndexError:
                    vector = dict()

                for v, u in matrix:
                    result[ v ] = mplus(
                        result.get(v, None),
                        mtimes( matrix.get( (v, u)), vector.get( u, None )))

        if 0 in matrices:
            m0 = matrices[ 0 ]
            for v in top_order:
                for u, _ in g.in_edges_iter( v ):
                    result[ v ] = mplus(
                        result.get(v, None),
                        mtimes( result[ u ], m0[ (v, u) ]))

        sequence = sequence + [result]

    return sequence

def is_enabled(g, v, marking):
    for u, _, idx in g.in_edges_iter( v, keys = True ):
        if marking[ (u, v, idx) ] == 0:
            return False
    return True

def consume( g, v, marking ):
    for u, _, idx in g.in_edges_iter( v, keys = True ):
        assert marking[ (u, v, idx) ] > 0
        marking[ (u, v, idx) ] -= 1

def produce( g, v, marking ):
    for _, w, idx in g.out_edges_iter( v, keys = True ):
        marking[ (v, w, idx) ] += 1

def io( g, inputseq ):
    outputseq = list()
    sources = [ v for v in g if g.in_degree(v) == 0 ]
    sinks = [ v for v in g if g.out_degree(v) == 0 ]
    state = { v for v in g if g.degree(v) > 1 }

    assert sinks and sources
    assert len( sources ) == 1, "too many sources: {}".format( sources )
    assert len( sinks ) == 1

    source = sources[ 0 ]
    sink = sinks[ 0 ]
    marking = dict()

    for u, v, idx, data in g.edges_iter( data = True, keys = True ):
        marking[ (u, v, idx) ] = data.get('tokens', 0)

    # 1 fire all non-source nodes as many times as possible
    stateseq = dict()
    for v in state:
        stateseq[v] = list()

    changed = True
    while changed:
        changed = False
        for v in state:
            # check if v is enabled
            while is_enabled( g, v, marking ):
                changed = True
                consume( g, v, marking )
                produce( g, v, marking )
                stateseq[ v ].append( None )
        
        # fire sink
        while is_enabled( g, sink, marking ):
            outputseq.append( None )
            changed = True
            consume( g, sink, marking )

    # maintain priority queue
    heap = []
    for _, w, idx, data in g.out_edges_iter( source, data = True, keys = True ):
        for t in inputseq:
            heapq.heappush( heap, ( t + data['weight'], source, w, idx ))

    # 2 fire 
    while heap:
        t, v, w, idx = heapq.heappop( heap )
        marking[ (v, w, idx) ] += 1
        if is_enabled( g, w, marking ):
            if w == sink:
                outputseq.append( t )
            else:
                stateseq[ w ].append( t )

            consume( g, w, marking )
            for _, z, idx, data in g.out_edges_iter( w, keys = True, data = True ):
                heapq.heappush( heap, ( t + data['weight'], w, z, idx ))
        
    return outputseq, stateseq

example = nx.MultiDiGraph()
example.add_edge( 'u', 'b', weight = 1, tokens = 1 )
example.add_edge( 'b', 'c', weight = 6, tokens = 1 )
example.add_edge( 'c', 'a', weight = 2, tokens = 2 )
example.add_edge( 'a', 'c', weight = 2, tokens = 1 )
example.add_edge( 'a', 'a', weight = 1, tokens = 1 )
example.add_edge( 'a', 'b', weight = 7, tokens = 1 )
example.add_edge( 'a', 'b', weight = 2 )
example.add_edge( 'c', 'y', weight = 0 )

