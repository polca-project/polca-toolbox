import networkx as nx
from random import seed, randrange, shuffle
from networkx import fast_gnp_random_graph
import math
from fractions import Fraction
from shortestpaths.core import *

def rand5(n = 2 ** 18, fam = 1, seed = None):
    if seed is not None:
        random.seed(seed)
    g = sprand(n, 5 * n)
    family(g, fam)
    return g

def sqnc(layers = 512, fam = 1, seed = None):
    if seed is not None:
        random.seed(seed)

    g = torus(layers, layers)
    family(g, fam)
    assert g.number_of_nodes() == layers * layers + 1
    return g

def lnc(layers = 16384, fam = 1, seed = None):
    if seed is not None:
        random.seed(seed)

    g = torus(16, layers)
    family(g, fam)
    assert g.number_of_nodes() == layers * 16 + 1
    return g

def pnc(layers = 8192, fam = 1, seed = None):
    if seed is not None:
        random.seed(seed)

    g = torus(32, layers)
    # add 64 random arcs per layer
    for x in range(layers):
        for k in range(64):
            src = randrange( 32 )
            snk = src
            while src == snk:
                snk = randrange( 32 )
            g.add_edge( (x, src), (x, snk), weight = randrange(1, 101) )

        # for each vertex, add 4 arcs to forward layers
        if x < layers - 1:
            for y in range( 32 ):
                for k in range(4):
                    l = randrange( x + 1, layers)
                    dist = l - x
                    g.add_edge( (x, y), (l, randrange(0, 32) ), weight = randrange(1, 10001) * dist * dist )
    family(g, fam)
    return g

def torus(height, width, seed = None):
    if seed is not None:
        random.seed(seed)

    g = nx.DiGraph()

    # create arcs from source
    for y in range(height):
        # inter-layer arc
        g.add_edge( 0, (0, y), weight = randrange(1000, 10001))
    
    # create width layers
    for x in range(width):
        # create layer with height vertices
        for y in range(height):
            # intra-layer arc
            g.add_edge((x, y), (x, (y + 1) % height), weight = randrange(1, 101))

            # inter-layer arc
            g.add_edge((x, y), ( (x + 1) % width, y), weight = randrange(1000, 10001))

    assert g.number_of_nodes() == height * width + 1
    return g
        
def sprand(n, m, l = 1, u = 1000, seed = None):
    assert m > n
    if seed is not None:
        random.seed(seed)

    g = nx.DiGraph()
    vertices = list(range(n))
    shuffle(vertices)
    
    for i in range(n):
        g.add_edge( vertices[i], vertices[ (i + 1) % n ], weight = randrange(l, u + 1) )
    
    for i in range(m - n):
        # pick random source vertex
        # pick random target vertex
        v = vertices[ randrange(n) ]
        w = vertices[ randrange(n) ]
        while v == w:
            w = vertices[ randrange(n) ]
        g.add_edge( v, w, weight = randrange(l, u + 1) )
    return g

def family(g, fam = 1):
    n = g.number_of_nodes()
    cycles, length = {
        1: (0, 0),
        2: (1, 3),
        3: (math.floor(math.sqrt(n)), 3),
        4: (math.floor(n ** Fraction(1, 3)), math.floor(math.sqrt(n))),
        5: (1, n)}[fam]
    negcycle(g, cycles, length)

def negcycle(g, cycles = 0, length = 3, potential = 16384):
    vertices = list(g.nodes())
    shuffle(vertices)
    for _ in range(cycles):
        cycle = list()
        for _ in range(length):
            cycle.append(vertices.pop())
        for i in range(1, len(cycle)):
            g.add_edge(cycle[i - 1], cycle[i], weight = 0)
        g.add_edge(cycle[-1], cycle[0], weight = -1)
    for v in g:
        pot = randrange(potential)
        for _, _, data in g.in_edges_iter( v, True ):
            data['weight'] += pot
        for _, _, data in g.out_edges_iter( v, True ):
            data['weight'] -= pot

def verify_distances( graph, distances ):
    for u, v, data in graph.edges( data = True ):
        du = distances.get(u, None)
        if du is not None:
            if v not in distances:
                return False
            dv = distances.get(v)
            cost = data['weight']
            if du + cost < dv:
                return False
    else:
        return True

def test_rand5(algorithm, n = 50, runs = 1500, singlerun = False):
    vssum = 0
    maxit, argmax = 0, None
    for run in range(runs) if not singlerun else range(runs, runs + 1):
        random.seed(run + 1)
        g = sprand(n, m * n)
        negcycle(g, 0, 0)

        if singlerun:
            import pdb; pdb.set_trace()

        try:
            distances, scans = algorithm(g, 0)

            vssum += scans

            assert verify_distances(g, distances)

            print("Run {}: scans per vertex: {}".format(run, scans))
        except NegativeCycleException:
            print("Negative cycle detected")

    print("Avg. scans per vertex: {}".format(vssum / runs))

def test_random(algorithm, n = 50, p = 0.2, runs = 1500, singlerun = False):
    vssum = 0
    maxit, argmax = 0, None
    for run in range(runs) if not singlerun else range(runs, runs + 1):
        g = fast_gnp_random_graph(n, p, seed = run + 1, directed = True)
        for u, v, data in g.edges( data = True ):
            data['weight'] = 1 + randrange(10)

        root = next(g.nodes_iter())
        if singlerun:
            import pdb; pdb.set_trace()
        distances, scans = algorithm(g, root)

        vssum += scans

        if not verify_distances(g, distances):
            raise Exception("incorrect distances")

        print("Run {}: scans per vertex: {}".format(run, scans))
    print("Avg. scans per vertex: {}".format(vssum / runs))

