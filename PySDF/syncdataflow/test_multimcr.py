import networkx as nx
import syncdataflow.multimcr as mcr

g1 = nx.DiGraph()
g1.add_edge(1, 2, weight = 5, coef = 2, var = 'd2')
g1.add_edge(2, 3, weight = 4, coef = 3, var = 'd1')
g1.add_edge(3, 1, weight = 3, coef = 1, var = 'd4')
g1.add_edge(2, 4, weight = 7, coef = 3, var = 'd3')
g1.add_edge(4, 1, weight = 6)
g1.add_edge(1, 1, weight = 0, coef = 1, var = 'd1')
g1.add_edge(2, 2, weight = 0, coef = 1, var = 'd2')
g1.add_edge(3, 3, weight = 0, coef = 1, var = 'd3')
g1.add_edge(4, 4, weight = 0, coef = 1, var = 'd4')

sol = mcr.compute_minimum( g1 )
assert sum(sol.values()) == 8, "Non-optimal solution for graph {}\nData: {}".format(g1.edges(), g1.edge)

g1.add_edge(1, 1, weight = 1, coef = 1, var = 'd1')
g1.add_edge( 4, 4, weight = 1, coef = 1, var = 'd4' )
g1.add_edge( 4, 5, weight = 2, coef = 1, var = 'd1' )
g1.add_edge( 5, 4, weight = 3, coef = 1, var = 'd3' )

sol = mcr.compute_minimum( g1 )
assert sum(sol.values()) == 9.5, "Non-optimal solution for graph {}\nData: {}".format(g1.edges(), g1.edge)

