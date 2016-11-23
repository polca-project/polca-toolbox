import networkx as nx
from shortestpaths.core import *

name = "GOR"

def find_shortest_paths( graph, root):
    stack_a = list()
    stack_b = [root]
    distances = {root: 0}
    visited = set()
    post = set()
    labeled = set()
    scans = 0
    try:
        while stack_b:
            visited.clear()
            labeled.clear()
            post.clear()
            while stack_b:
                v = stack_b[-1]
                assert v in distances
                if v not in visited:
                    # scan v
                    if v in distances:
                        scans += 1
                        visited.add(v)
                        dv = distances[ v ]
                        for v, w, data in graph.out_edges_iter( v, data = True ):
                            cost = data.get('weight', 1)
                            dw = distances.get( w, dv + cost + 1 )
                            delta = dw - (dv + cost)
                            if delta > 0:
                                if w not in visited:
                                    # print("Relaxing edge ({}, {})".format( v, w ))
                                    distances[ w ] = dw - delta
                                    stack_b.append( w )
                                    labeled.add( w )
                                elif w not in post:
                                    raise NegativeCycleException( scans )
                else:
                    # post visit, put on stack_a if it was labeled
                    stack_b.pop()
                    post.add( v )
                    
                    stack_a.append( v )

            # scan visited nodes in topological order
            scanned = set()
            labeled.clear()
            while stack_a:
                v = stack_a.pop()
                # scan v
                scans += 1
                dv = distances[ v ]
                for v, w, data in graph.out_edges_iter( v, data = True ):
                    cost = data.get('weight', 1)
                    dw = distances.get( w, dv + cost + 1 )
                    delta = dw - (dv + cost)
                    if delta > 0:
                        distances[ w ] = dw - delta
                        # if w not in labeled:
                        #     stack_b.append( w )
                        #     labeled.add( w )
                        # pass

                        if w not in scanned:
                            distances[ w ] = dw - delta
                        else:
                            stack_b.append( w )
                            # labeled.add( w )
                scanned.add(v)

        return distances, scans / graph.number_of_nodes()
    except NegativeCycleException as ex:
        return {}, scans / graph.number_of_nodes()

