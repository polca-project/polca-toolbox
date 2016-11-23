from shortestpaths.dfstree import DFSTree
from shortestpaths.core import *

name = "DFS"

def find_shortest_paths( graph, root ):
    current_pass = list()
    current_pass.append(root)
    next_pass = list()
    scanned = set()
    distances = { root: 0 }

    scans = 0

    tree = DFSTree(root)

    try:
        while current_pass:
            scanned.clear()
            while current_pass:
                v = current_pass.pop()
                if v not in scanned:
                    # scan v
                    scans += 1
                    dv = distances.get(v)
                    for v, w, data in graph.out_edges_iter( v, True ):
                        weight = data.get('weight', 1)
                        dw = distances.get(w, dv + weight + 1)
                        delta = dw - dv - weight
                        if delta > 0:
                            distances[w] = dw - delta
                            # print("Disassembling subtree while relaxing ({}, {})".format(v, w))
                            tree.append_child( v, w )
                            y = tree.after( (w, 0) )
                            while y != (w, 1):
                                if y[1] == 0:
                                    # pre visit
                                    if y[0] == v:
                                        raise NegativeCycleException(scans)
                                    distances[y[0]] -= (delta - 1)
                                    scanned.add(y[0])
                                    y = tree.after(y)
                                else:
                                    # post visit: delete
                                    z = y[0]
                                    y = tree.after(y)
                                    tree.delete( z )

                            if w in scanned:
                                # we must scan v again
                                next_pass.append( w )
                            else:
                                current_pass.append( w )

                    scanned.add(v)
            current_pass = next_pass
            next_pass = list()

        return distances, scans / graph.number_of_nodes()
    except NegativeCycleException as ex:
        return {}, scans / graph.number_of_nodes()

