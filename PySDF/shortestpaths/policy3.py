
name = "PI"

def scan( graph, v, tree, scanned, labeled, distances, pre = None, post = None ):
    if v in labeled:
        labeled.remove( v )

    dv = distances.get(v)
    for v, w, data in graph.out_edges_iter( v, True ):
        weight = data.get('weight', 1)
        dw = distances.get(w, dv + weight + 1)
        delta = dw - dv - weight
        if delta > 0:
            if pre is not None and post is not None and w in pre:
                if pre[v] < pre[w] and post.get(w, 0) < pre[w]:
                    raise NegativeCycleException()

            if w in scanned:
                # we must scan v again
                labeled.add(v)
                #print("*** Did NOT relax ({}, {})".format(v, w))
                continue

            # the current parent of w can't lie on the shortest path from
            # the root to w
            distances[w] = dw - delta

            # print("Relaxed ({}, {}), distances[{}] = {}".format(v, w, w, distances[w]))
            tree.append_child( v, w )

            #print("*** Relaxed ({}, {})".format(v, w))
            labeled.add(w)

    scanned.add(v)

def update_policy( graph, tree, labeled, distances ):
    """ Visit nodes in the tree in pre-order.
    If a node has a changed distance, scan it
    
    """
    global testing

    scanned = set()
    scans = 0
    pre, post = {}, {}
    tick = 0
    dfs = tree.dfs()
    for v, i in dfs:
        tick += 1
        if i == 0:
            # pre-visit v
            pre[v] = tick
            if v in labeled:
                scan( graph, v, tree, scanned, labeled, distances, pre, post )
                scans += 1
            scanned.add(v)
        else:
            # post-visit v
            post[v] = tick

    prev_tree = copy(tree)

    post_order = list()
    pre_order = list()
    for v, i in tree.dfs():
        if i == 0:
            pre_order.append(v)
        else:
            post_order.append(v)

    # Assert that all admissible edges are decreasing cross edges
    if testing:
        pre, post = tree.pre_post_indices()
        for u, v, data in graph.edges( data = True ):
            if u in distances:
                if distances[u] + data.get('weight', 1) < distances[v]:
                    assert post[v] < pre[u], "u [{}, {}] -> v [{}, {}]".format(pre[u], post[u], pre[v], post[v])

    if labeled:
        scanned.clear()
        # visit in reverse post order
        for i in range(len(post_order)):
            v = post_order[-(i + 1)]
            if v in labeled:
                scan( graph, v, tree, scanned, labeled, distances )
                scans += 1
            scanned.add( v )

        if testing:
            # Assert that all admissible edges are cross edges
            pre, post = tree.pre_post_indices()
            for u, v, data in graph.edges( data = True ):
                if u in distances:
                    if distances[u] + data.get('weight', 1) < distances[v]:
                        assert post[v] < pre[u] or post[u] < pre[v], "u [{}, {}] -> v [{}, {}]".format(pre[u], post[u], pre[v], post[v])

        if False and labeled:
            scanned.clear()
            # visit in post order
            assert tree.is_valid()
            assert prev_tree.is_valid()
            for v in post_order:
                if v in labeled and v not in scanned:
                    k = (v, 0)
                    while k != (v, 1):
                        if k[1] == 0:
                            w = k[0]
                            assert w not in scanned
                            if w in labeled:
                                scan( graph, w, tree, scanned, labeled, distances )
                                scans += 1
                        k = prev_tree.after( k )
                    prev_tree.delete( v )

            # Assert that all admissible edges are increasing cross edges or forward edges
            if testing:
                pre, post = tree.pre_post_indices()
                for u, v, data in graph.edges( data = True ):
                    if u in distances:
                        if distances[u] + data.get('weight', 1) < distances[v]:
                            assert post[u] < pre[v] or post[v] < post[u], "u [{}, {}] -> v [{}, {}]".format(pre[u], post[u], pre[v], post[v])

    return tree, scans

def find_shortest_paths( graph, root ):
    tree = DFSTree( root )

    labeled = set()
    labeled.add(root)

    total_scans = 0
    distances = {root: 0}

    try:
        while labeled:
            tree, vertex_scans = update_policy( graph, tree, labeled, distances )
            total_scans += vertex_scans

        return distances, total_scans / graph.number_of_nodes()
    except NegativeCycleException as ex:
        total_scans += ex.info
        print("Negative cycle found. Scans per vertex: {}".format(total_scans / graph.number_of_nodes()))
        return {}, total_scans / graph.number_of_nodes()

