class Forest( object ):

    def __init__(self):
        super().__init__()
        self.__next = {}
        self.__prev = {}
        self.__head = None
        self.__tail = None
        self.__parent = dict()
        self.__roots = set()

    def add_root( self, root ):
        if root in self.__parent:
            raise ValueError("Node {} is already in tree".format( root ))

        if self.__head is None:
            self.__head = ( root, 0 )

        self.__tail = ( root, 1 )

        self.__prev[ (root, 0) ] = None
        self.__next[ (root, 0) ] = (root, 1)
        self.__prev[ (root, 1) ] = (root, 0)
        self.__next[ (root, 1) ] = None

        self.__parent[ root ] = None

        self.__roots.add( root )

    def add_forest( self, other ):
        for root in other.roots():
            if root in self.__roots:
                raise ValueError("Node {} is already in tree".format( root ))
            self.add_root( root )
            for v in other.pre_order( root ):
                for child in other.children( v ):
                    if child in self.__parent:
                        raise ValueError("Node {} is already in tree".format( child ))

                    self.append_child( v, child )
            
    def roots( self ):
        for i in self.__roots:
            yield i

    def is_valid(self):
        assert None not in self.__next, "None in __next"
        assert None not in self.__prev, "None in __prev"
        assert None not in self.__parent, "None in __parent"

        for key in self.__next:
            assert key in self.__prev
            after_key = self.__next[key]
            assert after_key is None or self.__prev[after_key] == key, "inconsistency in linked list"

        for key in self.__prev:
            assert key in self.__next
            before_key = self.__prev[key]
            assert before_key is None or self.__next[before_key] == key, "inconsistency in linked list"

        visited = set()
        v = self.__head
        for v in self.__next:
            if v[1] == 0 and self.__parent[ v[0] ] is None:
                w = v
                while w is not None:
                    if w in visited:
                        import pdb; pdb.set_trace()

                    assert w not in visited, "cycle"
                    visited.add(w)
                    w = self.__next[w]

        for v in self.__next:
            assert v in visited, "not a tree"

        return True

    def parent(self, child):
        return self.__parent[child]

    def __copy__(self):
        result = DFSTree( self.__head[0] )
        result.__head = self.__head
        result.__tail = self.__tail
        result.__parent = self.__parent.copy()
        result.__next = self.__next.copy()
        result.__prev = self.__prev.copy()
        return result

    def __contains__(self, item):
        assert ((item, 0) in self.__next) == ((item, 1) in self.__next)
        return (item, 0) in self.__next

    def append_child(self, parent, child):
        if self.__parent.get( child, None ) == parent:
            return False

        if child in self.__roots:
            self.__roots.remove( child )

        pre_child = (child, 0)
        post_child = (child, 1)

        # disconnect the subtree
        if pre_child not in self.__next:
            # new node
            assert pre_child not in self.__prev
            assert post_child not in self.__next
            assert post_child not in self.__prev
            self.__next[ pre_child ] = post_child
            self.__prev[ post_child ] = pre_child

        before_subtree = self.__prev.get( pre_child, None )
        after_subtree = self.__next.get( post_child, None )
        if before_subtree is not None:
            assert after_subtree is not None
            self.__next[ before_subtree ] = after_subtree
            self.__prev[ after_subtree ] = before_subtree
        
        # child is pre-visited after last child of parent is post-visited,
        # so look up post-visits of parent and its last child
        post_parent = (parent, 1)
        post_lastchild = self.__prev[ post_parent ]

        # insert subtree as last child of parent
        self.__prev[ pre_child ] = post_lastchild
        self.__next[ post_lastchild ] = pre_child

        self.__prev[ post_parent ] = post_child
        self.__next[ post_child ] = post_parent

        self.__parent[ child ] = parent
        # assert self.is_valid()
        return True

    def prepend_child(self, parent, child):
        if self.__parent.get( child, None ) == parent:
            return False

        if child in self.__roots:
            self.__roots.remove( child )

        pre_child = (child, 0)
        post_child = (child, 1)

        # disconnect the subtree
        if pre_child not in self.__next:
            # new node
            assert pre_child not in self.__prev
            assert post_child not in self.__next
            assert post_child not in self.__prev
            self.__next[ pre_child ] = post_child
            self.__prev[ post_child ] = pre_child

        before_subtree = self.__prev.get( pre_child, None )
        after_subtree = self.__next.get( post_child, None )
        if before_subtree is not None:
            assert after_subtree is not None
            self.__next[ before_subtree ] = after_subtree
            self.__prev[ after_subtree ] = before_subtree
        
        # child is pre-visited after parent is pre-visited,
        # and before parent's successor visited
        # so look up pre-visits of parent and its last child
        pre_parent = (parent, 0)
        after_parent = self.__next[ pre_parent ]

        # insert subtree as last child of parent
        self.__prev[ after_parent ] = post_child
        self.__next[ post_child ] = after_parent

        self.__prev[ pre_child ] = pre_parent
        self.__next[ pre_parent ] = pre_child

        self.__parent[ child ] = parent
        assert self.is_valid()
        return True

    def delete_subtree( self, v ):
        self.cut_subtree( v )
        assert self.__tail != ( v, 1 ), "Current implementation assumes __tail != ({}, 1)".format( v )

        tuples = set()
        for tup in self.dfs( v ):
            tuples.add( tup )

        for tup in tuples:
            del self.__next[ tup ]
            del self.__prev[ tup ]
            del self.__parent[ tup ]

        del self.__roots[ v ]

    def cut_subtree( self, v ):
        before_pre = self.__prev[ (v, 0) ]
        after_post = self.__next[ (v, 1) ]
        self.__next[ before_pre ] = after_post
        self.__prev[ after_post ] = before_pre
        self.__prev[ (v, 0) ] = None
        self.__next[ (v, 1) ] = None
        self.__parent[ v ] = None

        self.__roots.add( v )
        # assert self.is_valid()

    if False:
        def disassemble_subtree(self, v):
            n = self.__next[ (v, 0) ]
            while n != (v, 1):
                if n[1] == 0:
                    yield n[0]
                    n = self.__next[ n ]
                else:
                    # post visit, delete
                    waste = n[0]
                    n = self.__next[ n ]
                    self.delete( waste )

    def before(self, v):
        return self.__prev[ v ]

    def after(self, v):
        return self.__next[ v ]
        
    def ancestors(self, v):
        v = self.__prev[ (v, 0) ]
        n = 0
        while v in self.__prev:
            if v[1] == 0 and n == 0:
                yield v[0]
            elif v[1] == 1:
                # skip subtree rooted at sibling v[0]
                v = (v[0], 0)

            v = self.__prev[ v ]

    def children(self, v):
        child = self.__next[ (v, 0) ]
        while child != (v, 1):
            if child[1] == 0:
                yield child[0]
                child = ( child[0], 1 )
            child = self.__next[ child ]

    def dfs(self, root = None ):
        if root is None:
            root = self.__head[ 0 ]

        n = (root, 0)
        while n != (root, 1):
            yield n
            n = self.__next[n]
        yield (root, 1)

    def pre_order(self, root = None):
        if root is None:
            root = self.__head[ 0 ]

        n = (root, 0)
        while n != (root, 1):
            if n[1] == 0:
                yield n[0]
            n = self.__next[n]

    def post_order(self):
        n = self.__head
        while n is not None:
            if n[1] == 1:
                yield n[0]

            n = self.__next[n]

    def reverse_post_order(self):
        n = self.__tail
        while n is not None:
            if n[1] == 1:
                yield n[0]

            n = self.__prev[n]

    def pre_post_indices(self):
        pre, post = {}, {}
        n = self.__head
        idx = 1
        while n is not None:
            if n[1] == 0:
                pre[n[0]] = idx
            else:
                post[n[0]] = idx

            n = self.__next[n]
            idx += 1
        return pre, post

