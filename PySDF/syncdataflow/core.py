from math import ceil
from functools import reduce
import networkx as nx
import json
import sys
import re
import pdb
from fractions import Fraction, gcd
from syncdataflow.integers import lcm
from syncdataflow.graphs import dfs_edges
import xml.etree.ElementTree as etree

class SDFGraph( nx.DiGraph ):
    def __init__(self, data = None, **attr):
        self.phases = {}
        self.consistent_subgraph = None
        super().__init__(data, **attr)

    def is_consistent( self ):
        if self.consistent_subgraph is None:
            self.q, self.s, self.tpi, self.consistent_subgraph = check_consistency( self )

        assert self.consistent_subgraph.number_of_edges() == self.number_of_edges()
        return self.consistent_subgraph.number_of_edges() == self.number_of_edges()

    def to_JSON(self):
        actors = list()
        for v, data in self.nodes_iter( data = True ):
            wcet = data.get('wcet', 0)
            actors.append( dict(name = str(v), wcet = wcet if len(wcet) > 1 else wcet[0] ))

        print(actors)
        channels = list()
        for v, w, data in self.edges_iter( data = True ):
            d = dict(to = str(w))
            d['from'] = str(v)
            if 'production' in data:
                prates = data.get('production')
                d['production'] = prates if len(prates) > 1 else prates[0]

            if 'consumption' in data:
                prates = data.get('consumption')
                d['consumption'] = prates if len(prates) > 1 else prates[0]

            if 'tokens' in data:
                d['tokens'] = data.get('tokens')

            channels.append(d)

        root = dict(type='csdf', actors=actors, channels=channels)
        return json.dumps(root, sort_keys = True, indent = 2)

    def add_node(self, n, attr_dict = None, **attr):
        if attr_dict is not None:
            attr_dict.update
        else:
            attr_dict = attr

        wcet = attr_dict['wcet'] = SDFGraph.validate_vector(n, attr_dict.get('wcet', 0), 'wcet')
        super().add_node( n, attr_dict)

        # check phases
        phases_n = self.node[n].get('phases', None)
        if phases_n is None:
            phases_n = len(wcet)
        elif phases_n != len(wcet):
            raise ValueError("Incompatible nr. of phases (node {} has {} phases, not {})".format(n, phases_n, len(wcet)))

    def get_phases(self, v):
        return self.node[v].get('phases', 1)

    def add_edge(self, u, v, attr_dict = None, **attr):
        assert attr_dict is None, "FIXME: add_edge does not deal with attr_dict"

        if 'production' in attr:
            production = attr['production'] = SDFGraph.validate_vector( (u, v), attr['production'], 'production rate' )
            assert type(production) is cyclic

            # check phases
            phases_u = self.node[u].get('phases', None)
            if phases_u is None:
                self.node[u]['phases'] = len( production )
            elif phases_u != len(production):
                raise ValueError("Incompatible nr. of phases (node {} has {} phases, not {})".format(u, phases_u, len(production)))

        if 'consumption' in attr:
            consumption = attr['consumption'] = SDFGraph.validate_vector( (u, v), attr['consumption'], 'consumption rates' )
            assert type(consumption) is cyclic

            # check phases
            phases_v = self.node[v].get('phases', None)
            if phases_v is None:
                self.node[v]['phases'] = len( consumption )
            elif phases_v != len(consumption):
                raise ValueError("Incompatible nr. of phases (node {} has {} phases, not {})".format(v, phases_v, len(consumption)))

        try:
            tokens = int(attr['tokens'])
            assert tokens >= 0
            if tokens != 0:
                attr['tokens'] = tokens
            else:
                del attr['tokens']
        except ValueError:
            raise SDFParseError("Channel {} has an invalid number of tokens: {}".format( (u, v), attr['tokens'] ))
        except KeyError:
            tokens = 0

        if (self.has_edge(u, v)):
            existing_data = self.get_edge_data(u, v)
            e_pr = existing_data.get('production', cyclic(1))
            e_cr = existing_data.get('consumption', cyclic(1))
            e_g = gcd( e_pr.sum(), e_cr.sum() )
            e_toks = existing_data.get('tokens', 0) // e_g

            pr = attr.get('production', cyclic(1))
            cr = attr.get('consumption', cyclic(1))
            g = gcd( pr.sum(), cr.sum() )
            toks = attr.get('tokens', 0) // g

            if e_toks <= toks:
                print("Warning: ignoring non-binding edge ({}, {}) with {} tokens (keeping {} tokens)".format(u, v, toks, e_toks))
                pass
            else:
                print("Warning: ignoring non-binding edge ({}, {}) with {} tokens (keeping {} tokens)".format(u, v, e_toks, toks))
                self.remove_edge(u, v)
                super().add_edge(u, v, attr_dict, **attr)
        else:
            super().add_edge(u, v, attr_dict, **attr)

        if 'capacity' in attr:
            try:
                capacity = int(attr['capacity'])
                if capacity < tokens: raise Exception("Tokens on channel {} violates specified capacity".format((u, v)))

                del self.get_edge_data(u, v)['capacity']

                # create reverse channel (w, v)
                self.add_edge(v, u, production = attr.get('consumption', 1), consumption = attr.get('production', 1), tokens = capacity - tokens)

            except ValueError:
                del self.get_edge_data(u, v)['capacity']

                # create reverse channel (w, v)
                self.add_edge(v, u, production = attr['consumption'], consumption = attr['production'], var = attr['capacity'])

    def validate_vector(vector_name, vector, description = "property"):
        if type(vector) is str:
            m = re.match(r'\[([^]]*)\]', vector);
            if not m:
                raise SDFParseError("String expression for {} of {} must be comma-separated list enclosed in square brackets".format(description, vector_name))

            lst = []
            for substr in m.group(1).split(','):
                substr = substr.strip()
                m = re.match(r'(\d+)(?:\s*\*\s*(\d+)|$)', substr)
                if not m:
                    raise SDFParseError("Items in {} list of {} must be of form '<int>' or '<int> * <int>'".format(description, vector_name))

                if m.group(2):
                    lst = lst + ([int(m.group(2))] * int(m.group(1)))
                else:
                    lst.append(int(m.group(1)))

            if not lst:
                raise SDFParseError("Empty {} list for {}".format(description, channel))

            vector = cyclic(lst)
        else:
            try:
                it = iter(vector[:])
                lst = []
                for number in it:
                    try:
                        number = int(number)
                        lst.append(number)
                    except ValueError:
                        raise SDFParseError("{} has an invalid {} entry: {}".format( vector_name, description, number) )

                if not vector:
                    raise SDFParseError("Empty {} list for {}".format(description, vector_name))

                vector = cyclic(lst)
            except TypeError:
                try:
                    vector = cyclic(int(vector))
                except ValueError:
                    raise SDFParseError("{} has an invalid {}: {}".format( vector_name, description, vector) )

        return vector


class cyclic( tuple ):
    def __new__(self, *arg):
        try:
            return super().__new__(self, tuple(*arg))
        except TypeError as e:
            return super().__new__(self, tuple(arg))

    def __init__(self, *arg):
        self.__sum = sum(self)
    
    def __getitem__(self, idx):
        if type(idx) is slice:
            start = idx.start or 0
            start_mod = start % len(self)

            step = idx.step or 1
            period = len(self) // gcd( step, len(self) )

            pattern = []
            for i in range(period):
                if (idx.stop or 1) > 0:
                    pattern.append(super().__getitem__((start + i * step) % len(self)))
                else:
                    pattern.append(super().__getitem__((start - i * step) % len(self)))
            
            if idx.stop is not None:
                # return tuple
                if idx.stop >= 0:
                    result_len = max(0, 1 + (idx.stop - start - 1) // step)
                    num_periods = result_len // period
                    mod_periods = result_len % period
                    return tuple( pattern * num_periods + pattern[:mod_periods] )
                else:
                    result_len = max(0, 1 + (start - idx.stop - 1) // step)
                    num_periods = result_len // period
                    mod_periods = result_len % period
                    return tuple( pattern * num_periods + pattern[:mod_periods] )
            else:
                # return cyclic pattern
                return cyclic( pattern )
        else:
            return super().__getitem__(idx % len(self))

    def sum(self, start = 0, stop = None, step = 1):
        start_mod = start % len(self)
        period = len(self) // gcd( step, len(self) )

        pattern = self
        psum = self.__sum
        if start_mod > 0 or step != 1:
            pattern = []
            psum = 0
            for i in range(period):
                elem = super().__getitem__((start + i * step) % len(self))
                psum += elem
                pattern.append(elem)
        
        if stop is None:
            stop = len(self)

        # return tuple
        result_len = max(0, 1 + (stop - start - 1) // step)
        num_periods = result_len // period
        mod_periods = result_len % period
        return num_periods * psum + sum(pattern[:mod_periods])
    
class SDFParseError(Exception):
    pass

def predecessor(k = None, **kwargs):
    """ Computes the last producing firing that enables a consuming firing,
    for a CSDF channel with the specified production and consumption rates,
    and tokens.
    
    k       the number of consuming firings.
            if k is not specified, the function returns a function that takes a parameter k
            and computes predecessor(k)
    """
    prates = kwargs.get('production', cyclic(1))
    crates = kwargs.get('consumption', cyclic(1))
    tokens = kwargs.get('tokens', 0)
    plen = len(prates)
    clen = len(crates)
    csum = crates.sum()
    psum = prates.sum()

    if k is None:
        return lambda k: max(
            [ ((((k // clen) * csum + sum(crates[:(k % clen)]) - 1 - tokens - sum(prates[:i])) // psum) * plen + i + 1)
            for i in range(0, plen) ])
    else:
        maxval = None
        numerator = (k // clen) * csum + sum(crates[:(k % clen)]) - tokens - 1
        for i in range(0, plen):
            val_i = (numerator // psum) * plen + i + 1
            if not maxval or val_i > maxval:
                maxval = val_i
            numerator -= prates[i]

        return maxval

def chain(*predfuns):
    return reduce(lambda f, g: lambda x: g(f(x)), reversed(predfuns))

def load_sdf_xml(filename):
    tree = etree.parse(filename)
    root = tree.getroot()
    if root.tag != 'sdf3':
        raise SDFParseError("Missing sdf3 root element")
    if not 'type' in root.keys():
        raise SDFParseError("Missing attribute 'type'")
    graph_type = root.get('type')
    if graph_type not in ['sdf', 'csdf']:
        raise SDFParseError("Don't know how to deal with graph type {}".format(graph_type))
    if not 'version' in root.keys():
        raise SDFParseError("Missing attribute 'version'")
    if root.get('version') != '1.0':
        raise SDFParseError("Don't know how to deal with version {}".format(root.get('version')))
    app_graph = root.find('applicationGraph')
    if app_graph is None:
        raise SDFParseError("Missing 'applicationGraph' element")
    sdf_graph = app_graph.find(graph_type)
    sdf_graph_properties = app_graph.find('{}Properties'.format(graph_type))
    if sdf_graph is None:
        raise SDFParseError("Missing '{}' element".format(graph_type))
    if sdf_graph_properties is None:
        raise SDFParseError("Missing '{}Properties' element".format(graph_type))

    # Create empty directed graph
    sdfg = SDFGraph()

    # go over all actors and look up each actor's properties
    for actor_element in sdf_graph.findall("actor"):
        name = actor_element.get('name')
        times = sdf_graph_properties.find("actorProperties[@actor='{}']/processor[@default='true']/executionTime[@time]".format(name))
        if times is None:
            print("Warning: no execution time found for actor {}, assuming 1".format(name))
            times = '1'

        wcet = list()
        assert times is not None
        for t in times.get('time').split(','):
            try:
                wcet.append(int(t))
            except ValueError:
                raise SDFParseError("Invalid execution time for actor {}".format(name))
        sdfg.add_node( name, wcet = wcet )

    # go over channels
    for channel_element in sdf_graph.findall("channel"):
        src = channel_element.get('srcActor'), channel_element.get('srcPort')
        assert src[0] is not None, "channel has no srcActor"
        assert src[1] is not None, "channel has no srcPort"
        src_port = sdf_graph.find("actor[@name='{}']/port[@name='{}']".format( *src ))
        if src_port is None:
            raise SDFParseError("Unknown actor/port: {}/{}".format( *src))
        assert 'type' in src_port.attrib and src_port.attrib['type'] == 'out'
        production = list()
        for rate in src_port.get('rate', '1').split(','):
            try:
                production.append(int(rate))
            except ValueError:
                raise SDFParseError("Invalid rate for actor/port {}/{}".format(*src))

        dst = channel_element.get('dstActor'), channel_element.get('dstPort')
        dst_port = sdf_graph.find("actor[@name='{}']/port[@name='{}']".format( *dst ))
        if dst_port is None:
            raise SDFParseError("Unknown actor/port: {}/{}".format(*dst))
        assert 'type' in dst_port.attrib and dst_port.attrib['type'] == 'in'
        consumption = list()
        for rate in dst_port.get('rate', '1').split(','):
            try:
                consumption.append(int(rate))
            except ValueError:
                raise SDFParseError("Invalid rate for actor/port {}/{}".format(*dst))

        try:
            tokens = int(channel_element.get('initialTokens', 0))
        except ValueError:
            raise SDFParseError("Invalid initialTokens attribute for channel {}".format(channel_element.get('name')))

        sdfg.add_edge( src[0], dst[0], production = cyclic(production), consumption = cyclic(consumption), tokens = tokens )

    return sdfg

def write_sdf_json( g, filename ):
    with open(filename, 'w') as outfile:
        outfile.write( g.to_JSON())

def write_txt( g, filename ):
    with open(filename, 'w') as outfile:
        for v, data in g.nodes_iter( data = True ):
            outfile.write( "{} {}\n".format( v, data.get('wcet', 0 )[0]))

        outfile.write('\n')

        for v, w, data in g.edges_iter( data = True ):
            prates = data.get('production', cyclic(1))
            crates = data.get('consumption', cyclic(1))
            tokens = data.get('tokens', 0)
            outfile.write( "{} {} {} {} {}\n".format( v, w, prates[0], crates[0], tokens))
            
        outfile.write('\n')

def write_sdf_xml( g, filename ):
    root = etree.Element('sdf3', type = 'sdf', version = '1.0')
    ag = etree.SubElement(root, 'applicationGraph')
    sdf = etree.SubElement(ag, 'sdf', name = 'g', type = 'G')
    sdfprops = etree.SubElement(ag, 'sdfProperties')
    ports = dict()
    actors = dict()
    for v, data in g.nodes_iter( data = True ):
        wcet = ','.join(map(str, data['wcet']))
        ports[ v ] = 0
        actors[ v ] = etree.SubElement(sdf, 'actor', name = '{}'.format(v), type = 'A')
        actorprops = etree.SubElement(sdfprops, 'actorProperties', actor = '{}'.format(v))
        processor = etree.SubElement(actorprops, 'processor', type = 'p1', default = 'true')
        etree.SubElement( processor, 'executionTime', time = '{}'.format( wcet ))

    cidx = 0
    for v, w, data in g.edges_iter( data = True ):
        cidx += 1
        prates = ','.join(map( str, data.get('production', [1])))
        crates = ','.join(map( str, data.get('consumption', [1])))

        # add source port
        srcport = etree.SubElement( actors[v], 'port', name = 'p{}'.format( ports[v] ), type = 'out', rate = prates)
        dstport = etree.SubElement( actors[w], 'port', name = 'p{}'.format( ports[w] ), type = 'in', rate = crates)
        xmldata = dict(
            srcActor = '{}'.format(v), srcPort = 'p{}'.format( ports[ v ]),
            dstActor = '{}'.format(w), dstPort = 'p{}'.format( ports[ w ]))
        ports[ v ] += 1; ports[ w ] += 1

        if 'tokens' in data:
            xmldata['initialTokens'] = '{}'.format(data['tokens'])

        channel = etree.SubElement(sdf, 'channel', name = 'ch{}'.format(cidx), **xmldata)

    tree = etree.ElementTree( root )
    tree.write( filename,  )
    
def load_sdf(filename):
    data = json.load(open(filename))
    if 'actors' not in data:
        raise SDFParseError("Missing field: 'actors'")

    if 'channels' not in data:
        raise SDFParseError("Missing field: 'channels'")

    # Create empty directed graph
    sdfg = SDFGraph()

    for actor in data['actors']:
        if 'name' not in actor:
            raise Exception("Missing field in actor: 'name'")

        name = actor['name']

        if 'wcet' not in actor:
            print("Warning: assuming constant wcet of 1 for actor '{}'".format(name), file=sys.stderr)
            wcet = 1
        else:
            wcet = actor['wcet']
            
        sdfg.add_node(name, wcet = wcet)

    for channel in data['channels']:
        if 'from' not in channel: raise Exception("Missing field in channel: 'from'")
        if 'to' not in channel: raise Exception("Missing field in channel: 'to'")

        (v, w) = (channel['from'], channel['to'])
        if v not in sdfg: raise Exception("Unknown source actor '{}' specified in channel".format(v))
        if w not in sdfg: raise Exception("Unknown destination actor '{}' specified in channel".format(w))

        sdfg.add_edge(v, w, **channel)

        # remove unnecessary elements
        del sdfg.get_edge_data(v, w)['from']
        del sdfg.get_edge_data(v, w)['to']

    return sdfg

def check_consistency(sdfg):
    node_lcm_rates = {}

    for (v, w) in sdfg.edges():
        prates = sdfg.get_edge_data(v, w).get('production', cyclic(1))
        assert type(prates) is cyclic
        crates = sdfg.get_edge_data(v, w).get('consumption', cyclic(1))
        assert type(crates) is cyclic

        if prates.sum() <= 0 or len(prates) == 0:
            raise Exception("({}, {}) has an invalid production rate vector (sum = {})".format(v, w, prates.sum()))

        if crates.sum() <= 0 or len(crates) == 0:
            raise Exception("({}, {}) has an invalid consumption rate vector (sum = {})".format(v, w, crates.sum()))

        avg_prate = Fraction(prates.sum(), len(prates))
        avg_crate = Fraction(crates.sum(), len(crates))

        node_lcm_rates[v] = lcm(lcm(node_lcm_rates.get(v, 1), prates.sum()), avg_prate.denominator)
        if node_lcm_rates[v] == 0:
            raise Exception("LCM of rates associated with actor {} is zero", v)

        node_lcm_rates[w] = lcm(lcm(node_lcm_rates.get(w, 1), crates.sum()), avg_crate.denominator)
        if node_lcm_rates[w] == 0:
            raise Exception("LCM of rates associated with actor {} is zero", w)

    fractional_q = {}
    subgraph = nx.DiGraph()
    undirected = sdfg.to_undirected()
    for (v, w) in dfs_edges(undirected):
        if not sdfg.has_edge(v, w):
            v, w = w, v
        
        # get rates of channel (v, w)
        data = sdfg.get_edge_data(v, w)
        (p_vw, c_vw) = data.get('production', cyclic(1)), data.get('consumption', cyclic(1))

        # if there is an edge (w, v) in sdfg, check whether it's consistent with (v, w)
        if sdfg.has_edge(w, v):
            data = sdfg.get_edge_data(w, v)
            (p_wv, c_wv) = data.get('production', cyclic(1)), data.get('consumption', cyclic(1))
            if p_vw.sum() * p_wv.sum() == c_vw.sum() * c_wv.sum():
                subgraph.add_edge( w, v )

        # check consistency of (v, w) with rest of the graph
        if (v in fractional_q) and (w in fractional_q):
            if p_vw.sum() * fractional_q[v] != c_vw.sum() * fractional_q[w]:
                # don't include (v, w) in consistent subgraph
                continue

        elif (v in fractional_q):
            fractional_q[w] = fractional_q[v] * Fraction(p_vw.sum(), c_vw.sum())

        elif (w in fractional_q):
            fractional_q[v] = fractional_q[w] * Fraction(c_vw.sum(), p_vw.sum())

        else:
            fractional_q[v] = 1
            fractional_q[w] = Fraction(p_vw.sum(), c_vw.sum())
        
        subgraph.add_edge( v, w )

    # compute the LCM of the denominators in the fractional repetition vector
    m = 1
    for f in fractional_q.values():
        m = lcm(m, Fraction(f).denominator)

    # multiply all fractions with the LCM of their denominators to obtain the repetition vector
    q = {}
    tpi = 1
    for k in fractional_q:
        f = fractional_q[k] 
        q[k] = int(f * m * sdfg.get_phases(k))
        tpi = lcm(tpi, node_lcm_rates[k] * f * m)

    s = {}
    for v, w, data in sdfg.edges(data = True):
        (prate, crate) = data.get('production', cyclic(1)), data.get('consumption', cyclic(1))
        s[ (v, w) ] = (tpi * len(prate)) // (q[v] * prate.sum())

    return q, s, tpi, subgraph


