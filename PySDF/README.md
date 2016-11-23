# SDFpy

SDFpy is a library for the management and analysis of synchronous dataflow graphs,
written in Python. Synchronous dataflow (SDF) graphs model stream processing
systems. 

An SDF graph represents a function, or computation, which is applied to
an infinite stream of data. Nodes in an SDF graph act as functions: they map input
(data) streams, taken from incoming edges, to output streams, which are produced
onto outgoing edges. 

The model allows for temporal analysis: by assigning (worstcase)
processing times (execution times) to the graph’s nodes, one may compute
how much data can be processed by an SDF graph, per time unit.

SDFpy is capable of performing several analyses on SDF graphs, including
throughput analysis, which computes the performance bottleneck of the graph.

Performance bottlenecks indicate which sequences of computations limit the maximum
throughput of the graph, and thus provide valuable insight into which parts of
the computation should be improved, or assigned more resources.

SDFpy has been developed at the University of Twente.