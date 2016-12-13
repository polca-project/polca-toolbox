This directory contains the Python files that implements the
Reinforment Learning implementation for learning Heuristics to guide
the Haskell-based program transformation toolchain developed within
the POLCA project.

The content of the directory is:

- 00README.txt: this file

- main.py:      main program. It is configured to run the set of
  examples used in the PROHA paper.

- PolcaEnv.py:  module used by main.py that defines the
  "enviroment". It defines:
  - The transition matrix
  - Rewards associated to RL states
  - Methods to act over the "environment" (e.g. transitions, ...)

- polcamdp.py:  module used by main.py that defines de MDP task. This
  module defines functionality related which is basicaly perform RL
  actions and obtain rewards and observations based on what is defined
  in module "PolcaEnv.py"

- utils: Directory containing the program "generateRLtable.py" that
  generates the POLCA transition table by parsing the file
  "tableData.txt". The transition table is then used in "PolcaEnv.py"


TODO:

- The RL code in this directory requires the library PyBrain. Haven't
  checked special instalation requirements. So it is pending to check
  it and add install instruccions in this file.
