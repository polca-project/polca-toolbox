make rules FILE=rules_nbody
make exe_ext
make is_rollable
make features_ext
./polca_s2s_ext "python -OO ../Machine_learning_component/static_analyser/s2s_ml_interface.py" demo/nbody/2arrays_maxj/nbody.c BLOCK_ABS