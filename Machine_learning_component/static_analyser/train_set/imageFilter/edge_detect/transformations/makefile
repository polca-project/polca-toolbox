ROOT_PATH = ../../..

OUT_PATH = ../data
OUT_FILES = output*.dsp

PATH1 = edge_detect_1_1 edge_detect_1_2 edge_detect_1_3 edge_detect_1_4

EXECS = edge_detect check_output $(PATH1)
# $(PATH2)

include $(ROOT_PATH)/makefile_common

# all: $(EXECS)

# %.o: %.c
# 	$(CC) $(FLAGS) -c $(INCLUDE) $< -o $@

# $(EXECS) : % : %.o
# 	$(CC) $(FLAGS) $(IOSRC) $< -o $@.x

# clean:
# 	-rm -f *.o
# 	-rm -f *.x
# 	-rm -f ../data/output*.dsp
# 	-rm -f *~

# clean_out:
# 	-rm -f ../data/output*.dsp

# run:
# ifdef EXEC
# 	make clean_out
# 	$(EXEC)
# 	./check_output.x
# else
# 	@echo "ERROR: Need to provide name of executable, e.g.: make run EXEC=path/fileName"
# endif
