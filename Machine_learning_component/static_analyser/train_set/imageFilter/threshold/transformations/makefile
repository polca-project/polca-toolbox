ROOT_PATH = ../../..

OUT_PATH = ../data
OUT_FILES = output*.dsp


PATH1 = threshold0_1_1 threshold0_1_2 threshold0_1_3 threshold0_1_4

EXECS = threshold0 check_output $(PATH1)
TRANS = threshold0_transformed

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
