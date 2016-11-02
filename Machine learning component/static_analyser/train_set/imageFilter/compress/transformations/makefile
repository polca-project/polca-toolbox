ROOT_PATH = ../../..

OUT_PATH = ../data
OUT_FILES = output*.dsp

PATH1 = compress_1_1 compress_1_2 compress_1_3 compress_1_4 compress_1_5 compress_1_6 compress_1_7 compress_1_8 compress_1_9 compress_1_10 compress_1_11
PATH2 = compress_2_1 compress_2_2 compress_2_3 compress_2_4 compress_2_5 compress_2_6 compress_2_7 compress_2_8 compress_2_9 compress_2_10 compress_2_11 compress_2_12  compress_2_13 compress_2_14 compress_2_15
# PATH3 = step3_1 step3_2 step3_3
# PATH4 = step4_1 step4_2 step4_3
PATH5 = compress_5_1 compress_5_2 compress_5_3 compress_5_4 compress_5_5 compress_5_6 compress_5_7 compress_5_8 compress_5_9 compress_5_10
PATH6 = compress_6_1 compress_6_2 compress_6_3 compress_6_4 compress_6_5 compress_6_6 compress_6_7 compress_6_8 compress_6_9 compress_6_10 compress_6_11 compress_6_12 compress_6_13 compress_6_14

EXECS = compress check_output $(PATH6)
TRANS = compress_transformed

include $(ROOT_PATH)/makefile_common

# $(PATH1) $(PATH2)

# all: $(EXECS)

# %.o: %.c
# 	$(CC) $(FLAGS) -c $(INCLUDE) $< -o $@

# $(EXECS) : % : %.o
# 	$(CC) $(FLAGS) $(IOSRC) $< -o $@.x

# trans:
# 	$(CC) $(FLAGS) -c $(INCLUDE) $(TRANS).c -o $(TRANS).o
# 	$(CC) $(FLAGS) $(IOSRC) $(TRANS).o -o $(TRANS).x

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
