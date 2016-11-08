CC = gcc
EXTRAE_HOME = ./extrae/

INC = -I$(EXTRAE_HOME)/include

SRC =   polca_event.c         

OBJS =  polca_event.o 


HDRS =  inst_polca.h 

.PRECIOUS: $(OBJS)

DEFINES =

%.o : %.c
	$(CC) -fPIC $(CFLAGS) $(INC) -c  $(HDRS) $*.c   

libinstpolca.so.1.0.1: $(OBJS)

	$(CC) -O3 -shared -Wl,-soname,libinstpolca.so.1 -o libinstpolca.so.1.0.1 $(OBJS)
	ln -sf libinstpolca.so.1.0.1 libinstpolca.so.1.0
	ln -sf libinstpolca.so.1.0 libinstpolca.so.1
	ln -sf libinstpolca.so.1 libinstpolca.so

clean:
	rm -f $(OBJS) libinstpolca.so.1.0.1 libinstpolca.so.1.0 libinstpolca.so.1 libinstpolca.so *.gch
