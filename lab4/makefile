CC		= gcc

CFLAGS		= -g -Wall -pedantic -std=c99

OBJS		= main.o list.o error.o random.o set.o dataflow.o

OUT		= live

LDFLAGS		= -lpthread

S=100
V=10
U=4
A=1000
T=4
P=0

all: $(OBJS)	
	$(CC) $(CFLAGS) $(OBJS) $(LDFLAGS) -o $(OUT)
	valgrind --tool=helgrind ./$(OUT) $(S) $(V) $(U) $(A) $(T) $(P)
	./$(OUT) $(S) $(V) $(U) $(A) $(T) $(P)

clean:
	rm -f $(OUT) $(OBJS) cfg.dot
