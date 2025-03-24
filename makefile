CC = gcc
OBJS = $(patsubst %.c,%.o,$(wildcard *.c))
CFLAGS = -Wall -Wextra -std=c99
EXEC = vnim 

.PHONY: clean html

$(EXEC): $(OBJS)
	$(CC) $^ -o $(EXEC)

%.o: %.c
	$(CC) $< $(CFLAGS) -c -o $@

clean:
	rm -f $(OBJS) $(EXEC)

