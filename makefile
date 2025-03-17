CC=gcc

kilo: kilo_nico.c
	$(CC) kilo_nico.c -o kilo -Wall -Wextra -pedantic -std=c11
