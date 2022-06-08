#include <cstdio>
#include <cstdlib>
#include "generator.h"

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("usage: %s <board-size>\n",argv[0]);
        exit(1);
    }

    int n = atoi(argv[1]);
    Generator g(n);
    g.gen();
    g.shuffle_board();
    g.print_board();

    return 0;
}
