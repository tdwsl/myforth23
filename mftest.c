#include "mf23.h"

int main(int argc, char **args) {
    int i;

    init();

    if(argc > 1) {
        for(i = 1; i <= argc; i++)
            runFile(args[i]);
    } else run();

    return 0;
}
