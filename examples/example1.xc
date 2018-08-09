#include <run.xh>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void f(int x, int y)
{
    printf("%d\n", x + y);
}

int main(int argc, char **argv)
{
    run f(1, 2);
    sleep(1);
    return 0; 
}

