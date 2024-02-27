#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "ast.h"

Node *graph[MAX_SIZE] = {0};

int search_node(int one, int two, int three, char *type) 
{
    int i;

    for(i = 0; i < MAX_SIZE; i++) {
        if (graph[i]) {
            if(
                graph[i]->one == one &&
                graph[i]->two == two &&
                graph[i]->three == three &&
                !strcmp(type, graph[i]->type)
            ) return i;
        }
    }

    return -1;
}

int mknode(int one, int two, int three, char *type)
{
    Node *n; 
    int i;

    i = search_node(one, two, three, type);
    if(i != -1) return i;

    n = malloc(sizeof(Node));
    n->one = one;
    n->two = two;
    n->three = three;
    n->type = type;

    for(i = 0; i < MAX_SIZE && graph[i]; i++)
        ;

    graph[i] = n;

    return i;
}

void print_tree(int root, int level) 
{
    Node *n;

    if (!graph[root] || root < 0)
        return;

    n = graph[root];

    printf("%d", level);
    
    for(int i = 0; i < level; i++)
        printf(" -");

    printf(" %s\n", n->type);

    if(n->one != -1)
        print_tree(n->one, level+1);
    if(n->two != -1)
        print_tree(n->two, level+1);
    if(n->three != -1)
        print_tree(n->three, level+1);
}

/*
void print_table(void)
{
    for(int i = 0; i < MAX_SIZE; i++) {
        if (graph[i]) {
            printf("%d: %s  \t%d %d %d\n", i, graph[i]->type, 
                    graph[i]->one, graph[i]->two, graph[i]->three);
        }
    }
}
*/

void free_tree(void)
{
    for(int i = 0; i < MAX_SIZE; i++) {
        if (graph[i]) {
            free(graph[i]);
            graph[i] = NULL;
        }
    }
}

/*
int main(void)
{
    mknode(1, 2, 3, "pai");
    mknode(-1, -1, -1, "filho1");
    mknode(4, -1, -1, "filho2");
    mknode(-1, -1, -1, "filho3");
    mknode(mknode(-1, -1, -1, "filho3"),-1,-1, "filho4");
    print_tree(0, 0);
    putchar('\n');
    print_table();

    free_table();

    return 0;
}
*/
