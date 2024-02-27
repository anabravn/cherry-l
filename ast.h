#define MAX_SIZE 1024

typedef struct {
    int one, two, three;
    char *type;
} Node;

int mknode(int, int, int, char*);
void free_tree();
void print_tree(int, int);
