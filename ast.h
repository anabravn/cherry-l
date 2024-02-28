#define MAX_SIZE 1024

typedef union {
    int int4;
    float fp;
    char *str;
    char ch;
} NodeValue;

typedef struct {
    int one, two, three;
    char *type;
    NodeValue value;
} Node;

int mknode(int, int, int, char*, NodeValue v);
void free_tree();
void print_tree(int, int);
void print_nodes();
