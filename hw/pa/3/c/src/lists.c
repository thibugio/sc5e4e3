/* Rebecca Frederick
   EECS 345 
   Programming Exercise 3
   implements a doubly-linked list.
*/

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

struct node_t {
    int *e;
    struct node_t *next;
    struct node_t *prev;
};

// create a node with the given data
struct node_t *create_node_t(int *elem) {
    struct node_t *n = malloc(sizeof(struct node_t) + sizeof(*elem));
    n->e = elem;
    n->next = NULL;
    n->prev = NULL;
    return n;
}

struct list_t {
    struct node_t *head;
    struct node_t *tail;
};

// create an empty list
struct list_t *create_list_t() {
    struct list_t *l = malloc(sizeof(struct list_t));
    l->head = NULL;
    l->tail = NULL;
    return l;
}

// take an element and a list and add a new node containing the element 
// to the front of the list.
void add_to_front(int *const elem, struct list_t *l) {
    struct node_t *n = create_node_t(elem);
    if (l->head != NULL) {
        l->head->prev = n;
        n->next = l->head;
        l->head = n;
    } else {
        l->head = n;
        l->tail = n;
    }
}

// take an element and a list and add a new node containing the element
// to the back of the list.
void add_to_back(int *const elem, struct list_t *l) {
    struct node_t *n = create_node_t(elem);
    if (l->tail != NULL) {
        n->prev = l->tail;
        l->tail->next = n;
        l->tail = n;
    } else {
        l->tail = n;
        l->head = n;
    }
}

// takes a list and removes and frees the node at the front of the list 
// and returns the element stored in that node.
int *remove_from_front(struct list_t *l) {
    int *e = NULL;
    if (l->head != NULL) {
        e = l->head->e;
        struct node_t *temp = l->head;
        l->head = l->head->next;
        if (l->head == NULL) l->tail = l->head;
        free(temp);
    }
    return e;
}

// takes a list and removes and frees the node at the back of the list
// and returns the element stored in that node.
int *remove_from_back(struct list_t *l) {
    int *e = NULL;
    if (l->tail != NULL) {
        e = l->tail->e;
        struct node_t *temp = l->tail;
        l->tail = l->tail->prev;
        if (l->tail == NULL) l->head = l->tail;
        free(temp);
    }
    return e;
}

// takes two arrays, an int, and two function pointers
// the arrays have the same type and length
// the int is the length of the arrays
// the first function pointer points to an insert function
// the second function  pointer points to a remove function
// the contents of the second array should be the reverse of the 
// contents of the first array when the method completes.
void transfer(int *a1, int *a2, int len, void (*fi)(int *, struct list_t *), int* (*fr)(struct list_t*)) {
    if (!len) return;
    struct list_t *l = create_list_t();
    int i;
    for (i = 0; i < len; i++)
        fi(&(a1[i]), l); 
    for (i = 0; i < len; i++)
        a2[i] = *(fr(l));
    free(l);
}

void print_arr(int *a, char *fmt, int len) {
    int i;
    int *temp = a;
    for (i = 0; i < len; i++) {
        printf(fmt, *temp);
        printf(" ");
        temp++;
    }
    printf("\n");
}

// test the transfer function
int main(int argc, char *argv[]) {
    int a1[8];
    int a2[8];
    
    int i;
    for (i=0; i<8; i++) {
        if (i < 2)
            a1[i] = i;
        else
            a1[i] = a1[i-1] + a1[i-2];
    }
    
    print_arr(a1, "%d", 8);
    
    transfer(a1, a2, 8, &add_to_front, &remove_from_front);

    print_arr(a2, "%d", 8);

    return 0;
}
