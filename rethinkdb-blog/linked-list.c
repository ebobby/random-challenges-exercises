////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Linked list implementation.
//
// Original article: http://rethinkdb.com/blog/will-the-real-programmers-please-stand-up/
//
// Challenge: Write a C function that reverses a singly-linked list.
//
// Francisco Soto <ebobby@ebobby.org>
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>

typedef struct Node Node;

struct Node {
    Node *next;
    int   key;
    void *value;
};

typedef struct {
    Node *root;
} LinkedList;

LinkedList* linked_list_create () {
    LinkedList *list = malloc(sizeof(LinkedList));
    list->root = NULL;
    return list;
}

Node* linked_list_new_node (int key, void *value) {
    Node* node = malloc(sizeof(Node));
    node->next = NULL;
    node->key = key;
    node->value = value;
    return node;
}

Node* linked_list_insert (LinkedList *list, int key, void* value) {
    Node **where = &list->root;

    while ((*where) != NULL)
        where = &(*where)->next;

    (*where) = linked_list_new_node(key, value);

    return (*where);
}

Node *linked_list_find (LinkedList *list, int key) {
    Node **where = &list->root;

    while ((*where) != NULL && (*where)->key != key)
        where = &(*where)->next;

    return (*where);
}

void linked_list_destroy (LinkedList *list) {
    Node **where = &list->root;
    Node *next = NULL;

    while ((*where) != NULL) {
        printf("Destroying node key: %d.\n", (*where)->key);
        next = (*where)->next;
        free((*where));
        where = &next;
    }

    printf("Destroying list.\n");
    free(list);
}

LinkedList *linked_list_reverse (LinkedList *list) {
    LinkedList *reversed = linked_list_create();
    Node *swap = NULL;

    while (list->root != NULL) {
        swap = list->root;              // Remove from original list.
        list->root = list->root->next;

        swap->next = reversed->root;    // Insert at the root of the new list.
        reversed->root = swap;
    }

    linked_list_destroy(list);

    return reversed;
}

void linked_list_print (LinkedList *list) {
    Node **where = &list->root;

    while ((*where) != NULL) {
        printf("%d, ", (*where)->key);
        where = &(*where)->next;
    }
    printf(" NULL\n");
}

int linked_list_count (LinkedList *list) {
    Node **where = &list->root;
    int count = 0;

    while ((*where) != NULL) {
        count++;
        where = &(*where)->next;
    }

    return count;
}

int main (void) {
    LinkedList *list = linked_list_create();

    linked_list_insert(list, 1, NULL);
    linked_list_insert(list, 2, NULL);
    linked_list_insert(list, 3, NULL);
    linked_list_insert(list, 4, NULL);
    linked_list_insert(list, 5, NULL);
    linked_list_insert(list, 6, NULL);
    linked_list_insert(list, 7, NULL);
    linked_list_insert(list, 8, NULL);
    linked_list_insert(list, 9, NULL);
    linked_list_insert(list, 10, NULL);

    printf("Original list has %d items: ", linked_list_count(list));
    linked_list_print(list);

    list = linked_list_reverse(list);

    printf("Reversed list has %d items: ", linked_list_count(list));
    linked_list_print(list);

    linked_list_destroy(list);

    return 0;
}
