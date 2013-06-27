////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// binary tree implementation
//
// Practicing for Google's interveiews
//
// Francisco Soto <ebobby@ebobby.org>
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>

typedef struct binary_tree_node binary_tree_node;

struct binary_tree_node  {
    binary_tree_node *left;
    binary_tree_node *right;
    int key;
};

typedef struct {
    binary_tree_node  *root;
} binary_tree;

// Helper functions...

void binary_tree_insert_into_node (binary_tree_node **node, int key) {
    binary_tree_node *tmp = malloc(sizeof(binary_tree_node));
    tmp->left = NULL;
    tmp->right = NULL;
    tmp->key = key;
    *node = tmp;
}

void binary_tree_print_node_in_order (binary_tree_node *node) {
    if (node == NULL) {
        return;
    }

    binary_tree_print_node_in_order(node->left);
    printf("%d ", node->key);
    binary_tree_print_node_in_order(node->right);
}

// binary_tree functions...

binary_tree *binary_tree_create () {
    binary_tree *result = malloc(sizeof(binary_tree));
    result->root = NULL;
    return result;
}

void binary_tree_insert (binary_tree *bst, int key) {
    if (bst == NULL) {
        return;
    }

    binary_tree_node **next = &bst->root;

    while (*next != NULL) {
        if (key <= (*next)->key) {
            next = &(*next)->left;
        }
        else {
            next = &(*next)->right;
        }
    }

    binary_tree_insert_into_node(next, key);
}

binary_tree_node* binary_tree_search (binary_tree *bst, int key) {
    if (bst == NULL || bst->root == NULL) {
        return NULL;
    }

    binary_tree_node **next = &bst->root;

    while (*next != NULL) {
        if (key == (*next)->key) {
            break;
        }
        else if (key < (*next)->key) {
            next = &(*next)->left;
        }
        else {
            next = &(*next)->right;
        }
    }

    return *next;
}

void binary_tree_print_in_order (binary_tree *bst) {
    binary_tree_print_node_in_order(bst->root);
    printf("\n");
}

int main (void) {
    binary_tree *bst = binary_tree_create();

    binary_tree_insert(bst, 5);
    binary_tree_insert(bst, 50);
    binary_tree_insert(bst, 1);
    binary_tree_insert(bst, 100);
    binary_tree_insert(bst, 21);
    binary_tree_insert(bst, -10);
    binary_tree_insert(bst, 3);

    binary_tree_print_in_order(bst);

    printf("%d\n", binary_tree_search(bst, 100)->key);

    return 0;
}
