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

void binary_tree_delete_node_post_order (binary_tree_node *node) {
    if (node == NULL) {
        return;
    }

    binary_tree_delete_node_post_order(node->left);
    binary_tree_delete_node_post_order(node->right);

    printf("Freeing node: %d\n", node->key);
    free(node);
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

void binary_tree_delete (binary_tree *bst, int key) {
    if (bst == NULL) {
        return;
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

    if (*next == NULL) {
        return;
    }

    binary_tree_node *reference;

    if ((*next)->left == NULL && (*next)->right == NULL) {
        reference = *next;
        *next = NULL;
    }
    else if ((*next)->left == NULL) {
        reference = *next;
        *next = (*next)->right;
    }
    else if ((*next)->right == NULL) {
        reference = *next;
        *next = (*next)->left;
    }
    else { // we have both children...
        binary_tree_node **leftmost = &(*next)->right;

        while ((*leftmost)->left != NULL) {
            leftmost = &(*leftmost)->left;
        }

        // copy satellite data
        (*next)->key = (*leftmost)->key;

        // stage for removal
        reference = *leftmost;

        // right children, needs splicing
        if ((*leftmost)->right != NULL) {
            *leftmost = (*leftmost)->right;
        }
        else {
            // else just remove it.
            *leftmost = NULL;
        }
    }

    free(reference);
}

void binary_tree_print_in_order (binary_tree *bst) {
    binary_tree_print_node_in_order(bst->root);
    printf("\n");
}

void binary_tree_destroy (binary_tree **bst) {
    if (*bst == NULL) {
        return;
    }

    binary_tree_delete_node_post_order((*bst)->root);

    free(*bst);
    *bst = NULL;
}

int main (void) {
    binary_tree *bst = binary_tree_create();

    srand((int)time(NULL));

    int i = 0;
    for (i = 0; i < 30; i++) {
        binary_tree_insert(bst, rand() % 100);
    }

    printf("Original tree: ");
    binary_tree_print_in_order(bst);

    for (i = 0; i < 100; i++) {
        binary_tree_delete(bst, rand() % 100);
    }

    printf("Pruned tree: ");
    binary_tree_print_in_order(bst);

    binary_tree_destroy(&bst);

    return 0;
}
