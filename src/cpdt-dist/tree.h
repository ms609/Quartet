#ifndef TREE_H_
#define TREE_H_

#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <sstream>
#include <cassert>

#include "treenode.h"


class tree {
    
private:
    
    std::vector<tree_node*> nodes;
    int leaves_num;
    
    void get_leaves_count_supp(tree_node* node, std::vector<int>& counts) {
        assert(nodes[node->get_id()] == node);
        if (node->is_leaf()) {
            counts[node->get_id()] = 1;
            return;
        }
        for (int i = 0; i < node->get_num_children(); i++) {
            get_leaves_count_supp(node->get_child(i), counts);
            counts[node->get_id()] += counts[node->get_child(i)->get_id()];
        }
    }
    
public:
    
    const static int NONE = -1;
    
    tree() : leaves_num(0) {}
    
    tree(std::vector<tree_node*>& nodes) : leaves_num(0) {
        this->nodes = nodes;
        
        for (size_t i = 0; i < get_nodes_num(); i++) {
            if (this->nodes[i]->is_leaf()) {
                leaves_num++;
            }
        }
    }
    ~tree() {
        for (size_t i = 0; i < nodes.size(); i++) {
            delete nodes[i];
        }
    }
    
    int get_leaves_num() { return leaves_num; }
    size_t get_nodes_num() { return nodes.size(); }
    
    tree_node* get_node(int i) { return nodes[i]; }
    
    std::vector<int> get_leaves_count() {
        std::vector<int> leaves_count(get_nodes_num(), 0);
        get_leaves_count_supp(get_root(), leaves_count);
        return leaves_count;
    }
    
    tree_node* get_root() { return nodes[0]; }
    
    void make_biggest_subtree_first() {
        std::vector<int> leaves_count = get_leaves_count();
        
        for (int i = 0; i < get_nodes_num(); i++) {
            tree_node* node = get_node(i);
            if (node->is_leaf()) continue;
            
            int largest_subtree = 0;
            for (int j = 1; j < node->get_num_children(); j++) {
                if (leaves_count[node->get_child(largest_subtree)->get_id()] < leaves_count[node->get_child(j)->get_id()]) {
                    largest_subtree = j;
                }
            }
            
            node->swap_children(0, largest_subtree);
        }
    }
    
    std::string to_string() {
        std::stringstream ss;
        for (int i = 0; i < get_nodes_num(); i++) {
            ss << i << " " << nodes[i]->to_string() << std::endl;
        }
        return ss.str();
    }
    
    bool is_binary() {
        for (size_t i = 0; i < nodes.size(); i++) {
            if (nodes[i]->get_num_children() > 2) return false;
        }
        return true;
    }
    
};


#endif /* TREE_H_ */
