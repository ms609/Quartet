#ifndef TREENODE_H_
#define TREENODE_H_

#include <sstream>
#include <algorithm>

class tree_node {

private:

	int id;
	std::vector<tree_node*> children;
	tree_node* parent;
	int taxa;

	void set_parent(tree_node* parent) {
		this->parent = parent;
	}

public:

	tree_node() : id(-1), parent(NULL), taxa(-1) {}
	tree_node(int id) : id(id), parent(NULL), taxa(-1) { }
	tree_node(int id, tree_node* left, tree_node* right) : id(id), parent(NULL), taxa(-1) {
		if (left != NULL) { left->set_parent(this); }
		children.push_back(left);
		if (right != NULL) { right->set_parent(this); }
		children.push_back(right);
	}
	tree_node(int id, int taxa) : id(id), parent(NULL), taxa(taxa) { }

	tree_node(tree_node& other) {
		this->id = other.id;
		this->children = other.children;
		this->parent = other.parent;
		this->taxa = other.taxa;
	}

	~tree_node() {
		children.clear();
	}

	int get_id() { return id; }

	tree_node* get_left() { return children.empty() ? NULL : children[0]; }
	tree_node* get_right() { return children.empty() ? NULL : children[children.size()-1]; }

	tree_node* get_parent() { return parent; }

	int get_taxa() { return taxa; }

	int get_num_children() { return children.size(); }
	tree_node* get_child(int i) { return children[i]; }
	void add_child(tree_node* node) {
		children.push_back(node);
		node->set_parent(this);
	}
	void swap_children(int c1, int c2) {
		tree_node* temp = children[c1];
		children[c1] = children[c2];
		children[c2] = temp;
	}

	bool is_leaf() { return children.empty(); }

	std::string to_string() {
		std::stringstream ss;
		ss << "id=" << get_id();
		ss << " children = (";
		if (!children.empty()) {
			ss << children[0]->get_id();
		}
		for (unsigned int i = 1; i < children.size(); i++) {
			ss << ", " << children[i]->get_id();
		}
		ss << "), parent = " << (parent == NULL ? -1 : parent->get_id()) << ", taxa = " << taxa;
		return ss.str();
	}
};


#endif /* TREENODE_H_ */
