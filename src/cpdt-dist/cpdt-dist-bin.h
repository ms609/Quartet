#ifndef CPDT_DIST_BIN_H_
#define CPDT_DIST_BIN_H_

#include <vector>
#include <algorithm>

#include "bit.h"
#include "tree.h"
#include "treenode.h"
#include "utils.h"

typedef unsigned long long ull;

namespace cpdt_dist_bin {

struct cdp_node_t {
	int id;
	cdp_node_t* parent;
	int pos_in_parent;
	int size;
	ull tot_reds;
	ull* reds;
	ull* reds2;

	cdp_node_t** marked;
	int marked_size;

	cdp_node_t(int id) {
		this->id = id;
		parent = NULL;
		pos_in_parent = 1;
		size = 0;
		tot_reds = 0;
		reds = reds2 = NULL;
		marked = NULL;
		marked_size = 0;
	}
	
	~cdp_node_t() {
	  delete[] reds;
	  delete[] reds2;
	  delete[] marked;
	}
};
bool cdp_node_cmp(const cdp_node_t* n1, const cdp_node_t* n2) {
	return n1->id > n2->id;
}

std::vector<cdp_node_t*> cdp;
std::vector<cdp_node_t*> leaf_to_cdp;

std::vector<int> t1_leaves;
std::vector<int> node_range_begin, node_range_end;

ull good_triplets = 0;
ull sol = 0;

cdp_node_t* get_cdp_node() {
	int cdp_id = cdp.size();
	cdp.push_back(new cdp_node_t(cdp_id));
	return cdp[cdp_id];
}
void resize_bits(cdp_node_t* cdp_node) {
	cdp_node->size++;

	cdp_node->reds = new ull[cdp_node->size];
	std::fill(cdp_node->reds, cdp_node->reds+cdp_node->size, 0);

	cdp_node->reds2 = new ull[cdp_node->size];
	std::fill(cdp_node->reds2, cdp_node->reds2+cdp_node->size, 0);
}
cdp_node_t* build_cdp(tree_node* t_node) {
	cdp_node_t* cdp_node = get_cdp_node();

	tree_node* curr = t_node;
	if (curr->is_leaf()) {
		leaf_to_cdp[curr->get_taxa()] = cdp_node;
		return cdp_node;
	}

	while (true) {
		if (curr->get_left()->is_leaf()) {
			assert(curr->get_right()->is_leaf());

			cdp_node_t* subtree_id = get_cdp_node();
			cdp_node_t* leaf1_id = get_cdp_node(), * leaf2_id = get_cdp_node();

			subtree_id->parent = cdp_node;
			subtree_id->pos_in_parent = cdp_node->size + 1;
			cdp_node->size++;

			subtree_id->size++;
			leaf1_id->parent = subtree_id;
			leaf_to_cdp[curr->get_left()->get_taxa()] = leaf1_id;

			subtree_id->size++;
			leaf2_id->parent = subtree_id;
			leaf2_id->pos_in_parent = 2;
			leaf_to_cdp[curr->get_right()->get_taxa()] = leaf2_id;

			resize_bits(subtree_id);
			resize_bits(leaf1_id);
			resize_bits(leaf2_id);

			break;
		}

		cdp_node_t* subtree_id = build_cdp(curr->get_right());
		subtree_id->pos_in_parent = cdp_node->size + 1;
		cdp_node->size++;
		subtree_id->parent = cdp_node;
		curr = curr->get_left();
	}

	resize_bits(cdp_node);

	return cdp_node;
}

void track_t1_leaves(tree_node* node) {
	node_range_begin[node->get_id()] = t1_leaves.size();
	if (node->is_leaf()) {
		t1_leaves.push_back(node->get_taxa());
	} else {
		track_t1_leaves(node->get_child(0));
		track_t1_leaves(node->get_child(1));
	}
	node_range_end[node->get_id()] = t1_leaves.size();
}

void mark_leaf(cdp_node_t* leaf) {
	cdp_node_t* cdp_child = leaf;
	cdp_node_t* cdp_node = cdp_child->parent;
	while (cdp_node != NULL) {
		cdp_node->marked[cdp_node->marked_size++] = cdp_child;
		if (cdp_node->marked_size > 1) break;
		cdp_child = cdp_node;
		cdp_node = cdp_child->parent;
	}
}
void mark_subtree(tree_node* node) {
	int begin = node_range_begin[node->get_id()],
		end = node_range_end[node->get_id()];
	for (int i = begin; i < end; i++) {
		mark_leaf(leaf_to_cdp[t1_leaves[i]]);
	}
}

void color_node_red(cdp_node_t* cdp_node, ull reds) {
	cdp_node_t* cdp_child = cdp_node;
	cdp_node = cdp_child->parent;

	if (cdp_node != NULL) {
		if (cdp_child->pos_in_parent < cdp_node->size-1) {
			update_bit(cdp_node->reds, cdp_node->size, cdp_child->pos_in_parent, reds);
		}
		if (cdp_child->pos_in_parent > 1) {
			update_bit(cdp_node->reds2, cdp_node->size, cdp_child->pos_in_parent, comb2(reds));
		}
	}
	cdp_child->tot_reds += reds;
}

void color_node_blue(cdp_node_t* cdp_node, ull blues, ull blues_down) {
	cdp_node_t* cdp_child = cdp_node;
	cdp_node = cdp_child->parent;
	if (cdp_node != NULL) {
		ull red_down = read_prefix_bit(cdp_node->reds, cdp_child->pos_in_parent-1);

		good_triplets += comb2(blues) * (cdp_node->tot_reds - cdp_child->tot_reds);
		good_triplets += blues * (blues_down * (cdp_node->tot_reds - red_down - cdp_child->tot_reds));
		good_triplets += blues * comb2(red_down);
		if (cdp_child->pos_in_parent < cdp_node->size-1) {
			good_triplets += blues * read_bit(cdp_node->reds2, cdp_child->pos_in_parent+1, cdp_node->size-1);
		}
	}
}

void decolor_node_red(cdp_node_t* cdp_node, ull reds) {
	cdp_node_t* cdp_child = cdp_node;
	cdp_node = cdp_child->parent;
	if (cdp_node != NULL) {
		if (cdp_child->pos_in_parent < cdp_node->size-1) {
			update_bit(cdp_node->reds, cdp_node->size, cdp_child->pos_in_parent, -reds);
		}
		if (cdp_child->pos_in_parent > 1) {
			ull prev_red = cdp_child->tot_reds;
			update_bit(cdp_node->reds2, cdp_node->size, cdp_child->pos_in_parent, -(comb2(prev_red)-comb2(prev_red-reds)));
		}
	}
	cdp_child->tot_reds -= reds;
}

ull color_blue_marked_nodes(cdp_node_t* cdp_node, ull blues_down=0) {
	if (cdp_node->marked_size == 0) { // leaf
		color_node_blue(cdp_node, 1, blues_down);
		return 1;
	}

	if (cdp_node->marked_size > 1)
		std::sort(cdp_node->marked, cdp_node->marked+cdp_node->marked_size, cdp_node_cmp);

	ull tot = 0;
	for (int i = 0; i < cdp_node->marked_size; i++) {
		tot += color_blue_marked_nodes(cdp_node->marked[i], tot);
	}

	color_node_blue(cdp_node, tot, blues_down);
	cdp_node->marked_size = 0;

	return tot;
}
ull color_red_marked_nodes(cdp_node_t* cdp_node) {
	if (cdp_node->marked_size == 0) { // leaf
		color_node_red(cdp_node, 1);
		return 1;
	}

	ull tot = 0;
	for (int i = 0; i < cdp_node->marked_size; i++) {
		tot += color_red_marked_nodes(cdp_node->marked[i]);
	}

	color_node_red(cdp_node, tot);

	cdp_node->marked_size = 0;
	return tot;
}

void decolor_red_leaf(int leaf) {
	cdp_node_t* cdp_child = leaf_to_cdp[leaf];
	cdp_node_t* cdp_node = cdp_child->parent;
	while (cdp_node != NULL) {
		ull prev_red = cdp_child->tot_reds--;
		if (cdp_child->pos_in_parent < cdp_node->size-1) {
			update_bit(cdp_node->reds, cdp_node->size, cdp_child->pos_in_parent, -1);
		}
		if (cdp_child->pos_in_parent > 1) {
			update_bit(cdp_node->reds2, cdp_node->size, cdp_child->pos_in_parent, -(comb2(prev_red)-comb2(prev_red-1)));
		}
		cdp_child = cdp_node;
		cdp_node = cdp_node->parent;
	}
	cdp_child->tot_reds--;
}

ull decolor_red_marked_nodes(cdp_node_t* cdp_node) {
	if (cdp_node->marked_size == 0) { // leaf
		decolor_node_red(cdp_node, 1);
		return 1;
	}

	ull tot = 0;
	for (int i = 0; i < cdp_node->marked_size; i++) {
		tot += decolor_red_marked_nodes(cdp_node->marked[i]);
	}

	decolor_node_red(cdp_node, tot);

	return tot;
}

void leaves_coloring(tree_node* v, bool decolor) {
	if (v->get_child(0)->is_leaf()) {
		decolor_red_leaf(v->get_child(0)->get_taxa());
		decolor_red_leaf(v->get_child(1)->get_taxa());
		return;
	}

	mark_subtree(v->get_child(1));
	if (decolor) {
		decolor_red_marked_nodes(cdp[0]);
	}
	color_blue_marked_nodes(cdp[0]);
	sol += good_triplets;
	good_triplets = 0;

	leaves_coloring(v->get_child(0), true);

	if (!v->get_child(1)->is_leaf() && !v->get_child(1)->get_child(0)->is_leaf()) {
		mark_subtree(v->get_child(1)->get_child(0));
		color_red_marked_nodes(cdp[0]);
		leaves_coloring(v->get_child(1), false);
	}
}

ull triplet_distance(tree* t1, tree* t2) {
	t2->make_biggest_subtree_first();
	leaf_to_cdp.resize(t2->get_leaves_num());
	build_cdp(t2->get_root());

	for (size_t i = 0; i < cdp.size(); i++) {
		cdp[i]->marked = new cdp_node_t*[cdp[i]->size];
	}

	for (size_t i = 1; i < cdp.size(); i++) {
		cdp[i]->pos_in_parent = cdp[i]->parent->size - cdp[i]->pos_in_parent;
	}

	node_range_begin.resize(t1->get_nodes_num());
	node_range_end.resize(t1->get_nodes_num());
	track_t1_leaves(t1->get_root());

	t1->make_biggest_subtree_first();
	mark_subtree(t1->get_root()->get_left());
	color_red_marked_nodes(cdp[0]);
	leaves_coloring(t1->get_root(), false);
	delete[] cdp;
	delete[] leaf_to_cdp;
	delete[] leaves;
	delete[] leaves_count;
	delete[] node_range_begin;
	delete[] node_range_end;

	return comb3(t1->get_leaves_num()) - sol;
}

}


#endif
