#ifndef CPDT_DIST_H_
#define CPDT_DIST_H_

#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#ifdef GOOGLE_HASH
#include <sparsehash/dense_hash_map>
typedef google::dense_hash_map<int, int> hashmap;
#else
#include <unordered_map>
typedef std::unordered_map<int, int> hashmap;
#endif

#include "bit.h"
#include "tree.h"
#include "treenode.h"
#include "utils.h"

typedef unsigned int uint;
typedef unsigned long long ull;

namespace cpdt_dist {

struct cdp_node_t {
	int id;
	cdp_node_t* parent;
	int pos_in_parent;
	size_t size;
	bool singlenode;
	ull tot_reds;
	ull* reds;
	ull* reds2;
	ull* reds2p;

	size_t Cnum;
	int* Cused;
	hashmap* Cindices;
	ull Cstar;
	ull* C;
	ull C2;
	ull C2pstar;
	ull* C2p;
	ull SSstar;
	ull* SS;
	ull CC;

	cdp_node_t** marked;
	size_t marked_size;

	cdp_node_t(int id) {
		this->id = id;
		parent = NULL;
		pos_in_parent = 1;
		size = 0;
		singlenode = false;

		reds = reds2 = reds2p = NULL;
		tot_reds = 0;

		Cnum = 0;
		Cindices = NULL;

		C = C2p = SS = NULL;
		Cused = NULL;
		Cstar = C2pstar = SSstar = 0;
		C2 = CC = 0;
		marked = NULL;
		marked_size = 0;
	}
};
bool cdp_node_cmp(const cdp_node_t* n1, const cdp_node_t* n2) {
	return n1->id > n2->id;
}

std::vector<cdp_node_t*> cdp;
std::vector<cdp_node_t*> leaf_to_cdp;

std::vector<int> t1_leaves;
std::vector<int> node_range_begin, node_range_end;
std::vector<int> t2_leaves_count;

int nonred_colors;
ull good_triplets = 0;
ull sol = 0;

cdp_node_t* build_cdp_supp(tree_node* t_node);

cdp_node_t* get_cdp_node() {
	int cdp_id = cdp.size();
	cdp.push_back(new cdp_node_t(cdp_id));
	return cdp[cdp_id];
}
void resize_bits(cdp_node_t* cdp_node, int leaves) {
	cdp_node->size++;

	cdp_node->reds = new ull[cdp_node->size];
	std::fill(cdp_node->reds, cdp_node->reds+cdp_node->size, 0);

	cdp_node->Cused = new int[std::min(nonred_colors, leaves)];
	cdp_node->C = new ull[std::min(nonred_colors, leaves)];
#ifdef GOOGLE_HASH
	cdp_node->Cindices = new hashmap;
	cdp_node->Cindices->set_empty_key(-1);
	cdp_node->Cindices->set_deleted_key(-2);
#else
	cdp_node->Cindices = new hashmap;
#endif

	if (cdp_node->singlenode) {
		cdp_node->reds2 = new ull[cdp_node->size];
		std::fill(cdp_node->reds2, cdp_node->reds2+cdp_node->size, 0);

		cdp_node->C2p = new ull[std::min(nonred_colors, leaves)];
		cdp_node->SS = new ull[std::min(nonred_colors, leaves)];
	} else {
		cdp_node->reds2p = new ull[cdp_node->size];
		std::fill(cdp_node->reds2p, cdp_node->reds2p+cdp_node->size, 0);
	}
}

cdp_node_t* build_sn_node(tree_node* t_node) {
	cdp_node_t* cdp_node = get_cdp_node();
	cdp_node->singlenode = true;

	for (int i = !t_node->get_child(0)->is_leaf(); i < t_node->get_num_children(); i++) {
		cdp_node_t* subtree = build_cdp_supp(t_node->get_child(i));
		subtree->parent = cdp_node;
		subtree->pos_in_parent = cdp_node->size + 1;
		cdp_node->size++;
	}
	resize_bits(cdp_node, t2_leaves_count[t_node->get_id()]);

	return cdp_node;
}
cdp_node_t* build_cdp_supp(tree_node* t_node) {
	cdp_node_t* cdp_node = get_cdp_node();

	tree_node* curr = t_node;
	if (curr->is_leaf()) {
		leaf_to_cdp[curr->get_taxa()] = cdp_node;
		cdp_node->C = new ull[1]; cdp_node->C[0] = 1;
		cdp_node->Cstar = 1;
		cdp_node->Cnum = 1;
		cdp_node->Cused = new int[1];
		return cdp_node;
	}

	while (!curr->get_child(0)->is_leaf()) {
		cdp_node_t* node = curr->get_num_children() > 2 ?
				build_sn_node(curr) : build_cdp_supp(curr->get_child(1));
		node->pos_in_parent = cdp_node->size + 1;
		cdp_node->size++;
		node->parent = cdp_node;
		curr = curr->get_child(0);
	}

	if (curr->get_num_children() > 2) {
		cdp_node_t* node = build_sn_node(curr);
		node->pos_in_parent = cdp_node->size + 1;
		cdp_node->size++;
		node->parent = cdp_node;
	} else {
		cdp_node_t* subtree = get_cdp_node();
		cdp_node_t* leaf1 = build_cdp_supp(curr->get_child(0)),
				  * leaf2 = build_cdp_supp(curr->get_child(1));

		subtree->parent = cdp_node;
		subtree->pos_in_parent = cdp_node->size + 1;
		cdp_node->size++;

		subtree->size++;
		leaf1->parent = subtree;

		subtree->size++;
		leaf2->parent = subtree;
		leaf2->pos_in_parent = 2;

		resize_bits(subtree, 2);
	}

	resize_bits(cdp_node, t2_leaves_count[t_node->get_id()]);

	return cdp_node;
}
cdp_node_t* build_cdp(tree* t) {
	t->make_biggest_subtree_first();

	count_leaves(t, t2_leaves_count);

	leaf_to_cdp.resize(t->get_leaves_num());
	cdp_node_t* cdp_root = build_cdp_supp(t->get_root());

	cdp[0]->marked = new cdp_node_t*[cdp[0]->size];
	for (size_t i = 1; i < cdp.size(); i++) {
		cdp[i]->marked = new cdp_node_t*[cdp[i]->size];
		cdp[i]->pos_in_parent = cdp[i]->parent->size - cdp[i]->pos_in_parent;
	}

	return cdp_root;
}

void mark_leaf(cdp_node_t* leaf, int color) {
	if (color > 0) {
		leaf->Cused[0] = color;
	}
	cdp_node_t* cdp_child = leaf;
	cdp_node_t* cdp_node = cdp_child->parent;
	while (cdp_node != NULL) {
		cdp_node->marked[cdp_node->marked_size++] = cdp_child;
		if (cdp_node->marked_size > 1) return;
		cdp_child = cdp_node;
		cdp_node = cdp_child->parent;
	}
}
void mark_subtree(tree_node* node, int color) {
	int begin = node_range_begin[node->get_id()],
		end = node_range_end[node->get_id()];
	for (int i = begin; i < end; i++) {
		mark_leaf(leaf_to_cdp[t1_leaves[i]], color);
	}
}

void color_node_red(cdp_node_t* cdp_node, int reds) {
	cdp_node_t* cdp_child = cdp_node;
	cdp_node = cdp_child->parent;
	if (cdp_node != NULL) {
		if (ull(cdp_child->pos_in_parent) < cdp_node->size-1) {
			update_bit(cdp_node->reds, cdp_node->size, cdp_child->pos_in_parent, reds);
		}

		if (!cdp_child->singlenode) {
			update_bit(cdp_node->singlenode ? cdp_node->reds2 : cdp_node->reds2p,
					cdp_node->size, cdp_child->pos_in_parent, comb2(reds));
		} else {
			ull reds2p = read_prefix_bit(cdp_child->reds2, cdp_child->size-1);
			set_single(cdp_node->reds2p, cdp_node->size, cdp_child->pos_in_parent, reds2p);
		}
	}
	cdp_child->tot_reds += reds;
}

void color_snnode_blue(cdp_node_t* ui) {
	cdp_node_t* u = ui->parent;

	ull reds_down = read_prefix_bit(u->reds, ui->pos_in_parent-1);
	ull reds2_down = read_prefix_bit(u->reds2, ui->pos_in_parent-1);
	ull reds2sum = read_prefix_bit(u->reds2, u->size-1);

	for (size_t i = 0; i < ui->Cnum; i++) {
		ull Ca = ui->C[i];
		int color = ui->Cused[i];

		hashmap::const_iterator it = u->Cindices->find(color);
		ull uCi, uC2pi, uSSi;
		if (it != u->Cindices->end()) {
			int ucol = it->second;
			uCi = u->C[ucol];
			uC2pi = u->C2p[ucol];
			uSSi = u->SS[ucol];
		} else {
			uCi = uC2pi = uSSi = 0;
		}

		// Lemma 1.3
		good_triplets += Ca * (reds2sum - comb2(ui->tot_reds));
		good_triplets += Ca * (u->C2pstar-uC2pi);
		good_triplets += comb2(Ca) * (u->tot_reds-ui->tot_reds);
		good_triplets += comb2(Ca) * (u->Cstar-uCi);

		// Lemma 1.5
		good_triplets += Ca * comb2(u->Cstar-uCi + reds_down);
		good_triplets -= Ca * (u->C2-comb2(uCi) + comb2(reds_down));
		good_triplets -= Ca * (u->SSstar + reds2_down - uSSi);
		good_triplets += Ca * (reds2_down + u->C2pstar-uC2pi);
		good_triplets += Ca * ((u->Cstar-uCi)*(u->tot_reds-ui->tot_reds-reds_down));
	}
}

void color_cpnode_blue(cdp_node_t* vj) {
	cdp_node_t* vi = vj->parent;
	cdp_node_t* v = vi->parent;

	ull reds_down = read_prefix_bit(v->reds, vi->pos_in_parent-1);
	ull redsj_down = read_prefix_bit(vi->reds, vj->pos_in_parent-1);
	ull reds2p_up = ull(vi->pos_in_parent) < v->size-1 ? read_bit(v->reds2p, vi->pos_in_parent+1, v->size-1) : 0;

	for (size_t i = 0; i < vj->Cnum; i++) {
		ull Ca = vj->C[i];
		int color = vj->Cused[i];

		hashmap::const_iterator it = vi->Cindices->find(color);
		ull viCi = it != vi->Cindices->end() ? vi->C[it->second] : 0;

		it = v->Cindices->find(color);
		ull vCi = it != v->Cindices->end() ? v->C[it->second] : 0;

		// Lemma 1.2
		good_triplets += Ca * comb2(reds_down);
		good_triplets += Ca * (v->C2-comb2(vCi));
		good_triplets += Ca * reds2p_up;
		good_triplets += (Ca * (vCi+viCi) + comb2(Ca)) * (v->tot_reds-vi->tot_reds-reds_down);
		good_triplets += comb2(Ca) * reds_down;
		good_triplets += comb2(Ca) * (v->Cstar-vCi);

		// Lemma 1.4
		good_triplets += Ca * (v->Cstar-vCi+reds_down) * (vi->Cstar-viCi+redsj_down);
		good_triplets -= Ca * (vi->CC-(viCi*vCi)+(reds_down*redsj_down));
		good_triplets += Ca * (v->Cstar-vCi) * (vi->tot_reds-vj->tot_reds-redsj_down);
	}
}

void color_cptocp_node_blue(cdp_node_t* vj) {
	cdp_node_t* v = vj->parent;

	ull reds_down = read_prefix_bit(v->reds, vj->pos_in_parent-1);
	// note that in this case (cp -> cp), reds2p is used as reds2
	ull reds2p_up = ull(vj->pos_in_parent) < v->size-1 ? read_bit(v->reds2p, vj->pos_in_parent+1, v->size-1) : 0;

	for (size_t i = 0; i < vj->Cnum; i++) {
		ull Ca = vj->C[i];
		int color = vj->Cused[i];

		hashmap::const_iterator it = v->Cindices->find(color);
		ull vCi = it != v->Cindices->end() ? v->C[it->second] : 0;

		// Lemma 1.2 adapted
		good_triplets += Ca * comb2(reds_down);
		good_triplets += Ca * (v->C2-comb2(vCi));
		good_triplets += Ca * reds2p_up;
		good_triplets += (Ca*vCi + comb2(Ca)) * (v->tot_reds-vj->tot_reds-reds_down);
		good_triplets += comb2(Ca) * reds_down;
		good_triplets += comb2(Ca) * (v->Cstar-vCi);
	}
}

int get_sncolor_id(cdp_node_t* node, int color) {
	std::pair<hashmap::const_iterator, bool> p = node->Cindices->insert(hashmap::value_type(color, node->Cnum));
	int parcolid = p.first->second;
	if (p.second) {
		node->Cused[node->Cnum++] = color;

		node->C[parcolid] = 0;
		node->C2p[parcolid] = 0;
		node->SS[parcolid] = 0;
	}
	return parcolid;
}
int get_cpcolor_id(cdp_node_t* node, int color) {
	std::pair<hashmap::const_iterator, bool> p = node->Cindices->insert(hashmap::value_type(color, node->Cnum));
	int parcolid = p.first->second;
	if (p.second) {
		node->Cused[node->Cnum++] = color;
		node->C[parcolid] = 0;
	}
	return parcolid;
}

void update_snparent(cdp_node_t* node) {
	cdp_node_t* node_parent = node->parent;
	for (size_t i = 0; i < node->Cnum; i++) {
		int parcolid = get_sncolor_id(node->parent, node->Cused[i]);
		ull Ci = node->C[i];
		node_parent->C2 += comb2(node_parent->C[parcolid] + Ci) - comb2(node_parent->C[parcolid]);
		node_parent->Cstar += Ci;
		node_parent->C[parcolid] += Ci;
		node_parent->C2pstar += comb2(Ci);
		node_parent->C2p[parcolid] += comb2(Ci);

		hashmap::const_iterator it = node_parent->parent->Cindices->find(node->Cused[i]);
		if (it != node_parent->parent->Cindices->end()) {
			node_parent->CC += node_parent->parent->C[it->second] * Ci;
		}
		node_parent->SS[parcolid] += Ci*(node->Cstar-Ci+node->tot_reds) + comb2(Ci);
	}
	node_parent->SSstar += comb2(node->Cstar) + node->Cstar*node->tot_reds;
}
void update_cpparent(cdp_node_t* node) {
	cdp_node_t* parent = node->parent;
	for (size_t i = 0; i < node->Cnum; i++) {
		int parcolid = get_cpcolor_id(parent, node->Cused[i]);
		parent->C2 += comb2(parent->C[parcolid] + node->C[i]) - comb2(parent->C[parcolid]);
		parent->Cstar += node->C[i];
		parent->C[parcolid] += node->C[i];
	}
}

void reset_snnode(cdp_node_t* node) {
	node->C2 = 0;
	node->CC = 0;
	node->Cstar = 0;
	node->C2pstar = 0;
	node->SSstar = 0;
	node->Cnum = 0;
	node->Cindices->clear();
	node->marked_size = 0;
}
void reset_cpnode(cdp_node_t* node) {
	node->C2 = 0;
	node->Cstar = 0;
	node->Cnum = 0;
	node->Cindices->clear();
	node->marked_size = 0;
}

void color_blue_marked_nodes(cdp_node_t* cdp_node) {
	if (cdp_node->marked_size > 1) {
		std::sort(cdp_node->marked, cdp_node->marked+cdp_node->marked_size, cdp_node_cmp);
	}
	for (ull i = 0; i < cdp_node->marked_size; i++) {
		color_blue_marked_nodes(cdp_node->marked[i]);
	}

	if (cdp_node->parent != NULL) {
		if (!cdp_node->singlenode) {
			if (cdp_node->parent->singlenode) {
				color_snnode_blue(cdp_node);
				color_cpnode_blue(cdp_node);
				update_snparent(cdp_node);
			} else {
				color_cptocp_node_blue(cdp_node);
				update_cpparent(cdp_node);
			}
		} else {
			update_cpparent(cdp_node);
		}
	}

	if (!cdp_node->singlenode) {
		if (cdp_node->marked_size > 0) {
			reset_cpnode(cdp_node);
		}
	} else {
		reset_snnode(cdp_node);
	}
}
int color_red_marked_nodes(cdp_node_t* cdp_node) {
	if (cdp_node->marked_size == 0) { // leaf
		color_node_red(cdp_node, 1);
		return 1;
	}


	std::sort(cdp_node->marked, cdp_node->marked+cdp_node->marked_size, cdp_node_cmp);
	int tot = 0;
	for (ull i = 0; i < cdp_node->marked_size; i++) {
		tot += color_red_marked_nodes(cdp_node->marked[i]);
	}

	color_node_red(cdp_node, tot);

	cdp_node->marked_size = 0;
	return tot;
}

void decolor_node_red(cdp_node_t* cdp_node, int reds) {
	cdp_node_t* cdp_child = cdp_node;
	cdp_node = cdp_child->parent;
	if (cdp_node != NULL) {
		if (ull(cdp_child->pos_in_parent) < cdp_node->size-1) {
			update_bit(cdp_node->reds, cdp_node->size, cdp_child->pos_in_parent, -reds);
		}

		if (!cdp_child->singlenode) {
			ull prev_red = cdp_child->tot_reds;
			update_bit(cdp_node->singlenode ? cdp_node->reds2 : cdp_node->reds2p,
					cdp_node->size, cdp_child->pos_in_parent, -(comb2(prev_red)-comb2(prev_red-reds)));
		} else {
			ull reds2p = read_prefix_bit(cdp_child->reds2, cdp_child->size-1);
			set_single(cdp_node->reds2p, cdp_node->size, cdp_child->pos_in_parent, reds2p);
		}
	}
	cdp_child->tot_reds -= reds;
}
void decolor_red_leaf(int leaf) {
	cdp_node_t* cdp_child = leaf_to_cdp[leaf];
	cdp_node_t* cdp_node = cdp_child->parent;
	while (cdp_node != NULL) {
		ull prev_red = cdp_child->tot_reds--;
		if (ull(cdp_child->pos_in_parent) < cdp_node->size-1) {
			update_bit(cdp_node->reds, cdp_node->size, cdp_child->pos_in_parent, -1);
		}

		if (!cdp_child->singlenode) {
			update_bit(cdp_node->singlenode ? cdp_node->reds2 : cdp_node->reds2p,
					cdp_node->size, cdp_child->pos_in_parent, -(comb2(prev_red)-comb2(prev_red-1)));
		} else {
			ull reds2p = read_prefix_bit(cdp_child->reds2, cdp_child->size-1);
			set_single(cdp_node->reds2p, cdp_node->size, cdp_child->pos_in_parent, reds2p);
		}

		cdp_child = cdp_node;
		cdp_node = cdp_node->parent;
	}
	cdp_child->tot_reds--;
}

int decolor_red_marked_nodes(cdp_node_t* cdp_node) {
	if (cdp_node->marked_size == 0) { // leaf
		decolor_node_red(cdp_node, 1);
		return 1;
	}

	std::sort(cdp_node->marked, cdp_node->marked+cdp_node->marked_size, cdp_node_cmp);
	int tot = 0;
	for (ull i = 0; i < cdp_node->marked_size; i++) {
		tot += decolor_red_marked_nodes(cdp_node->marked[i]);
	}

	decolor_node_red(cdp_node, tot);
	return tot;
}

void leaves_coloring(tree_node* v, bool decolor) {
	if (v->is_leaf()) {
		decolor_red_leaf(v->get_taxa());
		return;
	} /*else if (v->get_num_children() == 2 && v->get_child(0)->is_leaf() && v->get_child(1)->is_leaf()) {
		// if there are only two leaves in the current subtree then it is useless to color it
		decolor_red_leaf(v->get_child(0)->get_taxa());
		decolor_red_leaf(v->get_child(1)->get_taxa());
		return;
	}*/

	for (int i = 1; i < v->get_num_children(); i++) {
		mark_subtree(v->get_child(i), i);
	}
	if (decolor) {
		decolor_red_marked_nodes(cdp[0]);
	}
	color_blue_marked_nodes(cdp[0]);

	sol += good_triplets;
	good_triplets = 0;

	leaves_coloring(v->get_child(0), true);

	for (int i = 1; i < v->get_num_children(); i++) {
		if (!v->get_child(i)->is_leaf()) {
			mark_subtree(v->get_child(i)->get_child(0), 0);
			color_red_marked_nodes(cdp[0]);
			leaves_coloring(v->get_child(i), false);
		}
	}
}

ull triplet_distance(tree* t1, tree* t2) {
	// the number of colors is the highest degree in T1
	for (ull i = 0; i < t1->get_nodes_num(); i++) {
		nonred_colors = std::max(nonred_colors, t1->get_node(i)->get_num_children()-1);
	}
	std::cout << "COLORS: " << nonred_colors << std::endl;

	// builds cdp
	build_cdp(t2);

	t1->make_biggest_subtree_first();

	// lists the leaves in left to right order, and associates to each internal node
	// a range of leaves
	track_leaves(t1, t1_leaves, node_range_begin, node_range_end);

	// colors the whole tree red, to initialize the invariant
	mark_subtree(t1->get_root()->get_child(0), 0);
	color_red_marked_nodes(cdp[0]);

	// main algorithm
	leaves_coloring(t1->get_root(), false);

	return comb3(t1->get_leaves_num()) - sol;
}

}


#endif
