#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct dawg_node {
	int eow:1;
	int eoc:1;
	char c:8;
	int childIdx:22;
} __attribute__((packed));


void dumpNode(int i, struct dawg_node* node) {
		printf("NODE %u { childIdx: %u, char: '%c', eow: '%u', eoc: '%u' }\n", i, (int) node->childIdx, (char) node->c, (int) 1&node->eow, (int) 1&node->eoc);
}


/* pushNode:
 * 	rv=push_node children
 *	write_all rv
 *	return me
*/

struct dawg_node buildNode(struct dawg_node srca, struct dawg_node srcb) {
	
}


/*
writeNode:
	writeGrandchildren
	writeChildren
	writeNode
*/

int main(int argc, char* argv[]) {
	unsigned int i=0;
	int k=1;
	struct dawg_node *nodes;
	int* mem=malloc(100000000);
	int size=read(0, mem, 100000000);
	int n=size/4;
	for(i=0;i<n;i++)
		mem[i]=ntohl(mem[i]);

	nodes=&((struct dawg_node*) mem)[2];

	dumpNode(n-2, &nodes[n-4]);
	k=nodes[n-4].childIdx-1;
	do {
		k++;
		dumpNode(k, &nodes[k]);
	} while(!(nodes[k].eoc));
	
}
