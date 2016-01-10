#include <stdio.h>
#include <string.h>

struct dawg_node {
	int eoc:1;
	int eow:1;
	char c:8;
	int childIdx:22;
} __attribute__((packed));

main() {
	unsigned int i=0;
	int k=1;
	struct dawg_node node;
	int node_raw, node_swap;
	for(i=0;k>0;i++) {
		struct dawg_node node;
		k=read(0, &node_raw, 4);
		node_swap = ntohl(node_raw);
		memcpy(&node, &node_swap, 4);

		//printf("%4x NODE %u { childIdx: %u, char: '%c', eow: '%u', eoc: '%u' }\n", node_raw, i, (int) node.childIdx, (char) node.c, (int) node.eow, (int) node.eoc);
	}
		printf("%08x %08x NODE %u { childIdx: %u, char: '%c', eow: '%u', eoc: '%u' }\n", node_raw, node_swap, i, (int) node.childIdx, (char) node.c, (int) node.eow, (int) node.eoc);
}
