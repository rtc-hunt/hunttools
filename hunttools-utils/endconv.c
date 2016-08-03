/*
 *	endconv: endianness converter
 *	Simple wrapper to host libc's endian functions
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <endian.h>
#include <libgen.h>

static char *progname;

static uint16_t x16;
static uint32_t x32;
static uint64_t x64;
static size_t x;

static void usage(void)
{
	printf("usage: execve(endconv, \"<endian.h> xxxtoxxx()\", \"[INPUT]\", \"[OUTPUT]\")\n");
	printf("install this program like that: ln endconv htobe16\n");
	exit(1);
}

#define CONV(Z, A) \
	else if (!strcmp(progname, #Z)) { \
		while (1) { \
			if (feof(in) || ferror(in)) break; \
			x = fread(&A, 1, sizeof(A), in); \
			A = Z(A); \
			fwrite(&A, x, 1, out); \
		} \
	}


int main(int argc, char **argv)
{
	FILE *in = stdin, *out = stdout;
	progname = basename(*argv);

	if (*(argv+1)) {
		in = fopen(*(argv+1), "rb");
		if (!in) return 1;
	}

	if (*(argv+1) && *(argv+2)) {
		out = fopen(*(argv+2), "wb");
		if (!out) return 2;
	}

	if (0) {}

/* host to target functions */
	CONV(htobe16, x16)
	CONV(htole16, x16)
	CONV(htobe32, x32)
	CONV(htole32, x32)
	CONV(htobe64, x64)
	CONV(htole64, x64)

/* from target to host functions */
	CONV(be16toh, x16)
	CONV(le16toh, x16)
	CONV(be32toh, x32)
	CONV(le32toh, x32)
	CONV(be64toh, x64)
	CONV(le64toh, x64)

	else usage();

	fclose(in);
	fclose(out);

	return 0;
}
