#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Template ====================================================================

#define array_init(arr, type)                                                  \
	do {                                                                       \
		(arr).data = calloc(8, sizeof(type));                                  \
		if ((arr).data == NULL) {                                              \
			fprintf(stderr, "Out of memory.\n");                               \
			exit(1);                                                           \
		}                                                                      \
		(arr).size = 0;                                                        \
		(arr).cap = 8;                                                         \
	} while (0)

#define array_deinit(arr)                                                      \
	do {                                                                       \
		free((arr).data);                                                      \
		(arr).size = 0;                                                        \
		(arr).cap = 0;                                                         \
	} while (0)

#define array_push(arr, type, item)                                            \
	do {                                                                       \
		(arr).data[(arr).size++] = (item);                                     \
		if ((arr).size == (arr).cap) {                                         \
			(arr).cap *= 2;                                                    \
			(arr).data = realloc((arr).data, (arr).cap * sizeof(type));        \
			if ((arr).data == NULL) {                                          \
				fprintf(stderr, "Out of memory.\n");                           \
				exit(1);                                                       \
			}                                                                  \
		}                                                                      \
	} while (0)

void fclose_wrap(FILE **f) { fclose(*f); }

char *read_file(char const *filename)
{
	[[gnu::cleanup(fclose_wrap)]] FILE *f = fopen(filename, "rb");
	if (f == NULL) {
		fprintf(stderr, "Could not open \"%s\": %s\n", filename,
				strerror(errno));
		exit(1);
	}
	fseek(f, 0L, SEEK_END);
	long length = ftell(f);
	rewind(f);
	char *buf = (char *)malloc(length * sizeof(char) + 1);
	if (buf == NULL) {
		fprintf(stderr, "Could not allocate buffer: %s\n", strerror(errno));
		exit(1);
	}
	long read = fread(buf, sizeof(char), length, f);
	if (read < length) {
		fprintf(stderr, "Could not read file \"%s\"\n", filename);
		exit(1);
	}
	buf[read] = '\0';
	return buf;
}

// Data structures =============================================================

typedef enum {
	OP_ADV = 0,
	OP_BXL = 1,
	OP_BST = 2,
	OP_JNZ = 3,
	OP_BXC = 4,
	OP_OUT = 5,
	OP_BDV = 6,
	OP_CDV = 7,
} opcode_t;

typedef enum {
	COMB_0 = 0,
	COMB_1 = 1,
	COMB_2 = 2,
	COMB_3 = 3,
	COMB_A = 4,
	COMB_B = 5,
	COMB_C = 6,
	COMB_UNUSED = 7,
} combo_t;

typedef struct vm_s {
	char *data;
	long size;
	long cap;
	long ip;
	long ra;
	long rb;
	long rc;
} vm_t;

void vm_deinit(vm_t *v)
{
	array_deinit(*v);
	v->ip = 0;
	v->ra = 0;
	v->rb = 0;
	v->rc = 0;
}

typedef struct output_s {
	char *data;
	long size;
	long cap;
} output_t;

void output_deinit(output_t *o) { array_deinit(*o); }

// Main program ================================================================

static inline bool is_digit(char c) { return c >= '0' && c <= '9'; }

// My parsing gets lazier by the day.
void parse_data(vm_t *out, char *src)
{
	out->size = 0;
	int numbers_parsed = 0;
	char *curr = src;
	while (curr[0] != '\0') {
		if (is_digit(curr[0])) {
			if (numbers_parsed == 0) {
				out->ra = strtol(curr, &curr, 10);
				numbers_parsed++;
			} else if (numbers_parsed == 1) {
				out->rb = strtol(curr, &curr, 10);
				numbers_parsed++;
			} else if (numbers_parsed == 2) {
				out->rc = strtol(curr, &curr, 10);
				numbers_parsed++;
			} else {
				int temp = (char)strtol(curr, &curr, 10);
				if (temp < 0 || temp > 7) {
					fprintf(stderr, "Bad format.\n");
					exit(1);
				}
				array_push(*out, char, temp);
				numbers_parsed++;
			}
		} else {
			curr++;
		}
	}
	if (numbers_parsed < 3) {
		fprintf(stderr, "Bad format.\n");
		exit(1);
	}
}

static inline long get_combo(vm_t *vm, char arg)
{
	switch (arg) {
	case COMB_0:
		return 0;
	case COMB_1:
		return 1;
	case COMB_2:
		return 2;
	case COMB_3:
		return 3;
	case COMB_A:
		return vm->ra;
	case COMB_B:
		return vm->rb;
	case COMB_C:
		return vm->rc;
	default:
		fprintf(stderr, "invalid combo argument '%d'\n", arg);
		exit(1);
	}
}

void run(vm_t *vm, output_t *out)
{
	vm->ip = 0;
	out->size = 0;
	while (vm->ip < vm->size) {
		char op = vm->data[vm->ip++];
		char arg = vm->data[vm->ip++];
		long temp = 0;
		switch (op) {
		case OP_ADV:
			vm->ra /= 1 << get_combo(vm, arg);
			break;
		case OP_BXL:
			vm->rb ^= arg;
			break;
		case OP_BST:
			vm->rb = get_combo(vm, arg) & 7;
			break;
		case OP_JNZ:
			if (vm->ra != 0) {
				vm->ip = arg;
			}
			break;
		case OP_BXC:
			vm->rb ^= vm->rc;
			break;
		case OP_OUT:
			temp = get_combo(vm, arg) & 7;
			array_push(*out, char, temp);
			break;
		case OP_BDV:
			vm->rb = vm->ra / (1 << get_combo(vm, arg));
			break;
		case OP_CDV:
			vm->rc = vm->ra / (1 << get_combo(vm, arg));
			break;
		default:
			fprintf(stderr, "invalid opcode '%d'\n", op);
			exit(1);
		}
	}
}

long find_quine(vm_t *vm, output_t *out)
{
	long a = 1;
	while (true) {
		vm->ra = a;
		run(vm, out);
		if (out->size < vm->size) {
			bool suffix = true;
			for (long i = vm->size - out->size, j = 0; i < vm->size; i++, j++) {
				if (!(vm->data[i] == out->data[j])) {
					suffix = false;
					break;
				}
			}
			if (suffix) {
				a <<= 3;
			} else {
				a += 1;
			}
		} else if (out->size == vm->size) {
			if (memcmp(vm->data, out->data, vm->size) == 0) {
				return a;
			} else {
				a++;
			}
		} else {
			return -1;
		}
	}
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s input\n", argv[0]);
		exit(1);
	}
	char *data = read_file(argv[1]);

	[[gnu::cleanup(vm_deinit)]]
	vm_t vm = {0};
	array_init(vm, char);
	parse_data(&vm, data);

	[[gnu::cleanup(output_deinit)]]
	output_t out = {0};
	array_init(out, char);

	run(&vm, &out);

	if (out.size > 0) {
		printf("%d", out.data[0]);
		for (long i = 1; i < out.size; i++) {
			printf(",%d", out.data[i]);
		}
		printf("\n");
	}

	long quine = find_quine(&vm, &out);
	if (quine > -1) {
		printf("Quine at %ld.\n", quine);
	} else {
		printf("No quine.\n");
	}

	free(data);
	return 0;
}
