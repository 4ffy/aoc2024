#include <errno.h>
#include <regex.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define array_init(arr, type)                                                  \
	do {                                                                       \
		arr.data = calloc(8, sizeof(type));                                    \
		if (arr.data == NULL) {                                                \
			fprintf(stderr, "Out of memory.\n");                               \
			exit(1);                                                           \
		}                                                                      \
		arr.size = 0;                                                          \
		arr.cap = 8;                                                           \
	} while (0)

#define array_deinit(arr)                                                      \
	do {                                                                       \
		free(arr.data);                                                        \
		arr.size = 0;                                                          \
		arr.cap = 0;                                                           \
	} while (0)

#define array_push(arr, type, item)                                            \
	do {                                                                       \
		arr.data[arr.size] = item;                                             \
		arr.size++;                                                            \
		if (arr.size == arr.cap) {                                             \
			arr.cap *= 2;                                                      \
			arr.data = realloc(arr.data, arr.cap * sizeof(type));              \
			if (arr.data == NULL) {                                            \
				fprintf(stderr, "Out of memory.\n");                           \
				exit(1);                                                       \
			}                                                                  \
		}                                                                      \
	} while (0)

typedef struct int_array_s int_array_t;
struct int_array_s {
	int *data;
	size_t size;
	size_t cap;
};

typedef struct int_array_array_s int_array_array_t;
struct int_array_array_s {
	int_array_t *data;
	size_t size;
	size_t cap;
};

typedef struct string_array_s string_array_t;
struct string_array_s {
	char const **data;
	size_t size;
	size_t cap;
};

typedef struct node_s node_t;
struct node_s {
	int value;
	int_array_t neighbors;
};

typedef struct graph_s graph_t;
struct graph_s {
	node_t *data;
	size_t size;
	size_t cap;
};

node_t node_new(int value)
{
	node_t node = {0};
	node.value = value;
	array_init(node.neighbors, int);
	return node;
}

void graph_print(graph_t g)
{
	for (size_t i = 0; i < g.size; i++) {
		printf("%d: ", g.data[i].value);
		for (size_t j = 0; j < g.data[i].neighbors.size; j++) {
			printf("%d ", g.data[i].neighbors.data[j]);
		}
		printf("\n");
	}
}

int comp_int(void const *a, void const *b)
{
	int arg1 = *(int const *)a;
	int arg2 = *(int const *)b;
	if (arg1 < arg2)
		return -1;
	if (arg1 > arg2)
		return 1;
	return 0;
}

int comp_node(void const *a, void const *b)
{
	node_t arg1 = *(node_t const *)a;
	node_t arg2 = *(node_t const *)b;
	if (arg1.value < arg2.value)
		return -1;
	if (arg1.value > arg2.value)
		return 1;
	return 0;
}

node_t *graph_find_node(graph_t g, int value)
{
	node_t temp = {.value = value};
	return (node_t *)bsearch(&temp, g.data, g.size, sizeof(node_t), comp_node);
}

bool graph_has_neighbor(graph_t g, int left, int right)
{
	node_t *node = graph_find_node(g, left);
	if (node == NULL) {
		return false;
	}
	int *result = (int *)bsearch(&right, node->neighbors.data,
								 node->neighbors.size, sizeof(int), comp_int);
	return result != NULL;
}

void graph_add_pair(graph_t *g, int left, int right)
{
	if (!graph_find_node(*g, left)) {
		node_t new_node = node_new(left);
		array_push((*g), node_t, new_node);
	}
	if (!graph_find_node(*g, right)) {
		node_t new_node = node_new(right);
		array_push((*g), node_t, new_node);
	}
	qsort(g->data, g->size, sizeof(node_t), comp_node);

	node_t *node = graph_find_node(*g, left);
	array_push(node->neighbors, int, right);
	qsort(node->neighbors.data, node->neighbors.size, sizeof(int), comp_int);
}

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
	size_t length = (size_t)ftell(f);
	rewind(f);

	char *buf = (char *)malloc(length * sizeof(char) + 1);
	if (buf == NULL) {
		fprintf(stderr, "Could not allocate buffer: %s\n", strerror(errno));
		exit(1);
	}

	size_t read = fread(buf, sizeof(char), length, f);
	if (read < length) {
		fprintf(stderr, "Could not read file \"%s\"\n", filename);
		exit(1);
	}

	buf[read] = '\0';
	return buf;
}

string_array_t split_lines(char *src)
{
	string_array_t result = {0};
	array_init(result, char *);
	char *line = strtok(src, "\n");
	while (line != NULL) {
		array_push(result, char *, line);
		line = strtok(NULL, "\n");
	}
	return result;
}

int_array_t parse_rules(string_array_t lines)
{
	[[gnu::cleanup(regfree)]]
	regex_t re = {0};
	regcomp(&re, "\\([[:digit:]]\\{1,\\}\\)|\\([[:digit:]]\\{1,\\}\\)", 0);
	regmatch_t matches[3];
	int_array_t result = {0};
	array_init(result, int);

	for (size_t i = 0; i < lines.size; i++) {
		char const *str = lines.data[i];
		if (!regexec(&re, str, 3, matches, 0)) {
			int left = (int)strtol(str + matches[1].rm_so, NULL, 10);
			int right = (int)strtol(str + matches[2].rm_so, NULL, 10);
			array_push(result, int, left);
			array_push(result, int, right);
		}
	}
	return result;
}

bool has_comma(char const *str, size_t length)
{
	for (size_t i = 0; i < length; i++) {
		if (str[i] == ',') {
			return true;
		}
	}
	return false;
}

int_array_array_t parse_pages(string_array_t lines)
{
	int_array_array_t result = {0};
	array_init(result, int_array_t);

	for (size_t i = 0; i < lines.size; i++) {
		char const *line = lines.data[i];
		size_t length = strlen(line);
		if (has_comma(line, length)) {
			int_array_t current = {0};
			array_init(current, int);

			char *copy = strdup(line);
			if (copy == NULL) {
				fprintf(stderr, "Out of memory.\n");
				exit(1);
			}
			char *number = strtok(copy, ",");
			while (number != NULL) {
				int temp = (int)strtol(number, NULL, 10);
				array_push(current, int, temp);
				number = strtok(NULL, ",");
			}
			free((void *)copy);

			array_push(result, int_array_t, current);
		}
	}
	return result;
}

bool pages_valid(graph_t g, int_array_t pages)
{
	for (size_t i = 0; i < pages.size - 1; i++) {
		for (size_t j = i + 1; j < pages.size; j++) {
			if (!graph_has_neighbor(g, pages.data[i], pages.data[j])) {
				return false;
			}
		}
	}
	return true;
}

int sum_valid(graph_t g, int_array_array_t pages_arr)
{
	int sum = 0;
	for (size_t i = 0; i < pages_arr.size; i++) {
		int_array_t pages = pages_arr.data[i];
		if (pages_valid(g, pages)) {
			sum += pages.data[pages.size / 2];
		}
	}
	return sum;
}

int sum_invalid(graph_t g, int_array_array_t pages_arr)
{
	int sum = 0;
	for (size_t i = 0; i < pages_arr.size; i++) {
		int_array_t pages = pages_arr.data[i];
		if (!pages_valid(g, pages)) {
			int_array_t fixed = {0};
			array_init(fixed, int);
			for (size_t j = 0; j < pages.size; j++) {
				array_push(fixed, int, pages.data[j]);
			}

			// Yeah! Bubble sort!
			size_t n = fixed.size;
			bool done = false;
			while (!done) {
				done = true;
				for (size_t i = 1; i < n; i++) {
					if (!graph_has_neighbor(g, fixed.data[i - 1],
											fixed.data[i])) {
						int temp = fixed.data[i - 1];
						fixed.data[i - 1] = fixed.data[i];
						fixed.data[i] = temp;
						done = false;
					}
				}
				n--;
			}

			sum += fixed.data[fixed.size / 2];
			array_deinit(fixed);
		}
	}
	return sum;
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s input\n", argv[0]);
		exit(1);
	}

	char *data = read_file(argv[1]);

	graph_t graph = {0};
	array_init(graph, node_t);

	string_array_t lines = split_lines(data);
	int_array_t pairs = parse_rules(lines);
	int_array_array_t pages = parse_pages(lines);
	array_deinit(lines);

	for (size_t i = 0; i < pairs.size; i += 2) {
		graph_add_pair(&graph, pairs.data[i], pairs.data[i + 1]);
	}
	array_deinit(pairs);

	// graph_print(graph);

	printf("Sum of valid: %d\n", sum_valid(graph, pages));
	printf("Sum of fixed: %d\n", sum_invalid(graph, pages));
	for (size_t i = 0; i < pages.size; i++) {
		array_deinit(pages.data[i]);
	}
	array_deinit(pages);

	for (size_t i = 0; i < graph.size; i++) {
		array_deinit(graph.data[i].neighbors);
	}
	array_deinit(graph);
	free(data);
	return 0;
}
