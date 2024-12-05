#include <errno.h>
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

typedef enum {
	COLOR_WHITE,
	COLOR_GRAY,
	COLOR_BLACK,
} color_t;

typedef struct int_array_s int_array_t;
struct int_array_s {
	int *data;
	size_t size;
	size_t cap;
};

typedef struct node_s node_t;
struct node_s {
	int value;
	int_array_t neighbors;
	color_t color;
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
	node.color = COLOR_WHITE;
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

bool graph_has_unvisited(graph_t g)
{
	for (size_t i = 0; i < g.size; i++) {
		if (g.data[i].color == COLOR_WHITE) {
			return true;
		}
	}
	return false;
}

node_t *graph_find_node(graph_t g, int value)
{
	// I could maintain a sorted list or something to speed this up.
	for (size_t i = 0; i < g.size; i++) {
		if (g.data[i].value == value) {
			return g.data + i;
		}
	}
	return NULL;
}

void tsort_visit(graph_t g, node_t *node, int_array_t *result)
{
	if (node == NULL) {
		return;
	}
	if (node->color == COLOR_BLACK) {
		return;
	}
	if (node->color == COLOR_GRAY) {
		fprintf(stderr, "Graph has a cycle.\n");
		exit(1);
	}
	node->color = COLOR_GRAY;
	for (size_t i = 0; i < node->neighbors.size; i++) {
		node_t *neighbor = graph_find_node(g, node->neighbors.data[i]);
		tsort_visit(g, neighbor, result);
	}
	node->color = COLOR_BLACK;
	array_push((*result), int, node->value);
}

int_array_t tsort(graph_t g)
{
	int_array_t result = {0};
	array_init(result, int);
	for (size_t i = 0; i < g.size; i++) {
		g.data[i].color = COLOR_WHITE;
	}
	node_t *start = NULL;
	while (graph_has_unvisited(g)) {
		for (size_t i = 0; i < g.size; i++) {
			if (g.data[i].color == COLOR_WHITE) {
				start = g.data + i;
				break;
			}
		}
		tsort_visit(g, start, &result);
	}
	return result;
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

int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s input\n", argv[0]);
		exit(1);
	}

	char *data = read_file(argv[1]);

	graph_t graph = {0};
	array_init(graph, node_t);

	for (int i = 0; i < 10; i++) {
		node_t node = node_new(i);
		array_push(graph, node_t, node);
	}
	array_push(graph.data[1].neighbors, int, 2);
	array_push(graph.data[1].neighbors, int, 4);
	array_push(graph.data[1].neighbors, int, 8);
	array_push(graph.data[2].neighbors, int, 0);
	array_push(graph.data[2].neighbors, int, 3);
	array_push(graph.data[5].neighbors, int, 8);
	array_push(graph.data[5].neighbors, int, 9);
	array_push(graph.data[6].neighbors, int, 1);
	array_push(graph.data[6].neighbors, int, 5);
	array_push(graph.data[8].neighbors, int, 7);
	array_push(graph.data[4].neighbors, int, 3);
	graph_print(graph);

	int_array_t sorted = tsort(graph);
	for (size_t i = 0; i < sorted.size; i++) {
		printf("%d\n", sorted.data[i]);
	}
	array_deinit(sorted);

	for (size_t i = 0; i < graph.size; i++) {
		array_deinit(graph.data[i].neighbors);
	}
	array_deinit(graph);
	free(data);
	return 0;
}
