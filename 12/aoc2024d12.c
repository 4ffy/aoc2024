#include <errno.h>
#include <regex.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Template ==========================================================

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

typedef struct string_array_s {
	char **data;
	long size;
	long cap;
} string_array_t;

static inline void string_array_deinit(string_array_t *s) { array_deinit(*s); }

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

void split_regex(string_array_t *out, char *src, char const *pat)
{
	[[gnu::cleanup(regfree)]]
	regex_t re = {0};
	regmatch_t match[1] = {0};
	if (regcomp(&re, pat, REG_EXTENDED)) {
		fprintf(stderr, "Bad regex.\n");
		exit(1);
	}
	out->size = 0;
	char *substr = src;
	while (!regexec(&re, substr, 1, match, 0)) {
		for (regoff_t i = match[0].rm_so; i < match[0].rm_eo; i++) {
			substr[i] = '\0';
		}
		array_push((*out), char *, substr);
		substr += match[0].rm_eo;
	}
	if (strlen(substr) > 0) {
		array_push((*out), char *, substr);
	}
}

// Data structures ===================================================

typedef struct long_pair_s {
	long first;
	long second;
} long_pair_t;

typedef struct long_pair_array_s {
	long_pair_t *data;
	long size;
	long cap;
} long_pair_array_t;

static inline void long_pair_array_deinit(long_pair_array_t *l)
{
	array_deinit(*l);
}

typedef struct walk_result_s {
	int area;
	int perimeter;
	int sides;
} walk_result_t;

typedef enum {
	CONN_NORTH = 1,
	CONN_SOUTH = 2,
	CONN_WEST = 4,
	CONN_EAST = 8,
} connection_t;

typedef struct cell_s {
	char id;
	bool visited;
	uint8_t neighbors;
} cell_t;

typedef struct grid_s {
	cell_t *data;
	long size;
	long cap;
	long width;
	long height;
} grid_t;

static inline void grid_deinit(grid_t *g)
{
	array_deinit(*g);
	g->width = 0;
	g->height = 0;
}

static inline cell_t *grid_get(grid_t *grid, long y, long x)
{
	return &grid->data[y * grid->width + x];
}

static inline long_pair_t grid_get_location(grid_t *grid, cell_t *cell)
{
	long idx = cell - grid->data;
	long_pair_t result = {.first = idx / grid->width, // (y, x)
						  .second = idx % grid->width};
	return result;
}

static inline bool grid_is_visited(grid_t *grid, long y, long x)
{
	return grid_get(grid, y, x)->visited;
}

static inline void grid_visit(grid_t *grid, long y, long x)
{
	grid->data[y * grid->width + x].visited = true;
}

static inline void grid_clear_visited(grid_t *grid)
{
	for (long i = 0; i < grid->size; i++) {
		grid->data[i].visited = false;
	}
}

// Main program ======================================================

void print_grid(grid_t *grid)
{
	for (long y = 0; y < grid->height - 1; y++) {
		for (long x = 0; x < grid->width - 1; x++) {
			cell_t *cell = grid_get(grid, y, x);
			printf("%c%s", cell->id, cell->neighbors & CONN_EAST ? "--" : "  ");
		}
		printf("%c\n", grid_get(grid, y, grid->width - 1)->id);

		for (long x = 0; x < grid->width; x++) {
			cell_t *cell = grid_get(grid, y, x);
			printf("%c  ", cell->neighbors & CONN_SOUTH ? '|' : ' ');
		}
		printf("\n");
	}
	for (long x = 0; x < grid->width - 1; x++) {
		cell_t *cell = grid_get(grid, grid->height - 1, x);
		printf("%c%s", cell->id, cell->neighbors & CONN_EAST ? "--" : "  ");
	}
	printf("%c\n", grid_get(grid, grid->height - 1, grid->width - 1)->id);
}

void parse_grid(grid_t *out, char *src)
{
	[[gnu::cleanup(string_array_deinit)]]
	string_array_t lines = {0};
	array_init(lines, char *);
	split_regex(&lines, src, "\n");
	out->height = lines.size;
	out->width = strlen(lines.data[0]);
	for (long y = 0; y < out->height; y++) {
		for (long x = 0; x < out->width; x++) {
			cell_t temp = {
				.id = lines.data[y][x], .visited = false, .neighbors = 0};
			array_push(*out, cell_t, temp);
		}
	}
}

void find_regions(grid_t *grid)
{
	for (long y = 0; y < grid->height; y++) {
		for (long x = 0; x < grid->width; x++) {
			cell_t *cell = grid_get(grid, y, x);
			if (y > 0 && grid_get(grid, y - 1, x)->id == cell->id) {
				cell->neighbors |= CONN_NORTH;
				grid_get(grid, y - 1, x)->neighbors |= CONN_SOUTH;
			}
			if (y < grid->height - 1 &&
				grid_get(grid, y + 1, x)->id == cell->id) {
				cell->neighbors |= CONN_SOUTH;
				grid_get(grid, y + 1, x)->neighbors |= CONN_NORTH;
			}
			if (x > 0 && grid_get(grid, y, x - 1)->id == cell->id) {
				cell->neighbors |= CONN_WEST;
				grid_get(grid, y, x - 1)->neighbors |= CONN_EAST;
			}
			if (x < grid->width - 1 &&
				grid_get(grid, y, x + 1)->id == cell->id) {
				cell->neighbors |= CONN_EAST;
				grid_get(grid, y, x + 1)->neighbors |= CONN_WEST;
			}
		}
	}
}

walk_result_t walk_region(grid_t *grid, long start_y, long start_x)
{
	int area = 0;
	int perimeter = 0;
	int sides = 0;

	[[gnu::cleanup(long_pair_array_deinit)]]
	long_pair_array_t stack = {0};
	array_init(stack, long_pair_t);
	long_pair_t curr = {.first = start_y, .second = start_x};
	array_push(stack, long_pair_t, curr);
	while (stack.size > 0) {
		curr = stack.data[--stack.size];
		long y = curr.first;
		long x = curr.second;
		cell_t *cell = grid_get(grid, y, x);
		if (!cell->visited) {
			cell->visited = true;
			area++;

			if (cell->neighbors & CONN_NORTH) {
				long_pair_t temp = {.first = y - 1, .second = x};
				array_push(stack, long_pair_t, temp);
			} else {
				perimeter++;
			}

			if (cell->neighbors & CONN_SOUTH) {
				long_pair_t temp = {.first = y + 1, .second = x};
				array_push(stack, long_pair_t, temp);
			} else {
				perimeter++;
			}

			if (cell->neighbors & CONN_WEST) {
				long_pair_t temp = {.first = y, .second = x - 1};
				array_push(stack, long_pair_t, temp);
			} else {
				perimeter++;
			}

			if (cell->neighbors & CONN_EAST) {
				long_pair_t temp = {.first = y, .second = x + 1};
				array_push(stack, long_pair_t, temp);
			} else {
				perimeter++;
			}
		}
	}

	walk_result_t result = {
		.area = area, .perimeter = perimeter, .sides = sides};
	return result;
}

long sum_price(grid_t *grid)
{
	long sum = 0;
	grid_clear_visited(grid);
	for (long y = 0; y < grid->height; y++) {
		for (long x = 0; x < grid->width; x++) {
			if (!grid_is_visited(grid, y, x)) {
				walk_result_t walk = walk_region(grid, y, x);
				sum += walk.area * walk.perimeter;
			}
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
	[[gnu::cleanup(grid_deinit)]]
	grid_t grid = {0};
	array_init(grid, cell_t);
	parse_grid(&grid, data);
	find_regions(&grid);
	long result = sum_price(&grid);
	printf("Normal cost: %ld\n", result);
	free(data);
	return 0;
}
