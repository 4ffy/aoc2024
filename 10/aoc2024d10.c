#include <errno.h>
#include <regex.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SET_MAX_LOAD 0.75

#define array_init(arr, type)                                                  \
	do {                                                                       \
		arr.data = calloc(8, sizeof(type));                                    \
		if (arr.data == nullptr) {                                             \
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
		arr.data[arr.size++] = item;                                           \
		if (arr.size == arr.cap) {                                             \
			arr.cap *= 2;                                                      \
			arr.data = realloc(arr.data, arr.cap * sizeof(type));              \
			if (arr.data == nullptr) {                                         \
				fprintf(stderr, "Out of memory.\n");                           \
				exit(1);                                                       \
			}                                                                  \
		}                                                                      \
	} while (0)

typedef struct int_array_s {
	int *data;
	long size;
	long cap;
} int_array_t;

typedef struct string_array_s {
	char **data;
	long size;
	long cap;
} string_array_t;

void string_array_deinit(string_array_t *s) { array_deinit((*s)); }

typedef struct point_s {
	int y;
	int x;
} point_t;

static inline bool points_equal(point_t p1, point_t p2)
{
	return p1.x == p2.x && p1.y == p2.y;
}

typedef struct point_array_s {
	point_t *data;
	long size;
	long cap;
} point_array_t;

void point_array_deinit(point_array_t *p) { array_deinit((*p)); }

typedef struct grid_s {
	int_array_t data;
	long height;
	long width;
} grid_t;

void grid_deinit(grid_t *g)
{
	array_deinit((g->data));
	g->height = 0;
	g->width = 0;
}

static inline int grid_get(grid_t *g, int y, int x)
{
	return g->data.data[y * g->width + x];
}

typedef struct point_set_entry_s {
	bool has_value;
	point_t value;
} point_set_entry_t;

typedef struct point_set_s {
	point_set_entry_t *data;
	long size;
	long cap;
} point_set_t;

void point_set_deinit(point_set_t *p) { array_deinit((*p)); }

uint32_t hash_point(point_t point)
{
	size_t length = sizeof(point);
	uint8_t *data = (uint8_t *)(&point);
	uint32_t hash = 2166136261u;
	for (size_t i = 0; i < length; i++) {
		hash ^= data[i];
		hash *= 16777619;
	}
	return hash;
}

point_set_entry_t *_point_set_find(point_set_entry_t *entries, long cap,
								   point_t point)
{
	uint32_t hash = hash_point(point);
	uint32_t idx = hash & (cap - 1);
	while (true) {
		point_set_entry_t *entry = &entries[idx];
		if (!entry->has_value) {
			return entry;
		}
		if (entry->has_value && points_equal(entry->value, point)) {
			return entry;
		}
		idx = (idx + 1) & (cap - 1);
	}
}

void _point_set_expand(point_set_t *set)
{
	long new_cap = set->cap * 2;
	point_set_entry_t *new_data = calloc(new_cap, sizeof(point_set_entry_t));
	if (new_data == nullptr) {
		fprintf(stderr, "Out of memory.\n");
		exit(1);
	}
	set->size = 0;
	for (long i = 0; i < set->cap; i++) {
		point_set_entry_t entry = set->data[i];
		if (entry.has_value) {
			point_set_entry_t *dest =
				_point_set_find(new_data, new_cap, entry.value);
			*dest = entry;
			set->size++;
		}
	}
	free(set->data);
	set->data = new_data;
	set->cap = new_cap;
}

bool point_set_insert(point_set_t *set, point_t point)
{
	if ((double)(set->size + 1) > (double)set->cap * SET_MAX_LOAD) {
		_point_set_expand(set);
	}
	point_set_entry_t *dest = _point_set_find(set->data, set->cap, point);
	bool is_new = !dest->has_value;
	if (is_new) {
		set->size++;
	}
	dest->has_value = true;
	dest->value = point;
	return is_new;
}

point_set_entry_t point_set_get(point_set_t *set, point_t point)
{
	point_set_entry_t result = {0};
	result.has_value = false;
	if (set->size == 0) {
		return result;
	}
	point_set_entry_t *entry = _point_set_find(set->data, set->cap, point);
	if (entry->has_value) {
		result.has_value = true;
		result.value = point;
	}
	return result;
}

void fclose_wrap(FILE **f) { fclose(*f); }

char *read_file(char const *filename)
{
	[[gnu::cleanup(fclose_wrap)]] FILE *f = fopen(filename, "rb");
	if (f == nullptr) {
		fprintf(stderr, "Could not open \"%s\": %s\n", filename,
				strerror(errno));
		exit(1);
	}

	fseek(f, 0L, SEEK_END);
	long length = ftell(f);
	rewind(f);
	char *buf = (char *)malloc(length * sizeof(char) + 1);
	if (buf == nullptr) {
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

void parse_grid(grid_t *grid, char *src)
{
	[[gnu::cleanup(string_array_deinit)]]
	string_array_t lines = {0};
	array_init(lines, char *);
	split_regex(&lines, src, "\n");

	grid->height = lines.size;
	grid->width = (long)strlen(lines.data[0]);
	for (int y = 0; y < grid->height; y++) {
		for (int x = 0; x < grid->width; x++) {
			int digit = lines.data[y][x] - '0';
			array_push(grid->data, int, digit);
		}
	}
}

typedef struct walk_result_s {
	int score;
	int rating;
} walk_result_t;

walk_result_t walk(grid_t *g, long start_y, long start_x)
{
	[[gnu::cleanup(point_set_deinit)]]
	point_set_t unique = {0};
	array_init(unique, point_set_entry_t);

	[[gnu::cleanup(point_array_deinit)]]
	point_array_t stack = {0};
	array_init(stack, point_t);

	int rating = 0;
	point_t curr = {.y = (int)start_y, .x = (int)start_x};
	point_t temp = {0};
	array_push(stack, point_t, curr);
	while (stack.size > 0) {
		curr = stack.data[--stack.size];
		int y = curr.y;
		int x = curr.x;
		int value = grid_get(g, y, x);
		int target = value + 1;
		if (value == 9) {
			rating++;
			point_set_insert(&unique, curr);
			continue;
		}
		if (y > 0 && grid_get(g, y - 1, x) == target) {
			temp.y = y - 1;
			temp.x = x;
			array_push(stack, point_t, temp);
		}
		if (y < g->height - 1 && grid_get(g, y + 1, x) == target) {
			temp.y = y + 1;
			temp.x = x;
			array_push(stack, point_t, temp);
		}
		if (x > 0 && grid_get(g, y, x - 1) == target) {
			temp.y = y;
			temp.x = x - 1;
			array_push(stack, point_t, temp);
		}
		if (x < g->width - 1 && grid_get(g, y, x + 1) == target) {
			temp.y = y;
			temp.x = x + 1;
			array_push(stack, point_t, temp);
		}
	}
	walk_result_t result = {.score = unique.size, .rating = rating};
	return result;
}

walk_result_t score_all(grid_t *grid)
{
	walk_result_t result = {0};
	for (long y = 0; y < grid->height; y++) {
		for (long x = 0; x < grid->width; x++) {
			if (grid_get(grid, y, x) == 0) {
				walk_result_t temp = walk(grid, y, x);
				result.score += temp.score;
				result.rating += temp.rating;
			}
		}
	}
	return result;
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
	array_init(grid.data, int);
	parse_grid(&grid, data);

	walk_result_t result = score_all(&grid);
	printf("Sum of score: %d\n", result.score);
	printf("Sum of rating: %d\n", result.rating);

	free(data);
	return 0;
}
