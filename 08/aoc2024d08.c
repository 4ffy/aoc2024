#include <errno.h>
#include <regex.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// A-Z, a-z, 0-9
#define NUM_SYMBOLS 62
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
	point_array_t chars[NUM_SYMBOLS];
	int width;
	int height;
} grid_t;

void grid_deinit(grid_t *g)
{
	for (int i = 0; i < NUM_SYMBOLS; i++) {
		array_deinit(g->chars[i]);
	}
	g->width = 0;
	g->height = 0;
}

static inline bool grid_in_bounds(grid_t *g, int y, int x)
{
	return (y >= 0 && y < g->height && x >= 0 && x < g->width);
}

int char_to_grid_index(char c)
{
	if (c >= '0' && c <= '9') {
		return c - '0';
	}
	if (c >= 'A' && c <= 'Z') {
		return c - 'A' + 10;
	}
	if (c >= 'a' && c <= 'z') {
		return c - 'a' + 36;
	}
	return -1;
}

char grid_index_to_char(int idx)
{
	if (idx < 10) {
		return (char)idx + '0';
	}
	if (idx < 36) {
		return (char)idx + 'A' - 10;
	}
	return (char)idx + 'a' - 36;
}

void grid_print(grid_t *grid)
{
	for (int i = 0; i < NUM_SYMBOLS; i++) {
		if (grid->chars[i].data) {
			printf("%c:", grid_index_to_char(i));
			for (long j = 0; j < grid->chars[i].size; j++) {
				point_t temp = grid->chars[i].data[j];
				printf(" (%d, %d)", temp.y, temp.x);
			}
			printf("\n");
		}
	}
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

	grid->height = (int)lines.size;
	grid->width = (int)strlen(lines.data[0]);
	for (int y = 0; y < grid->height; y++) {
		for (int x = 0; x < grid->width; x++) {
			char c = lines.data[y][x];
			int idx = char_to_grid_index(c);
			if (idx != -1) {
				if (grid->chars[idx].data == nullptr) {
					array_init(grid->chars[idx], point_t);
				}
				point_t point = {.y = y, .x = x};
				array_push(grid->chars[idx], point_t, point);
			}
		}
	}
}

void find_antinodes(point_set_t *out, grid_t *grid)
{
	for (int g = 0; g < NUM_SYMBOLS; g++) {
		point_array_t group = grid->chars[g];
		if (group.size == 0) {
			continue;
		}
		for (long i = 0; i < group.size - 1; i++) {
			for (long j = i + 1; j < group.size; j++) {
				point_t p1 = group.data[i];
				point_t p2 = group.data[j];
				int dy = p2.y - p1.y;
				int dx = p2.x - p1.x;
				if (grid_in_bounds(grid, p1.y - dy, p1.x - dx)) {
					point_t temp = {.y = p1.y - dy, .x = p1.x - dx};
					point_set_insert(out, temp);
				}
				if (grid_in_bounds(grid, p2.y + dy, p2.x + dx)) {
					point_t temp = {.y = p2.y + dy, .x = p2.x + dx};
					point_set_insert(out, temp);
				}
			}
		}
	}
}

int count_antinodes(grid_t *grid)
{
	[[gnu::cleanup(point_set_deinit)]]
	point_set_t antinodes = {0};
	array_init(antinodes, point_set_entry_t);
	find_antinodes(&antinodes, grid);
	int count = 0;
	for (long i = 0; i < antinodes.cap; i++) {
		point_set_entry_t entry = antinodes.data[i];
		if (entry.has_value) {
			count++;
		}
	}
	return count;
}

void find_antinodes_2(point_set_t *out, grid_t *grid)
{
	for (int g = 0; g < NUM_SYMBOLS; g++) {
		point_array_t group = grid->chars[g];
		if (group.size == 0) {
			continue;
		}
		for (long i = 0; i < group.size - 1; i++) {
			for (long j = i + 1; j < group.size; j++) {
				point_t p1 = group.data[i];
				point_t p2 = group.data[j];
				int dy = p2.y - p1.y;
				int dx = p2.x - p1.x;
				int k = 0;
				bool done = false;
				while (!done) {
					done = true;
					if (grid_in_bounds(grid, p1.y - dy * k, p1.x - dx * k)) {
						done = false;
						point_t temp = {.y = p1.y - dy * k, .x = p1.x - dx * k};
						point_set_insert(out, temp);
					}
					if (grid_in_bounds(grid, p2.y + dy * k, p2.x + dx * k)) {
						done = false;
						point_t temp = {.y = p2.y + dy * k, .x = p2.x + dx * k};
						point_set_insert(out, temp);
					}
					k++;
				}
			}
		}
	}
}

int count_antinodes_2(grid_t *grid)
{
	[[gnu::cleanup(point_set_deinit)]]
	point_set_t antinodes = {0};
	array_init(antinodes, point_set_entry_t);
	find_antinodes_2(&antinodes, grid);
	int count = 0;
	for (long i = 0; i < antinodes.cap; i++) {
		point_set_entry_t entry = antinodes.data[i];
		if (entry.has_value) {
			count++;
		}
	}
	return count;
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
	parse_grid(&grid, data);
	// grid_print(&grid);

	printf("Antinode count: %d\n", count_antinodes(&grid));
	printf("Antinode count 2: %d\n", count_antinodes_2(&grid));

	free(data);
	return 0;
}
