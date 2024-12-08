#include <errno.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// A-Z, a-z, 0-9
#define NUM_SYMBOLS 62

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
	grid_print(&grid);
	printf("\n");

	free(data);
	return 0;
}
