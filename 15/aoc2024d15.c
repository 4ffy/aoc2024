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

typedef struct string_array_s {
	char **data;
	long size;
	long cap;
} string_array_t;

void string_array_deinit(string_array_t *s) { array_deinit(*s); }

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

typedef struct grid_s {
	char *data;
	long size;
	long cap;
	long height;
	long width;
	long robot_x;
	long robot_y;
} grid_t;

void grid_deinit(grid_t *g)
{
	array_deinit(*g);
	g->height = 0;
	g->width = 0;
}

static inline bool in_bounds(grid_t *g, long y, long x)
{
	return y >= 0 && y < g->height && x >= 0 && x < g->width;
}

static inline char get(grid_t *g, long y, long x)
{
	return g->data[y * g->width + x];
}

void print_grid(grid_t *grid)
{
	for (long y = 0; y < grid->height; y++) {
		for (long x = 0; x < grid->width; x++) {
			printf("%c", get(grid, y, x));
		}
		printf("\n");
	}
}

void to_wide(grid_t *to, grid_t *from)
{
	to->size = 0;
	if (to->cap < 2 * from->cap) {
		to->cap = 2 * from->cap;
		to->data = realloc(to->data, to->cap * sizeof(char));
		if (to->data == nullptr) {
			fprintf(stderr, "Out of memory.\n");
			exit(1);
		}
	}
	to->height = from->height;
	to->width = 2 * from->width;
	to->robot_y = from->robot_y;
	to->robot_x = 2 * from->robot_x;
	for (long i = 0; i < from->size; i++) {
		char c = from->data[i];
		switch (c) {
		case 'O':
			array_push(*to, char, '[');
			array_push(*to, char, ']');
			break;
		case '@':
			array_push(*to, char, '@');
			array_push(*to, char, '.');
			break;
		default:
			array_push(*to, char, c);
			array_push(*to, char, c);
			break;
		}
	}
}

// Parsing =====================================================================

void split_newlines(string_array_t *out, char *src)
{
	out->size = 0;
	char *start = src;
	char *curr = src;
	while (curr[0] != '\0') {
		if (curr[0] == '\n') {
			array_push(*out, char *, start);
			curr[0] = '\0';
			curr++;
			start = curr;
		} else {
			curr++;
		}
	}
	array_push(*out, char *, start);
}

void split_double_newline(string_array_t *out, char *src)
{
	out->size = 0;
	char *start = src;
	char *curr = src;
	while (curr[0] != '\0') {
		if (curr[0] == '\n' && curr[1] != '\0' && curr[1] == '\n') {
			array_push(*out, char *, start);
			curr[0] = '\0';
			curr[1] = '\0';
			curr += 2;
			start = curr;
		} else {
			curr++;
		}
	}
	array_push(*out, char *, start);
}

void parse_grid(grid_t *out, string_array_t *workspace, char *src)
{
	split_newlines(workspace, src);
	out->height = workspace->size;
	out->width = strlen(workspace->data[0]);
	for (long y = 0; y < out->height; y++) {
		for (long x = 0; x < out->width; x++) {
			char c = workspace->data[y][x];
			if (c == '@') {
				out->robot_y = y;
				out->robot_x = x;
			}
			array_push(*out, char, c);
		}
	}
}

// Main program ================================================================

bool can_move(grid_t *grid, long y, long x, char dir)
{
	char c = get(grid, y, x);
	if (c == '.') {
		return true;
	}
	if (c == '#') {
		return false;
	}
	switch (dir) {
	case '^':
		if (c == '[') {
			return false;
		} else if (c == ']') {
			return false;
		} else {
			return in_bounds(grid, y - 1, x) && can_move(grid, y - 1, x, '^');
		}
	case 'v':
		if (c == '[') {
			return false;
		} else if (c == ']') {
			return false;
		} else {
			return in_bounds(grid, y + 1, x) && can_move(grid, y + 1, x, 'v');
		}
	case '<':
		return in_bounds(grid, y, x - 1) && can_move(grid, y, x - 1, '<');
	case '>':
		return in_bounds(grid, y, x + 1) && can_move(grid, y, x + 1, '>');
	default:
		fprintf(stderr, "Bad direction '%c'\n", dir);
		return false;
	}
}

void force_move(grid_t *grid, long y, long x, char dir)
{
	char c = get(grid, y, x);
	switch (dir) {
	case '^':
		grid->data[(y - 1) * grid->width + x] = c;
		grid->data[y * grid->width + x] = '.';
		break;
	case 'v':
		grid->data[(y + 1) * grid->width + x] = c;
		grid->data[y * grid->width + x] = '.';
		break;
	case '<':
		grid->data[y * grid->width + (x - 1)] = c;
		grid->data[y * grid->width + x] = '.';
		break;
	case '>':
		grid->data[y * grid->width + (x + 1)] = c;
		grid->data[y * grid->width + x] = '.';
		break;
	default:
		fprintf(stderr, "Bad direction '%c'\n", dir);
		return;
	}
}

bool move(grid_t *grid, long y, long x, char dir)
{
	char c = get(grid, y, x);
	if (!can_move(grid, y, x, dir)) {
		return false;
	}
	if (c == '.') {
		return false;
	}
	switch (dir) {
	case '^':
		if (c == '[') {
			return false;
		} else if (c == ']') {
			return false;
		} else {
			move(grid, y - 1, x, '^');
			force_move(grid, y, x, '^');
			return true;
		}
	case 'v':
		if (c == '[') {
			return false;
		} else if (c == ']') {
			return false;
		} else {
			move(grid, y + 1, x, 'v');
			force_move(grid, y, x, 'v');
			return true;
		}
	case '<':
		move(grid, y, x - 1, '<');
		force_move(grid, y, x, '<');
		return true;
	case '>':
		move(grid, y, x + 1, '>');
		force_move(grid, y, x, '>');
		return true;
	default:
		fprintf(stderr, "Bad direction '%c'\n", dir);
		return false;
	}
}

void move_robot(grid_t *grid, char *moves)
{
	char *mov = moves;
	while (mov[0] != '\0') {
		switch (mov[0]) {
		case '^':
			if (move(grid, grid->robot_y, grid->robot_x, '^')) {
				grid->robot_y--;
			}
			break;
		case 'v':
			if (move(grid, grid->robot_y, grid->robot_x, 'v')) {
				grid->robot_y++;
			}
			break;
		case '<':
			if (move(grid, grid->robot_y, grid->robot_x, '<')) {
				grid->robot_x--;
			}
			break;
		case '>':
			if (move(grid, grid->robot_y, grid->robot_x, '>')) {
				grid->robot_x++;
			}
			break;
		default:
			break;
		}
		mov++;
	}
}

long gps(grid_t *grid)
{
	long sum = 0;
	for (long y = 0; y < grid->height; y++) {
		for (long x = 0; x < grid->width; x++) {
			char c = get(grid, y, x);
			if (c == 'O' || c == '[') {
				sum += 100 * y + x;
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

	[[gnu::cleanup(string_array_deinit)]]
	string_array_t workspace = {0};
	array_init(workspace, char *);
	split_double_newline(&workspace, data);
	if (workspace.size != 2) {
		fprintf(stderr, "Bad input format\n");
		free(data);
		return 1;
	}
	char *grid_src = workspace.data[0];
	char *moves = workspace.data[1];

	[[gnu::cleanup(grid_deinit)]]
	grid_t grid = {0};
	array_init(grid, char);
	parse_grid(&grid, &workspace, grid_src);

	[[gnu::cleanup(grid_deinit)]]
	grid_t wide = {0};
	array_init(wide, char);
	to_wide(&wide, &grid);

	move_robot(&grid, moves);
	// print_grid(&grid);
	printf("Narrow GPS: %ld\n", gps(&grid));

	free(data);
	return 0;
}
