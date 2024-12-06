#include <assert.h>
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

//! Directions. Can also be used as a bit mask.
typedef enum {
	DIR_NORTH = 1 << 0,
	DIR_SOUTH = 1 << 1,
	DIR_EAST = 1 << 2,
	DIR_WEST = 1 << 3,
} direction_t;

//! Grid cell.
typedef struct cell_s {
	//! Cell is an obstacle.
	bool is_obstacle;
	//! Bit field of directions visited from. 0 is unvisited.
	unsigned is_visited;
} cell_t;

//! Dynamic array of cells.
typedef struct cell_array_s {
	cell_t *data;
	long size;
	long cap;
} cell_array_t;

//! Main grid struct.
typedef struct grid_s {
	cell_array_t data;
	long width;
	long height;
	long start_x;
	long start_y;
} grid_t;

//! Get a cell from a grid.
static inline cell_t *grid_get(grid_t *g, long y, long x)
{
	return &g->data.data[y * g->width + x];
}

//! Print a grid to stdout.
void grid_print(grid_t *grid)
{
	printf(" ");
	for (long x = 0; x < grid->width; x++) {
		printf("%ld", x % 10);
	}
	printf("\n");

	for (long y = 0; y < grid->height; y++) {
		printf("%ld", y % 10);
		for (long x = 0; x < grid->width; x++) {
			if (grid_get(grid, y, x)->is_obstacle) {
				printf("#");
			} else if (grid_get(grid, y, x)->is_visited) {
				printf("@");
			} else {
				printf(".");
			}
		}
		printf("\n");
	}
}

//! Wrapper around fclose for cleanup annotation.
void fclose_wrap(FILE **f) { fclose(*f); }

//! Read a file to a null-terminated string.
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

//! Parse the source string to initialize a grid.
void parse_grid(grid_t *grid, char const *src)
{
	array_init((grid->data), cell_t);
	grid->width = -1;
	grid->height = 0;
	grid->start_x = 0;
	grid->start_y = 0;

	for (char const *c = src; *c != '\0'; c++) {
		cell_t new_cell = {.is_obstacle = false, .is_visited = 0};
		switch (*c) {
		case '\n':
			grid->height++;
			if (grid->width == -1) {
				grid->width = c - src;
			}
			break;
		case '#':
			new_cell.is_obstacle = true;
			array_push((grid->data), cell_t, new_cell);
			break;
		case '^':
			array_push((grid->data), cell_t, new_cell);
			if (grid->width == -1) {
				grid->start_x = c - src;
			} else {
				grid->start_x = (c - src - grid->height) % grid->width;
			}
			grid->start_y = grid->height;
			break;
		default:
			array_push((grid->data), cell_t, new_cell);
			break;
		}
	}
}

//! Walk the grid, turning clockwise at obstacles, until the guard goes out of
//! bounds or encounters a loop. Return 1 if this is a loop return, 0 otherwise.
int grid_run(grid_t *grid)
{
	for (long i = 0; i < grid->data.size; i++) {
		grid->data.data[i].is_visited = 0;
	}

	long x = grid->start_x;
	long y = grid->start_y;
	direction_t dir = DIR_NORTH;

	while (true) {
		cell_t *curr = grid_get(grid, y, x);
		if ((curr->is_visited | dir) == curr->is_visited) {
			// Loop found.
			return 1;
		}
		curr->is_visited |= dir;

		switch (dir) {
		case DIR_NORTH:
			if (y - 1 < 0) {
				return 0;
			} else if (grid_get(grid, y - 1, x)->is_obstacle) {
				dir = DIR_EAST;
			} else {
				y--;
			}
			break;

		case DIR_SOUTH:
			if (y + 1 == grid->height) {
				return 0;
			} else if (grid_get(grid, y + 1, x)->is_obstacle) {
				dir = DIR_WEST;
			} else {
				y++;
			}
			break;

		case DIR_WEST:
			if (x - 1 < 0) {
				return 0;
			} else if (grid_get(grid, y, x - 1)->is_obstacle) {
				dir = DIR_NORTH;
			} else {
				x--;
			}
			break;

		case DIR_EAST:
			if (x + 1 == grid->width) {
				return 0;
			} else if (grid_get(grid, y, x + 1)->is_obstacle) {
				dir = DIR_SOUTH;
			} else {
				x++;
			}
			break;
		}
	}
}

//! Count the number of visited cells after a run.
int grid_count_visited(grid_t *grid)
{
	int sum = 0;
	for (long i = 0; i < grid->data.size; i++) {
		if (grid->data.data[i].is_visited) {
			sum++;
		}
	}
	return sum;
}

bool grid_obstacle_creates_loop(grid_t *grid, long y, long x)
{
	cell_t *cell = grid_get(grid, y, x);
	if (!cell->is_visited) {
		return false;
	}
	// North
	if (y - 1 > 0 && grid_get(grid, y - 1, x)->is_visited & DIR_WEST) {
		return true;
	}
	// South
	if (y + 1 < grid->height &&
		grid_get(grid, y + 1, x)->is_visited & DIR_EAST) {
		return true;
	}
	// West
	if (x - 1 > 0 && grid_get(grid, y, x - 1)->is_visited & DIR_SOUTH) {
		return true;
	}
	// East
	if (x + 1 < grid->width &&
		grid_get(grid, y, x + 1)->is_visited & DIR_NORTH) {
		return true;
	}
	return false;
}

int grid_count_loop_add(grid_t *grid)
{
	int sum = 0;
	for (long y = 0; y < grid->height; y++) {
		for (long x = 0; x < grid->width; x++) {
			if (grid_obstacle_creates_loop(grid, y, x)) {
				sum++;
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

	grid_t grid = {0};
	parse_grid(&grid, data);
	grid_print(&grid);
	printf("\n");

	int status = grid_run(&grid);
	grid_print(&grid);
	printf("\n");

	printf("%s exit.\n", status == 0 ? "Normal" : "Loop");
	printf("Cells visited: %d\n", grid_count_visited(&grid));
	printf("Loops possible: %d\n", grid_count_loop_add(&grid));

	array_deinit(grid.data);
	free(data);
	return 0;
}
