#include <assert.h>
#include <errno.h>
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

// Main program ======================================================

long score(long target_x, long target_y, long a_dx, long a_dy, long b_dx,
		   long b_dy)
{
	long a_count_n = b_dy * target_x - b_dx * target_y;
	long a_count_d = a_dx * b_dy - a_dy * b_dx;
	long b_count_n = a_dy * target_x - a_dx * target_y;
	long b_count_d = a_dy * b_dx - a_dx * b_dy;
	if (a_count_n % a_count_d == 0 && b_count_n % b_count_d == 0) {
		return 3 * a_count_n / a_count_d + b_count_n / b_count_d;
	}
	return 0;
}

long score_game(char *game, bool part_2)
{
	long operands[6] = {0};
	int n_op = 0;
	char *curr = game;
	while (curr[0] != '\0') {
		if (curr[0] >= '0' && curr[0] <= '9') {
			char *next = NULL;
			operands[n_op++] = strtol(curr, &next, 10);
			if (n_op > 6) {
				fprintf(stderr, "Bad format.\n");
				exit(1);
			}
			curr = next;
		} else {
			curr++;
		}
	}

	long a_dx = operands[0];
	long a_dy = operands[1];
	long b_dx = operands[2];
	long b_dy = operands[3];
	long target_x = operands[4];
	long target_y = operands[5];
	if (part_2) {
		target_x += 10000000000000;
		target_y += 10000000000000;
	}
	return score(target_x, target_y, a_dx, a_dy, b_dx, b_dy);
}

void split_games(string_array_t *out, char *src)
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

int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s input\n", argv[0]);
		exit(1);
	}
	char *data = read_file(argv[1]);

	[[gnu::cleanup(string_array_deinit)]]
	string_array_t games = {0};
	array_init(games, char *);
	split_games(&games, data);

	long sum = 0;
	for (long i = 0; i < games.size; i++) {
		sum += score_game(games.data[i], false);
	}
	printf("Minimum tokens: %ld\n", sum);

	sum = 0;
	for (long i = 0; i < games.size; i++) {
		sum += score_game(games.data[i], true);
	}
	printf("Minimum tokens 2: %ld\n", sum);

	free(data);
	return 0;
}
