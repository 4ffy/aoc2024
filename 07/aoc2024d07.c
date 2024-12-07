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
		arr.data[arr.size++] = item;                                           \
		if (arr.size == arr.cap) {                                             \
			arr.cap *= 2;                                                      \
			arr.data = realloc(arr.data, arr.cap * sizeof(type));              \
			if (arr.data == NULL) {                                            \
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

void int_array_deinit(int_array_t *i) { array_deinit((*i)); }

typedef struct string_array_s {
	char **data;
	long size;
	long cap;
} string_array_t;

void string_array_deinit(string_array_t *s) { array_deinit((*s)); }

typedef struct record_s {
	long expected;
	int_array_t operands;
} record_t;

typedef struct record_array_s {
	record_t *data;
	long size;
	long cap;
} record_array_t;

void record_array_deinit(record_array_t *r)
{
	for (long i = 0; i < r->size; i++) {
		array_deinit(r->data[i].operands);
	}
	array_deinit((*r));
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

void parse_data(record_array_t *out, char *src)
{
	[[gnu::cleanup(string_array_deinit)]]
	string_array_t lines = {0};
	array_init(lines, record_t);

	[[gnu::cleanup(string_array_deinit)]]
	string_array_t tokens = {0};
	array_init(tokens, char *);

	split_regex(&lines, src, "\n");
	for (long line = 0; line < lines.size; line++) {
		split_regex(&tokens, lines.data[line], ":? ");
		if (tokens.size < 3) {
			fprintf(stderr, "Bad input.\n");
			exit(1);
		}
		long expected = strtol(tokens.data[0], NULL, 10);
		int_array_t operands = {0};
		array_init(operands, int);
		for (long i = 1; i < tokens.size; i++) {
			array_push(operands, int, (int)strtol(tokens.data[i], NULL, 10));
		}
		record_t result = {.expected = expected, .operands = operands};
		array_push((*out), record_t, result);
	}
}

bool _record_is_valid_recurse(record_t *record, long target, long end)
{
	long last = record->operands.data[end];
	if (end == 0) {
		return last == target;
	}
	if (target % last == 0) {
		return _record_is_valid_recurse(record, target / last, end - 1);
	}
	if (target - last > 0) {
		return _record_is_valid_recurse(record, target - last, end - 1);
	}
	return false;
}

bool record_is_valid(record_t *record)
{
	return _record_is_valid_recurse(record, record->expected,
									record->operands.size - 1);
}

long sum_valid_records(record_array_t *records)
{
	long sum = 0;
	for (long i = 0; i < records->size; i++) {
		record_t *curr = &records->data[i];
		if (record_is_valid(curr)) {
			sum += curr->expected;
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
	char *src = read_file(argv[1]);

	[[gnu::cleanup(record_array_deinit)]]
	record_array_t records = {0};
	array_init(records, record_t);
	parse_data(&records, src);

	printf("Sum of valid: %ld\n", sum_valid_records(&records));

	free(src);
	return 0;
}
