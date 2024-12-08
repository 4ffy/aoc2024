#include <errno.h>
#include <regex.h>
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

int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s input\n", argv[0]);
		exit(1);
	}
	char *data = read_file(argv[1]);

	[[gnu::cleanup(string_array_deinit)]]
	string_array_t lines = {0};
	array_init(lines, char *);
	split_regex(&lines, data, "\n");
	for (size_t i = 0; i < lines.size; i++) {
		printf("%02ld\t%s\n", i, lines.data[i]);
	}

	[[gnu::cleanup(int_array_deinit)]]
	int_array_t test = {0};
	array_init(test, int);
	for (int i = 1; i < 11; i++) {
		array_push(test, int, i);
	}
	for (long i = 0; i < test.size; i++) {
		printf("%d\n", test.data[i]);
	}

	free(data);
	return 0;
}
