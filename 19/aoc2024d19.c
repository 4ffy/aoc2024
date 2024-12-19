#include <errno.h>
#include <regex.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Template ====================================================================

[[noreturn]]
void die(char const *format, ...)
{
	va_list args = {0};
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	exit(1);
}

#define array_init(arr, type)                                                  \
	do {                                                                       \
		(arr).data = calloc(8, sizeof(type));                                  \
		if ((arr).data == NULL) {                                              \
			die("Out of memory.\n");                                           \
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
				die("Out of memory.\n");                                       \
			}                                                                  \
		}                                                                      \
	} while (0)

typedef struct long_array_s {
	long *data;
	long size;
	long cap;
} long_array_t;

void long_array_deinit(long_array_t *l) { array_deinit(*l); }

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
		die("Could not open \"%s\": %s\n", filename, strerror(errno));
	}
	fseek(f, 0L, SEEK_END);
	long length = ftell(f);
	rewind(f);
	char *buf = (char *)malloc(length * sizeof(char) + 1);
	if (buf == NULL) {
		die("Could not allocate buffer: %s\n", strerror(errno));
	}
	long read = fread(buf, sizeof(char), length, f);
	if (read < length) {
		die("Could not read file \"%s\"\n", filename);
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
		die("Bad regex '%s'.\n", pat);
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

// Data structures =============================================================

typedef struct sview_s sview_t;
struct sview_s {
	char *data;
	long size;
};

static inline sview_t sview_from_string(char *str)
{
	return (sview_t){.data = str, .size = (long)strlen(str)};
}

typedef struct sview_array_s sview_array_t;
struct sview_array_s {
	sview_t *data;
	long size;
	long cap;
};

void sview_array_deinit(sview_array_t *s) { array_deinit(*s); }

#define TRIE_CHILDREN 5

typedef struct trie_s trie_t;
struct trie_s {
	bool leaf;
	trie_t *children[TRIE_CHILDREN]; // b, g, r, u, w in that order.
};

trie_t *trie_new()
{
	trie_t *trie = malloc(sizeof(trie_t));
	if (!trie) {
		die("Out of memory.\n");
	}
	trie->leaf = 0;
	for (int i = 0; i < TRIE_CHILDREN; i++) {
		trie->children[i] = nullptr;
	}
	return trie;
}

void trie_free(trie_t *trie)
{
	if (!trie) {
		return;
	}
	for (int i = 0; i < TRIE_CHILDREN; i++) {
		trie_free(trie->children[i]);
	}
	free(trie);
}

void trie_free_wrap(trie_t **trie) { trie_free(*trie); }

static inline int char_to_index(char c)
{
	switch (c) {
	case 'b':
		return 0;
	case 'g':
		return 1;
	case 'r':
		return 2;
	case 'u':
		return 3;
	case 'w':
		return 4;
	default:
		die("Invalid char '%c'.\n", c);
	}
}

long trie_size(trie_t *trie)
{
	long sum = trie->leaf ? 1 : 0;
	for (int i = 0; i < TRIE_CHILDREN; i++) {
		if (trie->children[i]) {
			sum += trie_size(trie->children[i]);
		}
	}
	return sum;
}

bool trie_insert(trie_t *trie, sview_t item)
{
	if (item.size == 0) {
		if (!trie->leaf) {
			trie->leaf = true;
			return true;
		} else {
			return false;
		}
	}
	sview_t next = {.data = item.data + 1, .size = item.size - 1};
	int idx = char_to_index(item.data[0]);
	if (!trie->children[idx]) {
		trie->children[idx] = trie_new();
	}
	return trie_insert(trie->children[idx], next);
}

bool trie_get(trie_t *trie, sview_t item)
{
	if (item.size == 0) {
		return trie->leaf;
	}
	sview_t next = {.data = item.data + 1, .size = item.size - 1};
	int idx = char_to_index(item.data[0]);
	return trie->children[idx] ? trie_get(trie->children[idx], next) : false;
}

long trie_depth(trie_t *trie)
{
	bool has_children = false;
	long max_depth = 0;
	for (int i = 0; i < TRIE_CHILDREN; i++) {
		if (trie->children[i]) {
			has_children = true;
			long depth = trie_depth(trie->children[i]);
			if (depth > max_depth) {
				max_depth = depth;
			}
		}
	}
	return has_children ? max_depth + 1 : 0;
}

// Main program ================================================================

void parse_data(trie_t *out_trie, sview_array_t *out_designs, char *src)
{
	[[gnu::cleanup(string_array_deinit)]]
	string_array_t sections = {0};
	array_init(sections, char *);
	split_regex(&sections, src, "\n\n");
	if (sections.size != 2) {
		die("Bad input format.\n");
	}

	[[gnu::cleanup(string_array_deinit)]]
	string_array_t patterns = {0};
	array_init(patterns, char *);
	split_regex(&patterns, sections.data[0], ", ");
	if (patterns.size == 0) {
		die("Bad input format.\n");
	}
	for (long i = 0; i < patterns.size; i++) {
		sview_t temp = sview_from_string(patterns.data[i]);
		trie_insert(out_trie, temp);
	}

	[[gnu::cleanup(string_array_deinit)]]
	string_array_t designs = {0};
	array_init(designs, char *);
	split_regex(&designs, sections.data[1], "\n");
	if (designs.size == 0) {
		die("Bad input format.\n");
	}
	for (long i = 0; i < designs.size; i++) {
		sview_t temp = sview_from_string(designs.data[i]);
		array_push(*out_designs, sview_t, temp);
	}
}

bool validate_design(trie_t *patterns, sview_t design)
{
	if (trie_get(patterns, design)) {
		return true;
	}
	for (long i = 0; i < design.size; i++) {
		sview_t prefix = {.data = design.data, .size = design.size - i};
		sview_t suffix = {.data = design.data + design.size - i, .size = i};
		if (trie_get(patterns, suffix) && validate_design(patterns, prefix)) {
			return true;
		}
	}
	return false;
}

// Unfortunately slower than C++, but I don't have any better ideas.
long count_build_ways(trie_t *patterns, sview_t design, long_array_t *cache)
{
	cache->size = design.size + 1;
	while (cache->size >= cache->cap) {
		cache->cap *= 2;
		cache->data = realloc(cache->data, cache->cap * sizeof(long));
		if (cache->data == nullptr) {
			die("Out of memory.\n");
		}
	}
	for (long i = 0; i < cache->size; i++) {
		cache->data[i] = 0;
	}
	cache->data[0] = 1;

	long depth = trie_depth(patterns);
	for (long i = 0; i <= design.size; i++) {
		for (long j = 0; j <= depth; j++) {
			sview_t substr = {.data = design.data + i - j, .size = j};
			if (i >= j && trie_get(patterns, substr)) {
				cache->data[i] += cache->data[i - j];
			}
		}
	}
	return cache->data[design.size];
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		die("Usage: %s input\n", argv[0]);
	}
	char *data = read_file(argv[1]);

	[[gnu::cleanup(trie_free_wrap)]]
	trie_t *patterns = trie_new();

	[[gnu::cleanup(sview_array_deinit)]]
	sview_array_t designs = {0};
	array_init(designs, sview_t);

	[[gnu::cleanup(long_array_deinit)]]
	long_array_t cache = {0};
	array_init(cache, long);

	parse_data(patterns, &designs, data);

	long count = 0;
	long build_ways = 0;
	for (long i = 0; i < designs.size; i++) {
		if (validate_design(patterns, designs.data[i])) {
			count++;
			build_ways += count_build_ways(patterns, designs.data[i], &cache);
		}
	}
	printf("Valid designs: %ld\n", count);
	printf("Ways to build: %ld\n", build_ways);

	free(data);
	return 0;
}
