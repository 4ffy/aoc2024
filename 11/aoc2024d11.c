#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Template ==========================================================

#define MAX_LOAD 0.5

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

// Data Structures ===================================================

typedef struct long_array_s {
	long *data;
	long size;
	long cap;
} long_array_t;

void long_array_deinit(long_array_t *l) { array_deinit(*l); }

typedef struct pair_s {
	long first;
	long second;
} pair_t;

static inline bool pairs_equal(pair_t x, pair_t y)
{
	return x.first == y.first && x.second == y.second;
}

typedef struct cache_entry_s {
	bool has_value;
	pair_t key;
	long value;
} cache_entry_t;

typedef struct get_result_s {
	bool has_value;
	long value;
} get_result_t;

typedef struct cache_s {
	cache_entry_t *data;
	long size;
	long cap;
} cache_t;

void cache_deinit(cache_t *c) { array_deinit(*c); }

uint32_t hash_pair(pair_t pair)
{
	uint8_t *data = (uint8_t *)(&pair);
	size_t length = sizeof(pair_t);
	uint32_t hash = 2166136261u;
	for (size_t i = 0; i < length; i++) {
		hash ^= data[i];
		hash *= 16777619;
	}
	return hash;
}

cache_entry_t *_cache_find(cache_entry_t *entries, long cap, pair_t key)
{
	uint32_t hash = hash_pair(key);
	uint32_t idx = hash & (cap - 1);
	while (true) {
		cache_entry_t *entry = &entries[idx];
		if (!entry->has_value) {
			return entry;
		}
		if (entry->has_value && pairs_equal(key, entry->key)) {
			return entry;
		}
		idx = (idx + 1) & (cap - 1);
	}
}

void _cache_expand(cache_t *cache)
{
	long new_cap = cache->cap * 2;
	cache_entry_t *new_data = calloc(new_cap, sizeof(cache_entry_t));
	if (new_data == nullptr) {
		fprintf(stderr, "Out of memory.\n");
		exit(1);
	}
	cache->size = 0;
	for (long i = 0; i < cache->cap; i++) {
		cache_entry_t entry = cache->data[i];
		if (entry.has_value) {
			cache_entry_t *dest = _cache_find(new_data, new_cap, entry.key);
			*dest = entry;
			cache->size++;
		}
	}
	free(cache->data);
	cache->data = new_data;
	cache->cap = new_cap;
}

bool cache_insert(cache_t *cache, pair_t key, long value)
{
	if ((double)(cache->size + 1) > (double)cache->cap * MAX_LOAD) {
		_cache_expand(cache);
	}
	cache_entry_t *dest = _cache_find(cache->data, cache->cap, key);
	bool is_new = !dest->has_value;
	if (is_new) {
		cache->size++;
	}
	dest->has_value = true;
	dest->key = key;
	dest->value = value;
	return is_new;
}

get_result_t cache_get(cache_t *cache, pair_t key)
{
	get_result_t result = {0};
	result.has_value = false;
	if (cache->size == 0) {
		return result;
	}
	cache_entry_t *entry = _cache_find(cache->data, cache->cap, key);
	if (entry->has_value) {
		result.has_value = true;
		result.value = entry->value;
	}
	return result;
}

// Parsing ===========================================================

static inline bool is_digit(char c) { return c >= '0' && c <= '9'; }

static inline bool is_whitespace(char c)
{
	return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

long parse_number(char *data, long length)
{
	long result = 0;
	for (long i = 0; i < length; i++) {
		char c = data[i];
		if (!is_digit(c)) {
			return LONG_MIN;
		}
		result = 10 * result + c - '0';
	}
	return result;
}

void parse_data(long_array_t *out, char *data)
{
	out->size = 0;
	char *start = data;
	char *curr = data;
	while (true) {
		while (is_digit(*curr) && !is_whitespace(*curr) && *curr != '\0') {
			curr++;
		}
		long number = parse_number(start, curr - start);
		array_push(*out, long, number);

		while (is_whitespace(*curr) && *curr != '0') {
			curr++;
		}
		if (*curr == '\0') {
			return;
		}
		start = curr;
	}
}

// Main Program ======================================================

static inline long digits(long x) { return (long)log10((double)x) + 1; }

static inline long pow_10(long exp) { return (long)pow(10.0, (double)exp); }

static inline pair_t split(long x)
{
	long split = pow_10(digits(x) / 2);
	pair_t result = {.first = x / split, .second = x % split};
	return result;
}

long blink(long x, long gens, cache_t *cache)
{
	if (gens == 0) {
		return 1;
	}

	pair_t key = {.first = x, .second = gens};
	get_result_t cached = cache_get(cache, key);
	if (cached.has_value) {
		return cached.value;
	}

	long result = 0;
	if (x == 0) {
		result = blink(1, gens - 1, cache);
	} else if ((digits(x) & 1) == 0) {
		pair_t temp = split(x);
		result += blink(temp.first, gens - 1, cache);
		result += blink(temp.second, gens - 1, cache);
	} else {
		result += blink(2024 * x, gens - 1, cache);
	}
	cache_insert(cache, key, result);
	return result;
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s input\n", argv[0]);
		exit(1);
	}
	char *data = read_file(argv[1]);

	[[gnu::cleanup(long_array_deinit)]]
	long_array_t list = {0};
	array_init(list, long);
	parse_data(&list, data);

	[[gnu::cleanup(cache_deinit)]]
	cache_t cache = {0};
	array_init(cache, cache_entry_t);

	long sum = 0;
	for (long i = 0; i < list.size; i++) {
		sum += blink(list.data[i], 25, &cache);
	}
	printf("25 blinks: %ld\n", sum);

	sum = 0;
	for (long i = 0; i < list.size; i++) {
		sum += blink(list.data[i], 75, &cache);
	}
	printf("75 blinks: %ld\n", sum);

	free(data);
	return 0;
}
