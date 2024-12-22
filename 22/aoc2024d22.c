#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
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
		if ((arr).data == nullptr) {                                           \
			die("Out of memory.\n");                                           \
		}                                                                      \
		(arr).size = 0;                                                        \
		(arr).cap = 8;                                                         \
	} while (false)

#define array_deinit(arr)                                                      \
	do {                                                                       \
		free((arr).data);                                                      \
		(arr).size = 0;                                                        \
		(arr).cap = 0;                                                         \
	} while (false)

#define array_push(arr, type, item)                                            \
	do {                                                                       \
		(arr).data[(arr).size++] = (item);                                     \
		if ((arr).size == (arr).cap) {                                         \
			(arr).cap *= 2;                                                    \
			(arr).data = realloc((arr).data, (arr).cap * sizeof(type));        \
			if ((arr).data == nullptr) {                                       \
				die("Out of memory.\n");                                       \
			}                                                                  \
		}                                                                      \
	} while (false)

void fclose_wrap(FILE **f) { fclose(*f); }

char *read_file(char const *filename)
{
	[[gnu::cleanup(fclose_wrap)]] FILE *f = fopen(filename, "rb");
	if (f == nullptr) {
		die("Could not open \"%s\": %s\n", filename, strerror(errno));
	}
	fseek(f, 0L, SEEK_END);
	long length = ftell(f);
	rewind(f);
	char *buf = (char *)malloc(length * sizeof(char) + 1);
	if (buf == nullptr) {
		die("Could not allocate buffer: %s\n", strerror(errno));
	}
	long read = fread(buf, sizeof(char), length, f);
	if (read < length) {
		die("Could not read file \"%s\"\n", filename);
	}
	buf[read] = '\0';
	return buf;
}

// Data structures =============================================================

typedef struct long_array_s long_array_t;
struct long_array_s {
	long *data;
	long size;
	long cap;
};

void long_array_deinit(long_array_t *l) { array_deinit(*l); }

#define CACHE_MAX_LOAD 0.75

typedef struct entry_s entry_t;
struct entry_s {
	bool has_value;
	char key[4];
	long value;
};

typedef struct cache_s cache_t;
struct cache_s {
	entry_t *data;
	long size;
	long cap;
};

void cache_deinit(cache_t *c) { array_deinit(*c); }

uint32_t hash_key(char key[4])
{
	uint8_t *data = (uint8_t *)(key);
	uint32_t hash = 2166136261u;
	for (size_t i = 0; i < 4; i++) {
		hash ^= data[i];
		hash *= 16777619;
	}
	return hash;
}

static inline bool keys_equal(char a[4], char b[4])
{
	for (int i = 0; i < 4; i++) {
		if (a[i] != b[i]) {
			return false;
		}
	}
	return true;
}

entry_t *_cache_find(entry_t *entries, long cap, char key[4])
{
	uint32_t hash = hash_key(key);
	uint32_t idx = hash & (cap - 1);
	while (true) {
		entry_t *entry = &entries[idx];
		if (!entry->has_value) {
			return entry;
		}
		if (entry->has_value && keys_equal(entry->key, key)) {
			return entry;
		}
		idx = (idx + 1) & (cap - 1);
	}
}

void _cache_expand(cache_t *cache)
{
	long new_cap = cache->cap * 2;
	entry_t *new_data = calloc(new_cap, sizeof(entry_t));
	if (new_data == nullptr) {
		die("Out of memory.\n");
	}
	cache->size = 0;
	for (long i = 0; i < cache->cap; i++) {
		entry_t entry = cache->data[i];
		if (entry.has_value) {
			entry_t *dest = _cache_find(new_data, new_cap, entry.key);
			*dest = entry;
			cache->size++;
		}
	}
	free(cache->data);
	cache->data = new_data;
	cache->cap = new_cap;
}

bool cache_insert(cache_t *cache, char key[4], long value)
{
	if ((double)(cache->size + 1) > (double)cache->cap * CACHE_MAX_LOAD) {
		_cache_expand(cache);
	}
	entry_t *dest = _cache_find(cache->data, cache->cap, key);
	bool is_new = !dest->has_value;
	if (is_new) {
		cache->size++;
	}
	dest->has_value = true;
	for (int i = 0; i < 4; i++) {
		dest->key[i] = key[i];
	}
	dest->value = value;
	return is_new;
}

entry_t cache_get(cache_t *cache, char key[4])
{
	entry_t result = {0};
	result.has_value = false;
	if (cache->size == 0) {
		return result;
	}
	entry_t *entry = _cache_find(cache->data, cache->cap, key);
	if (entry->has_value) {
		result.has_value = true;
		for (int i = 0; i < 4; i++) {
			result.key[i] = key[i];
		}
		result.value = entry->value;
	}
	return result;
}

void cache_clear(cache_t *cache)
{
	cache->size = 0;
	for (long i = 0; i < cache->cap; i++) {
		cache->data[i].has_value = false;
	}
}

// Parsing =====================================================================

static inline bool is_digit(char c) { return c >= '0' && c <= '9'; }

void parse_data(long_array_t *out, char *data)
{
	out->size = 0;
	char *curr = data;
	while (curr[0] != '\0') {
		while (curr[0] != '\0' && !is_digit(curr[0])) {
			curr++;
		}
		if (curr[0] == '\0') {
			break;
		}
		array_push(*out, long, strtol(curr, &curr, 10));
	}
}

// Main program ================================================================

static inline long next_secret(long x)
{
	long sub_1 = (x ^ (x << 6)) & 0xffffff;
	long sub_2 = (sub_1 ^ (sub_1 >> 5)) & 0xffffff;
	long sub_3 = (sub_2 ^ (sub_2 << 11)) & 0xffffff;
	return sub_3;
}

long nth_secret(long x, long n)
{
	long result = x;
	for (long i = 0; i < n; i++) {
		result = next_secret(result);
	}
	return result;
}

void all_secrets(long_array_t *out, long x, long n)
{
	out->size = 0;
	if (out->cap < n) {
		out->cap = n + 10; // whatever
		out->data = realloc(out->data, out->cap * sizeof(long));
		if (!out->data) {
			die("Out of memory.\n");
		}
	}
	long secret = x;
	for (long i = 0; i < n; i++) {
		array_push(*out, long, secret);
		secret = next_secret(secret);
	}
}

void find_sell_times(cache_t *cache, long_array_t secrets)
{
	cache_clear(cache);
	for (int i = 4; i < secrets.size; i++) {
		long prices[5] = {secrets.data[i - 4] % 10, secrets.data[i - 3] % 10,
						  secrets.data[i - 2] % 10, secrets.data[i - 1] % 10,
						  secrets.data[i] % 10};
		char deltas[4] = {
			(char)(prices[1] - prices[0]),
			(char)(prices[2] - prices[1]),
			(char)(prices[3] - prices[2]),
			(char)(prices[4] - prices[3]),
		};
		char price = prices[4];
		if (!cache_get(cache, deltas).has_value) {
			cache_insert(cache, deltas, price);
		}
	}
}

void sum_sell_times(cache_t *cache, long_array_t numbers, long n)
{
	[[gnu::cleanup(long_array_deinit)]]
	long_array_t secrets = {0};
	array_init(secrets, long);

	[[gnu::cleanup(cache_deinit)]]
	cache_t cache_2 = {0};
	array_init(cache_2, entry_t);

	for (long i = 0; i < numbers.size; i++) {
		all_secrets(&secrets, numbers.data[i], n);
		find_sell_times(&cache_2, secrets);
		for (long j = 0; j < cache_2.cap; j++) {
			entry_t sell_time = cache_2.data[j];
			if (sell_time.has_value) {
				entry_t temp = cache_get(cache, sell_time.key);
				if (temp.has_value) {
					cache_insert(cache, sell_time.key,
								 temp.value + sell_time.value);
				} else {
					cache_insert(cache, sell_time.key, temp.value);
				}
			}
		}
	}
}

long best_sell_time(cache_t cache)
{
	long result = 0;
	for (int i = 0; i < cache.cap; i++) {
		entry_t entry = cache.data[i];
		if (entry.has_value && entry.value > result) {
			result = entry.value;
		}
	}
	return result;
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		die("Usage: %s input\n", argv[0]);
	}
	char *data = read_file(argv[1]);

	[[gnu::cleanup(long_array_deinit)]]
	long_array_t numbers = {0};
	array_init(numbers, long);
	parse_data(&numbers, data);

	long sum = 0;
	for (long i = 0; i < numbers.size; i++) {
		sum += nth_secret(numbers.data[i], 2000);
	}
	printf("Sum of 2000th: %ld\n", sum);

	[[gnu::cleanup(cache_deinit)]]
	cache_t cache = {0};
	array_init(cache, entry_t);
	sum_sell_times(&cache, numbers, 2000);

	printf("Best sell result: %ld\n", best_sell_time(cache));

	free(data);
	return 0;
}
