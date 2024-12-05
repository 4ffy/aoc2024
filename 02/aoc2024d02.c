#define _GNU_SOURCE

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct array_s {
    int *data;
    size_t size;
    size_t cap;
} array_t;

static void array_init(array_t *a)
{
    a->data = calloc(8, sizeof(int));
    if (a->data == nullptr) {
        fprintf(stderr, "No memory\n");
        exit(1);
    }
    a->size = 0;
    a->cap = 8;
}

static void array_deinit(array_t *a)
{
    free(a->data);
    a->size = 0;
    a->cap = 0;
}

static void array_clear(array_t *a)
{
    for (size_t i = 0; i < a->size; i++) {
        a->data[i] = 0;
    }
    a->size = 0;
}

static void array_push(array_t *a, int x)
{
    a->data[a->size] = x;
    a->size++;
    if (a->size == a->cap) {
        a->cap *= 2;
        int *temp = a->data;
        a->data = realloc(a->data, a->cap * sizeof(int));
        if (a->data == nullptr) {
            fprintf(stderr, "No memory\n");
            free(temp);
            exit(1);
        }
        for (size_t i = a->size; i < a->cap; i++) {
            a->data[i] = 0;
        }
    }
}

static bool array_is_increasing(array_t *a)
{
    for (size_t i = 0; i < a->size - 1; i++) {
        if (a->data[i] >= a->data[i + 1]) {
            return false;
        }
    }
    return true;
}

static bool array_is_decreasing(array_t *a)
{
    for (size_t i = 0; i < a->size - 1; i++) {
        if (a->data[i] <= a->data[i + 1]) {
            return false;
        }
    }
    return true;
}

static bool array_is_smooth(array_t *a)
{
    for (size_t i = 0; i < a->size - 1; i++) {
        if (abs(a->data[i] - a->data[i + 1]) > 3) {
            return false;
        }
    }
    return true;
}

static bool array_is_naively_safe(array_t *a)
{
    return (array_is_increasing(a) || array_is_decreasing(a)) &&
           array_is_smooth(a);
}

static bool array_is_safe(array_t *a)
{
    if (array_is_naively_safe(a)) {
        return true;
    }

    [[gnu::cleanup(array_deinit)]]
    array_t sub_array = {0};
    array_init(&sub_array);

    for (size_t i = 0; i < a->size; i++) {
        array_clear(&sub_array);
        for (size_t j = 0; j < i; j++) {
            array_push(&sub_array, a->data[j]);
        }
        for (size_t j = i + 1; j < a->size; j++) {
            array_push(&sub_array, a->data[j]);
        }
        if (array_is_naively_safe(&sub_array)) {
            return true;
        }
    }
    return false;
}

static void fclose_wrap(FILE **f) { fclose(*f); }

static char *read_file(char const *filename)
{
    [[gnu::cleanup(fclose_wrap)]] FILE *f = fopen(filename, "rb");

    if (f == nullptr) {
        fprintf(stderr, "Could not open \"%s\": %s\n", filename,
                strerror(errno));
        exit(1);
    }

    fseek(f, 0L, SEEK_END);
    size_t length = (size_t)ftell(f);
    rewind(f);

    char *buf = (char *)malloc(length * sizeof(char) + 1);
    if (buf == nullptr) {
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

int main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s input\n", argv[0]);
        exit(1);
    }

    char *data = read_file(argv[1]);

    [[gnu::cleanup(array_deinit)]]
    array_t numbers = {0};
    array_init(&numbers);

    int naive = 0;
    int safe = 0;

    char *data_copy = data;
    char *line = strsep(&data_copy, "\n");
    while (line != nullptr) {
        array_clear(&numbers);

        char *line_copy = line;
        char *digits = strsep(&line_copy, " ");
        while (digits != nullptr) {
            long temp = strtol(digits, nullptr, 10);
            array_push(&numbers, (int)temp);
            digits = strsep(&line_copy, " ");
        }

        if (numbers.size > 1 && array_is_naively_safe(&numbers)) {
            naive++;
        }
        if (numbers.size > 1 && array_is_safe(&numbers)) {
            safe++;
        }

        line = strsep(&data_copy, "\n");
    }

    printf("Naive: %d\n", naive);
    printf("Safe: %d\n", safe);

    free(data);
    return 0;
}
