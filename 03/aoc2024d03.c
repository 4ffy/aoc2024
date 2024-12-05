#include <errno.h>
#include <regex.h>
#include <stdbool.h>
#include <stddef.h>
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
            type *temp = arr.data;                                             \
            arr.data = realloc(arr.data, arr.cap * sizeof(type));              \
            if (arr.data == NULL) {                                            \
                fprintf(stderr, "Out of memory.\n");                           \
                free(temp);                                                    \
                exit(1);                                                       \
            }                                                                  \
        }                                                                      \
    } while (0)

static char token_re[] =
    "do()\\|don't()\\|mul(\\([0-9]\\{0,3\\}\\),\\([0-9]\\{0,3\\}\\))";

typedef enum {
    TOKEN_ENABLE,
    TOKEN_DISABLE,
    TOKEN_MULTIPLY,
} token_type_t;

typedef struct token_s {
    token_type_t type;
    int a;
    int b;
} token_t;

typedef struct token_array_s {
    token_t *data;
    size_t size;
    size_t cap;
} token_array_t;

static token_array_t parse_tokens(char const *src)
{
    [[gnu::cleanup(regfree)]]
    regex_t re = {0};
    regmatch_t match[3] = {0};
    regcomp(&re, token_re, 0);
    token_array_t tokens = {0};
    array_init(tokens, token_t);

    char const *substr = src;
    while (!regexec(&re, substr, 3, match, 0)) {
        char const *match_str = substr + match[0].rm_so;
        regoff_t match_len = match[0].rm_eo - match[0].rm_so;
        token_t token = {0};

        if (!memcmp(match_str, "do()", match_len)) {
            token.type = TOKEN_ENABLE;
        } else if (!memcmp(match_str, "don't()", match_len)) {
            token.type = TOKEN_DISABLE;
        } else {
            token.type = TOKEN_MULTIPLY;
            token.a = (int)strtol(substr + match[1].rm_so, NULL, 10);
            token.b = (int)strtol(substr + match[2].rm_so, NULL, 10);
        }

        array_push(tokens, token_t, token);
        substr += match[0].rm_eo;
    }
    return tokens;
}

static int exec_tokens(token_array_t tokens, bool enable_do)
{
    int sum = 0;
    bool disabled = false;
    for (size_t i = 0; i < tokens.size; i++) {
        token_t token = tokens.data[i];
        switch (token.type) {
        case TOKEN_ENABLE:
            disabled = false;
            break;
        case TOKEN_DISABLE:
            if (enable_do) {
                disabled = true;
            }
            break;
        case TOKEN_MULTIPLY:
            if (!disabled) {
                sum += token.a * token.b;
            }
            break;
        }
    }
    return sum;
}

static void fclose_wrap(FILE **f) { fclose(*f); }

static char *read_file(char const *filename)
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

int main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s input\n", argv[0]);
        exit(1);
    }

    char *data = read_file(argv[1]);
    token_array_t tokens = parse_tokens(data);
    printf("Naive: %d\n", exec_tokens(tokens, false));
    printf("With dos/don'ts: %d\n", exec_tokens(tokens, true));

    array_deinit(tokens);
    free(data);
    return 0;
}
