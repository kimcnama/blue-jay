#ifndef __BLU_JSON_H
#define __BLU_JSON_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdarg.h>

enum blu_json_val_types_e {
    BLU_JSON_VAL_PRIMITIVE = 0,
    BLU_JSON_VAL_OBJ,
    BLU_JSON_VAL_ARRAY
};

enum blu_json_datatypes_e {
    BLU_JSON_TYPE_INT = 0,
    BLU_JSON_TYPE_FLOAT,
    BLU_JSON_TYPE_STRING,
    BLU_JSON_TYPE_BOOL,
    BLU_JSON_TYPE_NULL,
    BLU_JSON_TYPE_MIXED,
    BLU_JSON_TYPE_NONE
};

#define BLU_JSON_NOT_ENOUGH_CTX_SPACE   (-1)
#define BLU_JSON_ERROR_PARSING          (-2)
#define BLU_JSON_INVALID_JSON           (-3)
#define BLU_JSON_KEY_NOT_FOUND          (-4)
#define BLU_JSON_KEY_INVALID_PARAMS     (-5)
#define BLU_JSON_KEY_PARAM_NOT_INIT     (-6)
#define BLU_JSON_INVALID_DEST_BUF       (-7)
#define BLU_JSON_TYPES_DIFF_SIZES       (-8)
#define BLU_JSON_INVALID_REQ            (-9)

struct blu_json_key_str_t {
    uint8_t len;
    const char *key;
};

struct blu_json_key_metadata_t {
    enum blu_json_val_types_e obj_type;
    enum blu_json_datatypes_e data_type;
};

struct blu_json_key_t {
    struct blu_json_key_str_t key;
    struct blu_json_key_metadata_t meta;
};

int blu_json_parse(void *ctx, unsigned int max_ctx_len, const char* json, unsigned int json_len);
void blu_json_print_tokens(void *ctx, const char* json);

struct key_it_state_t {
    const void *ctx;
    uint16_t ctx_len;
    const char* json;
    unsigned int json_len;
    uint32_t is_init_magic;
    uint16_t last_ctx_off;
};

int blu_json_key_it_init(struct key_it_state_t *it, const void *ctx, const char* json, unsigned int json_len);
bool blu_json_key_it(struct key_it_state_t *it, struct blu_json_key_t *key, struct blu_json_key_str_t *parents, uint8_t max_parents, uint8_t *num_parents_out);

int blu_json_get_primitive_value(const void *ctx, const char* json, unsigned int json_len, void* buf, unsigned int buf_len, unsigned int num_keys, ...);

#ifdef __cplusplus
}
#endif

#endif /* __BLU_JSON_H */