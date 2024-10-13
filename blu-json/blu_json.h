#ifndef __BLU_JSON_H
#define __BLU_JSON_H

#ifdef __cplusplus
extern "C" {
#endif

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
    BLU_JSON_TYPE_MIXED
};

#define BLU_JSON_NOT_ENOUGH_CTX_SPACE   (-1)
#define BLU_JSON_ERROR_PARSING          (-2)
#define BLU_JSON_INVALID_JSON           (-3)

int blu_json_parse(void* ctx, unsigned int max_ctx_len, const char* json, unsigned int json_len);

#ifdef __cplusplus
}
#endif

#endif /* __BLU_JSON_H */