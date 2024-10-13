#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include "blu_json.h"

/*************************
 * Defines
 ************************/

#define __PACKED __attribute__((packed))

#define JSON_OBJ_START  '{'
#define JSON_OBJ_END    '}'
#define JSON_ARR_START  '['
#define JSON_ARR_END    ']'
#define JSON_STR_QUOTE  '"'
#define JSON_COLON      ':'
#define JSON_COMMA      ','
#define JSON_SPACE      ' '

#define JSON_NULL_STR   "null"
#define JSON_TRUE_STR   "true"
#define JSON_FALSE_STR  "false"

#define JSON_FLOAT_SCI_NOT      'E'
#define JSON_FLOAT_DOT          '.'

#define TOKEN_TYPE_BITMASK  (0x0f)
#define DATA_TYPE_BITMASK   (0xf0)

#define GET_TOKEN_TYPE(_x)          ((_x) & TOKEN_TYPE_BITMASK)
#define SET_TOKEN_TYPE(_x, _val)    (((_x) | (_val)))

#define GET_DATA_TYPE(_x)           (((_x) & DATA_TYPE_BITMASK) >> 4)
#define SET_DATA_TYPE(_x, _val)     (((_x) | ((_val) << 4)))

#define INVALID_OFFSET              (UINT16_MAX)

#define LOG(...) printf(__VA_ARGS__)

/*************************
 * Structures
 ************************/
struct parser_state_t {
    char* ctx_p;
    char* ctx_end_p;
    uint16_t cur_ctx_off;

    const char* json_p;
    const char* json_end_p;
    uint16_t cur_json_off;
    
    uint16_t cur_parent_off;
    uint16_t cur_key_off;
    uint8_t cur_key_len;

    uint16_t last_sibling_off;
};

typedef struct __PACKED {
    uint8_t len;
    uint16_t json_off;
} string_t;

typedef struct __PACKED {
    string_t key;
    unsigned char type;
    uint16_t parent_ctx_off;
    uint16_t sibling_ctx_off;
} json_token_t;

/* Object Data Types */
typedef struct __PACKED {
    json_token_t t;
    uint16_t json_start_off;
    uint16_t len;
} obj_t;

typedef obj_t arr_t;

/* Primitive Data Types */
typedef struct __PACKED {
    json_token_t t;
    int int_val;
} prim_int_t;

typedef struct __PACKED {
    json_token_t t;
    float float_val;
} prim_float_t;

typedef struct __PACKED {
    json_token_t t;
} prim_null_t;

typedef struct __PACKED {
    json_token_t t;
    uint8_t bool_val;
} prim_bool_t;

typedef struct __PACKED {
    json_token_t t;
    string_t val;
} prim_string_t;

typedef struct __PACKED {
    union {
        obj_t _obj;
        arr_t _arr;
        prim_int_t _int;
        prim_float_t _float;
        prim_null_t _null;
        prim_bool_t _bool;
        prim_string_t _str;
    } u;
} json_type_t;

static int move_to_char(struct parser_state_t *state_p, char c, bool is_move_off_past)
{
    uint16_t i = state_p->cur_json_off;
    uint16_t jump = 0;
    bool is_in_bounds = state_p->json_p + i < state_p->json_end_p;
    if (!is_in_bounds)
        return BLU_JSON_ERROR_PARSING;
    
    bool is_not_found = state_p->json_p[i] != c;
    
    while (is_in_bounds && is_not_found) {
        is_in_bounds = ++i + state_p->json_p < state_p->json_end_p;

        if (!is_in_bounds)
            return BLU_JSON_ERROR_PARSING;
        
        is_not_found = state_p->json_p[i] != c;
    }
    
    jump = i - state_p->cur_json_off;

    if (is_move_off_past) {
        is_in_bounds = state_p->json_p + i + 1 < state_p->json_end_p;
        if (!is_in_bounds)
            return BLU_JSON_ERROR_PARSING;
        ++i;
    }

    state_p->cur_json_off = i;
    return is_not_found ? BLU_JSON_ERROR_PARSING : (int)jump;
}

static int move_while_char_eq(struct parser_state_t *state_p, const char *skip_chars, uint8_t num_skip_chars)
{
    int i = 0;
    bool is_match = false;
    bool is_in_bounds = state_p->json_p + state_p->cur_json_off < state_p->json_end_p;
    if (!is_in_bounds)
        return BLU_JSON_ERROR_PARSING;
    
    for (uint8_t j = 0; j < num_skip_chars; ++j) {
        is_match = skip_chars[j] == state_p->json_p[state_p->cur_json_off];
        if (is_match)
            break;
    }

    while (is_in_bounds && is_match) {
        is_in_bounds = ++i + state_p->json_p + state_p->cur_json_off < state_p->json_end_p;

        if (!is_in_bounds)
            return BLU_JSON_ERROR_PARSING;
        
        for (uint8_t j = 0; j < num_skip_chars; ++j) {
            is_match = skip_chars[j] == state_p->json_p[state_p->cur_json_off + i];
            if (is_match)
                break;
        }
    }

    state_p->cur_json_off += i;
    return is_match ? BLU_JSON_ERROR_PARSING : i;
}

static int push_obj_arr_token(struct parser_state_t *state_p, enum blu_json_val_types_e type)
{
    uint8_t* ctx_mark_p = state_p->ctx_p + state_p->cur_ctx_off;
    if ((ctx_mark_p + sizeof(obj_t)) >= state_p->ctx_end_p) {
        return BLU_JSON_NOT_ENOUGH_CTX_SPACE;
    }

    obj_t* p = (obj_t*)ctx_mark_p;
    p->t.key.len = state_p->cur_key_len;
    p->t.key.json_off = state_p->cur_key_off;
    p->t.type = 0;
    p->t.type = SET_TOKEN_TYPE(p->t.type, type);
    p->t.type = SET_DATA_TYPE(p->t.type, BLU_JSON_TYPE_MIXED);
    p->t.parent_ctx_off = state_p->cur_parent_off;
    p->t.sibling_ctx_off = INVALID_OFFSET;
    p->json_start_off = state_p->cur_json_off;
    p->len = 0;

    if (state_p->cur_parent_off == INVALID_OFFSET) {
        state_p->cur_parent_off = 0;
    } else {
        state_p->cur_parent_off = state_p->cur_ctx_off;
    }

    state_p->cur_ctx_off += sizeof(obj_t);
    return 0;
}

static inline int push_obj_token(struct parser_state_t *state_p)
{
    return push_obj_arr_token(state_p, BLU_JSON_VAL_OBJ);
}

static inline int push_arr_token(struct parser_state_t *state_p)
{
    return push_obj_arr_token(state_p, BLU_JSON_VAL_ARRAY);
}

static int get_next_key(struct parser_state_t *state_p)
{
    int rc = move_to_char(state_p, JSON_STR_QUOTE, true);
    if (rc < 0)
        return rc;
    
    if (state_p->json_p[state_p->cur_json_off] == JSON_STR_QUOTE)
        /* key cant be empty */
        return BLU_JSON_INVALID_JSON;

    state_p->cur_key_off = state_p->cur_json_off;
    state_p->cur_key_len = 0;
    
    rc = move_to_char(state_p, JSON_STR_QUOTE, true);

    if (rc < 0) {
        return rc;
    }
    
    state_p->cur_key_len += rc;

    /* move cur json offset passed colon */
    rc = move_to_char(state_p, JSON_COLON, true);
    if (rc < 0) {
        return rc;
    }
    
    if (++state_p->cur_json_off + state_p->json_p >= state_p->json_end_p)
        return BLU_JSON_INVALID_JSON;
    
    LOG("Retrieved key %.4s", &state_p->json_p[state_p->cur_key_off]);

    return 0;
}

static int get_value_type(
    struct parser_state_t *state_p, enum blu_json_val_types_e *val_type_p)
{
    const char skip_chars[] = {' ', JSON_COLON, '\\'};
    int rc = move_while_char_eq(state_p, skip_chars, sizeof(skip_chars));
    if (rc < 0)
        return rc;
    
    switch (state_p->json_p[state_p->cur_json_off])
    {
    case JSON_OBJ_START:
        *val_type_p = BLU_JSON_VAL_OBJ;
        return 0;
    case JSON_ARR_START:
        *val_type_p = BLU_JSON_VAL_ARRAY;
        return 0;
    case JSON_OBJ_END:
    case JSON_ARR_END:
    case JSON_COLON:
        return BLU_JSON_ERROR_PARSING;
    default:
        *val_type_p = BLU_JSON_VAL_PRIMITIVE;
        return 0;
    }
}

static int infer_prim_type(const char *c, uint16_t max_len, enum blu_json_datatypes_e *prim_type_p)
{
    /* string */
    if (*c == JSON_STR_QUOTE) {
        *prim_type_p = BLU_JSON_TYPE_STRING;
        return 0;
    }

    /* null or bool */
    if (isalpha(*c)) {
        /* is alpha */
        if (strcmp(c, JSON_NULL_STR) == 0) {
            *prim_type_p = BLU_JSON_TYPE_NULL;
            return 0;
        } else if (strcmp(c, JSON_TRUE_STR) == 0 || strcmp(c, JSON_FALSE_STR) == 0) {
            *prim_type_p = BLU_JSON_TYPE_BOOL;
            return 0;
        }
        return BLU_JSON_ERROR_PARSING;
    }

    /* number */
    uint16_t i = 0;
    *prim_type_p = BLU_JSON_TYPE_INT;
    while ( i < max_len &&
        (c[i] != JSON_COMMA && c[i] != JSON_ARR_END && c[i] != JSON_OBJ_END && c[i] != JSON_SPACE)) {
        
        switch (c[i++])
        {
        case JSON_FLOAT_SCI_NOT:
        case JSON_FLOAT_DOT:
            *prim_type_p = BLU_JSON_TYPE_FLOAT;
            return 0;
        default:
            break;
        }

    }
    return 0;
}

static int str2int_and_move(struct parser_state_t *state_p, int *out_p)
{
    *out_p = atoi(&state_p->json_p[state_p->cur_json_off]);
    return move_to_char(state_p, JSON_COMMA, true);
}

static int str2float_and_move(struct parser_state_t *state_p, float *out_p)
{
    int move = 0;
    const char *str_p = &state_p->json_p[state_p->cur_json_off];
    char *end_p = state_p->json_end_p;

    *out_p = (float)strtod(str_p, &end_p);
    
    move = end_p - str_p;
    if (move <= 0)
        return BLU_JSON_ERROR_PARSING;

    state_p->cur_json_off += move;
    return move_to_char(state_p, JSON_COMMA, true);
}

static int str2str_and_move(struct parser_state_t *state_p, uint8_t *len_out_p, uint16_t *json_off_out_p)
{
    *json_off_out_p = state_p->cur_json_off;
    int rc = move_to_char(state_p, JSON_STR_QUOTE, false);
    if (rc < 0)
        return rc;
    if (rc > UINT8_MAX)
        return BLU_JSON_ERROR_PARSING;

    *len_out_p = (uint8_t)rc;
    
    return move_to_char(state_p, JSON_COMMA, true);
}

static int str2bool_and_move(struct parser_state_t *state_p, uint8_t *out_p)
{
    if (strcmp(JSON_TRUE_STR, &state_p->json_p[state_p->cur_json_off]) == 0)
        *out_p = true;
    else if (strcmp(JSON_FALSE_STR, &state_p->json_p[state_p->cur_json_off]) == 0)
        *out_p = false;
    else
        return BLU_JSON_ERROR_PARSING;

    return move_to_char(state_p, JSON_COMMA, true);
}

static int push_prim_token(struct parser_state_t *state_p)
{
    json_token_t token;
    enum blu_json_datatypes_e data_type;
    json_type_t type_token;
    uint8_t push_ctx_size = 0;
    
    int rc = infer_prim_type(
        &state_p->json_p[state_p->cur_json_off],
        state_p->json_end_p - state_p->json_p - state_p->cur_json_off, &data_type);
    
    if (rc < 0)
        return rc;
    
    int remaining_ctx_space = state_p->ctx_end_p - state_p->ctx_p - state_p->cur_ctx_off;
    if (remaining_ctx_space < sizeof(json_token_t))
        return BLU_JSON_NOT_ENOUGH_CTX_SPACE;

    /* populate token header */
    token.key.len = state_p->cur_key_len;
    token.key.json_off = state_p->cur_key_off;
    token.type = 0;
    token.type = SET_TOKEN_TYPE(token.type, BLU_JSON_VAL_PRIMITIVE);
    token.type = SET_DATA_TYPE(token.type, data_type);
    token.parent_ctx_off = state_p->cur_parent_off;

    token.sibling_ctx_off = INVALID_OFFSET;

    switch (data_type)
    {
    case BLU_JSON_TYPE_INT:
        memcpy(&type_token.u._int.t, &token, sizeof(json_token_t));
        push_ctx_size = sizeof(prim_int_t);
        rc = str2int_and_move(state_p, &type_token.u._int.int_val);
        break;
    case BLU_JSON_TYPE_FLOAT:
        memcpy(&type_token.u._float.t, &token, sizeof(json_token_t));
        push_ctx_size = sizeof(prim_float_t);
        rc = str2float_and_move(state_p, &type_token.u._float.float_val);
        break;
    case BLU_JSON_TYPE_STRING:
        memcpy(&type_token.u._str.t, &token, sizeof(json_token_t));
        push_ctx_size = sizeof(prim_string_t);
        rc = str2str_and_move(state_p, &type_token.u._str.val.len, &type_token.u._str.val.json_off);
        break;
    case BLU_JSON_TYPE_BOOL:
        memcpy(&type_token.u._bool.t, &token, sizeof(json_token_t));
        push_ctx_size = sizeof(prim_bool_t);
        rc = str2bool_and_move(state_p, &type_token.u._bool.bool_val);
        break;
    case BLU_JSON_TYPE_NULL:
        memcpy(&type_token.u._null.t, &token, sizeof(json_token_t));
        push_ctx_size = sizeof(prim_null_t);
        break;
    default:
        return BLU_JSON_ERROR_PARSING;
    }
    if (rc < 0)
        return rc;

    if (remaining_ctx_space < push_ctx_size)
        return BLU_JSON_NOT_ENOUGH_CTX_SPACE;

    if (state_p->last_sibling_off != INVALID_OFFSET) {
        /* set last siblings offset to point to me */
        json_token_t* older_sibling_p = (json_token_t*)&state_p->ctx_p[state_p->last_sibling_off];
        older_sibling_p->sibling_ctx_off = state_p->cur_ctx_off;
    }

    /* set last sibling offset for next youger sibling */
    state_p->last_sibling_off = state_p->cur_ctx_off;

    /* push prim token to ctx buf */
    memcpy(&state_p->ctx_p[state_p->cur_ctx_off], &type_token, push_ctx_size);
    state_p->cur_ctx_off += push_ctx_size;
    return 0;
}

int blu_json_parse(void* ctx, unsigned int max_ctx_len, const char* json, unsigned int json_len)
{
    int rc = 0;
    enum blu_json_val_types_e val_type = BLU_JSON_VAL_OBJ;
    enum blu_json_datatypes_e data_type;

    struct parser_state_t state = {
        .ctx_p = ctx,
        .ctx_end_p = ctx + max_ctx_len,
        .cur_ctx_off = 0,
        .json_p = json,
        .json_end_p = json + json_len,
        .cur_json_off = 0,
        .cur_parent_off = INVALID_OFFSET,
        .cur_key_off = INVALID_OFFSET,
        .cur_key_len = 0,
        .last_sibling_off = INVALID_OFFSET
    };

    if (json == NULL)
        return BLU_JSON_INVALID_JSON;

    if (ctx == NULL || max_ctx_len < sizeof(obj_t))
        return BLU_JSON_NOT_ENOUGH_CTX_SPACE;
    
    memset(state.ctx_p, 0x00, max_ctx_len);

    /* Find start of JSON */
    rc = move_to_char(&state, JSON_OBJ_START, true);
    if (rc < 0)
        return rc;

    /* Push first object */
    rc = push_obj_token(&state);
    if (rc < 0) {
        return rc;
    }

    while (state.json_p + state.cur_json_off < state.ctx_end_p) {

        /* find next key */
        rc = get_next_key(&state);
        if (rc < 0)
            return rc;

        rc = get_value_type(&state, &val_type);
        if (rc < 0)
            return rc;
        
        switch (val_type)
        {
        case BLU_JSON_VAL_OBJ:
            rc = push_obj_token(&state);
            break;
        case BLU_JSON_VAL_ARRAY:
            rc = push_obj_token(&state);
            // TODO: Array can not go back to get_next_key
            break;
        case BLU_JSON_VAL_PRIMITIVE:
            rc = push_prim_token(&state);
            break;
        default:
            break;
        }

        if (rc < 0)
            return rc;
    }
}