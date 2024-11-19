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

#define JSON_NULL_STR_SIZE  (sizeof(JSON_NULL_STR) - 1)
#define JSON_TRUE_STR_SIZE  (sizeof(JSON_TRUE_STR) - 1)
#define JSON_FALSE_STR_SIZE (sizeof(JSON_FALSE_STR) - 1)

#define JSON_FLOAT_SCI_NOT      'E'
#define JSON_FLOAT_DOT          '.'
#define JSON_NEG_NUM            '-'

#define SIZEOF_CTX_LEN          (2)

#define TOKEN_TYPE_BITMASK  (0x0f)
#define DATA_TYPE_BITMASK   (0xf0)

#define GET_TOKEN_TYPE(_x)          ((_x) & TOKEN_TYPE_BITMASK)
#define SET_TOKEN_TYPE(_x, _val)    (((_x) | (_val)))

#define GET_DATA_TYPE(_x)           (((_x) & DATA_TYPE_BITMASK) >> 4)
#define SET_DATA_TYPE(_x, _val)     (((_x) | ((_val) << 4)))

#define INVALID_OFFSET              (UINT16_MAX)

#define INIT_MAGIC  (0xDEADBEEFU)

#define LOG(...) printf(__VA_ARGS__)
#define CALL_LOG_FUNC(_func)    (_func)

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
    
    uint16_t cur_parent_ctx_off;
    uint16_t cur_key_json_off;
    uint8_t cur_key_len;

    uint16_t last_sibling_ctx_off;
    uint16_t* ctx_len_p;
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

/*************************
 * Logging Functions
 ************************/
static void _log_token(const json_token_t* p, const char *json, const uint8_t *ctx)
{
    const char *val_type_strings[] = {
        [BLU_JSON_VAL_PRIMITIVE] =  "PRIMITIVE",
        [BLU_JSON_VAL_OBJ] =        "OBJECT",
        [BLU_JSON_VAL_ARRAY] =      "ARRAY"
    };
    const char *datatype_strings[] = {
        [BLU_JSON_TYPE_INT] =       "INT",
        [BLU_JSON_TYPE_FLOAT] =     "FLOAT",
        [BLU_JSON_TYPE_STRING] =    "STRING",
        [BLU_JSON_TYPE_BOOL] =      "BOOL",
        [BLU_JSON_TYPE_NULL] =      "NULL",
        [BLU_JSON_TYPE_MIXED] =     "MIXED"
    };
    
    LOG("\n\nToken: %.*s \n", p->key.len, &json[p->key.json_off == INVALID_OFFSET ? 0 : p->key.json_off]);
    LOG("   Type %s\n", val_type_strings[GET_TOKEN_TYPE(p->type)]);
    LOG("   Data %s\n", datatype_strings[GET_DATA_TYPE(p->type)]);

    if (p->parent_ctx_off == INVALID_OFFSET) {
        LOG("   Parent: Invalid \n");
    } else {
        const json_token_t* parent = (const json_token_t*)&ctx[p->parent_ctx_off];
        LOG("   Parent: %.*s \n", parent->key.len, &json[parent->key.json_off == INVALID_OFFSET ? 0 : parent->key.json_off]);
    }

    if (p->sibling_ctx_off == INVALID_OFFSET) {
        LOG("   Sibling: Invalid \n");
    } else {
        const json_token_t* sib = (const json_token_t*)&ctx[p->sibling_ctx_off];
        LOG("   Sibling: %.*s \n", sib->key.len, json[sib->key.json_off == INVALID_OFFSET ? 0 : sib->key.json_off]);
    }

    if (GET_TOKEN_TYPE(p->type) == BLU_JSON_VAL_PRIMITIVE) {
        switch (GET_DATA_TYPE(p->type))
        {
        case BLU_JSON_TYPE_INT: {
            prim_int_t* val = (prim_int_t*)p; 
            LOG("   Value (int): %d \n", val->int_val);
            break;
        }
        case BLU_JSON_TYPE_FLOAT: {
            prim_float_t* val = (prim_float_t*)p; 
            LOG("   Value (float): %.6f \n", val->float_val);
            break;
        }
        case BLU_JSON_TYPE_STRING: {
            prim_string_t *str = (prim_string_t *)p;
            LOG("   Value (str): %.*s \n", str->val.len, json[str->val.json_off == INVALID_OFFSET ? 0 : str->val.json_off]);
            break;
        }
        case BLU_JSON_TYPE_BOOL: {
            prim_bool_t* val = (prim_bool_t*)p; 
            LOG("   Value (bool): %u \n", val->bool_val);
            break;
        }
        case BLU_JSON_TYPE_NULL: {
            LOG("   Value (null) \n");
            break;
        }
        case BLU_JSON_TYPE_MIXED: {
            LOG("   Value (mixed) \n");
            break;
        }
        default:
            break;
        }
    }
}

/*************************
 * Implementation
 ************************/
static int move_to_any_char(struct parser_state_t *state_p, const char *c, uint8_t num_chars, bool is_move_off_past)
{
    uint16_t i = state_p->cur_json_off;
    uint16_t jump = 0;

    bool is_found = false;
    bool is_in_bounds = state_p->json_p + i < state_p->json_end_p;
    
    if (!is_in_bounds)
        return BLU_JSON_ERROR_PARSING;

    for (uint8_t j = 0; j < num_chars; ++j) {
        is_found = c[j] == state_p->json_p[state_p->cur_json_off];
        if (is_found)
            break;
    }
    
    while (is_in_bounds && !is_found) {
        is_in_bounds = ++i + state_p->json_p < state_p->json_end_p;

        if (!is_in_bounds)
            return BLU_JSON_ERROR_PARSING;

        for (uint8_t j = 0; j < num_chars; ++j) {
            is_found = c[j] == state_p->json_p[i];
            if (is_found)
                break;
        }
    }
    
    jump = i - state_p->cur_json_off;

    if (is_move_off_past) {
        is_in_bounds = state_p->json_p + i + 1 < state_p->json_end_p;
        if (!is_in_bounds)
            return BLU_JSON_ERROR_PARSING;
        ++i;
    }

    state_p->cur_json_off = i;
    return is_found ? (int)jump : BLU_JSON_ERROR_PARSING;
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

static inline int move_json_offset(struct parser_state_t *state_p, uint16_t jump)
{
    if (state_p->json_p + state_p->cur_json_off + jump >= state_p->json_end_p)
        return BLU_JSON_ERROR_PARSING;
    state_p->cur_json_off += jump;
    return (int)jump;
}

static inline bool is_closing_object(struct parser_state_t *state_p, enum blu_json_val_types_e *closing_type_p)
{
    /* do not move just check */
    switch (state_p->json_p[state_p->cur_json_off])
    {
    case JSON_OBJ_END:
        *closing_type_p = BLU_JSON_VAL_OBJ;
        return true;
    case JSON_ARR_END:
        *closing_type_p = BLU_JSON_VAL_ARRAY;
        return true;
    default:
        return false;
    }
}

// Return 0 if not closed, 1 if object closed, 2 entire json closed , <0 on error
static int check_and_close_object(struct parser_state_t *state_p, enum blu_json_val_types_e *new_cur_parent_p)
{
    enum blu_json_val_types_e type;
    int rc = move_json_offset(state_p, 0);
    if (rc < 0)
        return rc;

    if (!is_closing_object(state_p, &type))
        return 0;
    
    /* is closing object */
    obj_t* obj_p = (obj_t*)&state_p->ctx_p[state_p->cur_parent_ctx_off];
    obj_p->len = state_p->cur_json_off + 1 - obj_p->json_start_off;
    
    /* Change current parent info to parents parent */
    state_p->cur_parent_ctx_off = obj_p->t.parent_ctx_off;

    rc = move_json_offset(state_p, 1);

    if (state_p->cur_parent_ctx_off == INVALID_OFFSET && type == BLU_JSON_VAL_OBJ) {
        return 2;
    }
    
    /* set current parent type */
    obj_p = (obj_t*)&state_p->ctx_p[state_p->cur_parent_ctx_off];
    state_p->cur_key_json_off = obj_p->t.key.json_off;
    state_p->cur_key_len = obj_p->t.key.len;
    *new_cur_parent_p = GET_TOKEN_TYPE(obj_p->t.type);

    return rc < 0 ? rc : 1;
}

static inline void check_and_update_ctx_size(struct parser_state_t *state_p)
{
    if (*state_p->ctx_len_p < state_p->cur_ctx_off)
        *state_p->ctx_len_p = state_p->cur_ctx_off;
}

static int push_obj_arr_token(struct parser_state_t *state_p, enum blu_json_val_types_e type)
{
    uint8_t* ctx_mark_p = state_p->ctx_p + state_p->cur_ctx_off;
    if ((ctx_mark_p + sizeof(obj_t)) >= state_p->ctx_end_p) {
        return BLU_JSON_NOT_ENOUGH_CTX_SPACE;
    }

    obj_t* p = (obj_t*)ctx_mark_p;
    bool is_parent_array = false;
    
    if (state_p->cur_parent_ctx_off != INVALID_OFFSET) {
        json_token_t* parent_p = (json_token_t*)&state_p->ctx_p[state_p->cur_parent_ctx_off];
        if (GET_TOKEN_TYPE(parent_p->type) == BLU_JSON_VAL_ARRAY) {
            p->t.key.len = 0;
            p->t.key.json_off = INVALID_OFFSET;
            is_parent_array = true;
        }
    }

    if (!is_parent_array) {
        p->t.key.len = state_p->cur_key_len;
        p->t.key.json_off = state_p->cur_key_json_off;
    }

    p->t.type = 0;
    p->t.type = SET_TOKEN_TYPE(p->t.type, type);
    p->t.type = SET_DATA_TYPE(p->t.type, BLU_JSON_TYPE_MIXED);
    p->t.parent_ctx_off = state_p->cur_parent_ctx_off;
    p->t.sibling_ctx_off = INVALID_OFFSET;
    p->json_start_off = state_p->cur_json_off;
    p->len = 0;

    if (state_p->cur_parent_ctx_off == INVALID_OFFSET) {
        state_p->cur_parent_ctx_off = 0;
    } else {
        state_p->cur_parent_ctx_off = state_p->cur_ctx_off;
    }

    state_p->cur_ctx_off += sizeof(obj_t);
    check_and_update_ctx_size(state_p);

    CALL_LOG_FUNC(_log_token(p, state_p->json_p, state_p->ctx_p));

    return 0;
}

static inline int push_obj_token(struct parser_state_t *state_p)
{
    return push_obj_arr_token(state_p, BLU_JSON_VAL_OBJ);
}

static inline int push_arr_token(struct parser_state_t *state_p)
{
    int rc = push_obj_arr_token(state_p, BLU_JSON_VAL_ARRAY);
    if (rc < 0)
        return rc;
    return move_json_offset(state_p, 1);
}

static int get_next_key(struct parser_state_t *state_p)
{
    char quote = JSON_STR_QUOTE;
    char colon = JSON_COLON;
    int rc = move_to_any_char(state_p, &quote, 1, true);
    if (rc < 0)
        return rc;
    
    if (state_p->json_p[state_p->cur_json_off] == JSON_STR_QUOTE)
        /* key cant be empty */
        return BLU_JSON_INVALID_JSON;

    state_p->cur_key_json_off = state_p->cur_json_off;
    state_p->cur_key_len = 0;
    
    rc = move_to_any_char(state_p, &quote, 1, true);

    if (rc < 0) {
        return rc;
    }
    
    state_p->cur_key_len += rc;

    /* move cur json offset passed colon */
    rc = move_to_any_char(state_p, &colon, 1, true);
    if (rc < 0) {
        return rc;
    }
    
    if (++state_p->cur_json_off + state_p->json_p >= state_p->json_end_p)
        return BLU_JSON_INVALID_JSON;

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

static bool cmp_str(const char* s1, const char* s2)
{
    uint16_t i = 0;
    char c, s;
    while ((c = s1[i]) != NULL && (s = s2[i]) != NULL) {
        if (c != s)
            return false;
        ++i;
    }
    return true;
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
        if (cmp_str(c, JSON_NULL_STR)) {
            *prim_type_p = BLU_JSON_TYPE_NULL;
            return 0;
        } else if (cmp_str(c, JSON_TRUE_STR) || cmp_str(c, JSON_FALSE_STR)) {
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
    char minus = JSON_NEG_NUM;
    *out_p = atoi(&state_p->json_p[state_p->cur_json_off]);
    
    int rc = move_while_char_eq(state_p, &minus, 1);
    if (rc < 0)
        return rc;
    
    int abs_val = *out_p < 0 ? -(*out_p) : *out_p;
    uint16_t steps = 1;
    while (abs_val >= 10) {
        ++steps;
        abs_val /= 10;
    }
    
    return move_json_offset(state_p, steps);    
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
    return move;
}

static int str2str_and_move(struct parser_state_t *state_p, uint8_t *len_out_p, uint16_t *json_off_out_p)
{
    char quote = JSON_STR_QUOTE;
    *json_off_out_p = state_p->cur_json_off;
    
    int rc = move_to_any_char(state_p, &quote, 1, false);
    if (rc < 0)
        return rc;
    if (rc > UINT8_MAX)
        return BLU_JSON_ERROR_PARSING;

    *len_out_p = (uint8_t)rc;

    /* move to char past closing '"' */
    return move_json_offset(state_p, 1);
}

static int str2bool_and_move(struct parser_state_t *state_p, uint8_t *out_p)
{
    uint16_t jump_len = 0;
    if (cmp_str(JSON_TRUE_STR, &state_p->json_p[state_p->cur_json_off])) {
        *out_p = true;
        jump_len = JSON_TRUE_STR_SIZE;
    }
    else if (cmp_str(JSON_FALSE_STR, &state_p->json_p[state_p->cur_json_off])) {
        *out_p = false;
        jump_len = JSON_FALSE_STR_SIZE;
    }
    else {
        return BLU_JSON_ERROR_PARSING;
    }

    return move_json_offset(state_p, jump_len);
}

static inline int str2null_and_move(struct parser_state_t *state_p)
{
    return move_json_offset(state_p, JSON_NULL_STR_SIZE); 
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
    token.key.json_off = state_p->cur_key_json_off;
    token.type = 0;
    token.type = SET_TOKEN_TYPE(token.type, BLU_JSON_VAL_PRIMITIVE);
    token.type = SET_DATA_TYPE(token.type, data_type);
    token.parent_ctx_off = state_p->cur_parent_ctx_off;

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
        rc = str2null_and_move(state_p);
        break;
    default:
        return BLU_JSON_ERROR_PARSING;
    }
    if (rc < 0)
        return rc;
    
    char comma_char = JSON_COMMA;
    rc = move_while_char_eq(state_p, &comma_char, 1);
    if (rc < 0)
        return rc;

    if (remaining_ctx_space < push_ctx_size)
        return BLU_JSON_NOT_ENOUGH_CTX_SPACE;

    if (state_p->last_sibling_ctx_off != INVALID_OFFSET) {
        /* set last siblings offset to point to me */
        json_token_t* older_sibling_p = (json_token_t*)&state_p->ctx_p[state_p->last_sibling_ctx_off];
        older_sibling_p->sibling_ctx_off = state_p->cur_ctx_off;
    }

    /* set last sibling offset for next youger sibling */
    state_p->last_sibling_ctx_off = state_p->cur_ctx_off;

    /* push prim token to ctx buf */
    memcpy(&state_p->ctx_p[state_p->cur_ctx_off], &type_token, push_ctx_size);
    CALL_LOG_FUNC(_log_token(&state_p->ctx_p[state_p->cur_ctx_off], state_p->json_p, state_p->ctx_p));
    state_p->cur_ctx_off += push_ctx_size;
    check_and_update_ctx_size(state_p);

    return 0;
}

/* 0 for string, 1 for close object, -err */
int move_to_string_or_obj_close(struct parser_state_t *state_p)
{
    char move_to_chars[] = {JSON_STR_QUOTE, JSON_OBJ_END};
    int rc = move_to_any_char(state_p, move_to_chars, sizeof(move_to_chars), false);
    if (rc < 0)
        return rc;

    switch (state_p->json_p[state_p->cur_json_off])
    {
    case JSON_STR_QUOTE:
        return 0;
    case JSON_OBJ_END:
        return 1;
    default:
        return BLU_JSON_ERROR_PARSING;
    }
}

int blu_json_parse(void* ctx, unsigned int max_ctx_len, const char* json, unsigned int json_len)
{
    int rc = 0;
    enum blu_json_val_types_e val_type = BLU_JSON_VAL_OBJ;
    enum blu_json_val_types_e cur_parent_type = BLU_JSON_VAL_OBJ;
    enum blu_json_datatypes_e data_type;

    struct parser_state_t state = {
        .ctx_p = ctx + SIZEOF_CTX_LEN,
        .ctx_end_p = ctx + max_ctx_len - SIZEOF_CTX_LEN,
        .cur_ctx_off = 0,
        .json_p = json,
        .json_end_p = json + json_len,
        .cur_json_off = 0,
        .cur_parent_ctx_off = INVALID_OFFSET,
        .cur_key_json_off = INVALID_OFFSET,
        .cur_key_len = 0,
        .last_sibling_ctx_off = INVALID_OFFSET,
        .ctx_len_p = ctx
    };
    char obj_start = JSON_OBJ_START;

    if (json == NULL)
        return BLU_JSON_INVALID_JSON;

    if (ctx == NULL || max_ctx_len < sizeof(obj_t) + SIZEOF_CTX_LEN)
        return BLU_JSON_NOT_ENOUGH_CTX_SPACE;
    
    memset(state.ctx_p, 0x00, max_ctx_len);

    /* Find start of JSON */
    rc = move_to_any_char(&state, &obj_start, 1, true);
    if (rc < 0)
        return rc;

    /* Push first object */
    rc = push_obj_token(&state);
    if (rc < 0) {
        return rc;
    }

    while (state.json_p + state.cur_json_off < state.ctx_end_p && state.json_p[state.cur_json_off]) {

        /* check if closing object */
        rc = check_and_close_object(&state, &cur_parent_type);
        if (rc < 0)
            return rc;
        else if (rc == 1)
            continue;
        else if (rc == 2)
            return (int)state.cur_ctx_off;

        if (cur_parent_type == BLU_JSON_VAL_OBJ) {
            /* find next key */
            rc = move_to_string_or_obj_close(&state);
            if (rc < 0)
                return rc;
            
            if (rc == 0) {
                rc = get_next_key(&state);
            }
            else {
                rc = check_and_close_object(&state, &cur_parent_type);
                if (rc < 0)
                    return rc;
                continue;
            }

            if (rc < 0)
                return rc;
        }

        rc = get_value_type(&state, &val_type);
        if (rc < 0)
            return rc;
        
        switch (val_type)
        {
        case BLU_JSON_VAL_OBJ:
            rc = push_obj_token(&state);
            cur_parent_type = BLU_JSON_VAL_OBJ;
            break;
        case BLU_JSON_VAL_ARRAY:
            rc = push_arr_token(&state);
            cur_parent_type = BLU_JSON_VAL_ARRAY;
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

    return BLU_JSON_ERROR_PARSING;
}

int blu_json_key_it_init(struct key_it_state_t *it, const void *ctx, const char* json, unsigned int json_len)
{
    if (it == NULL || ctx == NULL || json == NULL)
        return BLU_JSON_KEY_INVALID_PARAMS;
    
    it->ctx = ctx + SIZEOF_CTX_LEN;
    it->ctx_len = *((uint16_t*)ctx);
    it->json = json;
    it->json_len = json_len;
    it->is_init_magic = INIT_MAGIC;
    it->last_ctx_off = INVALID_OFFSET;
    return 0;
}

static int get_token_size(const json_token_t *cur_t_p)
{  
    switch (GET_TOKEN_TYPE(cur_t_p->type))
    {
    case BLU_JSON_VAL_OBJ:
        return sizeof(obj_t);
    case BLU_JSON_VAL_ARRAY:
        return sizeof(arr_t);
    case BLU_JSON_VAL_PRIMITIVE:
        break;
    default:
        return BLU_JSON_ERROR_PARSING;
    }

    switch (GET_DATA_TYPE(cur_t_p->type))
    {
    case BLU_JSON_TYPE_INT:
        return sizeof(prim_int_t);
    case BLU_JSON_TYPE_FLOAT:
        return sizeof(prim_float_t);
    case BLU_JSON_TYPE_STRING:
        return sizeof(prim_string_t);
    case BLU_JSON_TYPE_BOOL:
        return sizeof(prim_bool_t);
    case BLU_JSON_TYPE_NULL:
        return sizeof(prim_null_t);
    default:
        return BLU_JSON_ERROR_PARSING;
    }
}

bool blu_json_key_it(struct key_it_state_t *it, struct blu_json_key_t *key, struct blu_json_key_str_t *parents, uint8_t max_parents, uint8_t *num_parents_out)
{
    if (it == NULL || key == NULL)
        return false;
    
    if (it->ctx == NULL || it->json == NULL || it->is_init_magic != INIT_MAGIC)
        return false;

    json_token_t *token_p = NULL;
    int rc = 0;
    enum blu_json_val_types_e last_type;

    if (it->last_ctx_off == INVALID_OFFSET) {
        it->last_ctx_off = 0;
    }

    /* move off last token */
    token_p = (json_token_t*)(it->ctx + it->last_ctx_off);
    last_type = GET_TOKEN_TYPE(token_p->type);

    do {
        rc = get_token_size(token_p);
        if (rc < 0)
            return false;

        it->last_ctx_off += rc;

        if (it->last_ctx_off >= it->ctx_len)
            return false;

        token_p = (json_token_t*)(it->ctx + it->last_ctx_off);

        key->key.key = (const char*)(it->json + token_p->key.json_off);
        key->key.len = token_p->key.len;
        key->meta.obj_type = GET_TOKEN_TYPE(token_p->type);
        key->meta.data_type = GET_DATA_TYPE(token_p->type);
    } while (key->key.key == INVALID_OFFSET || key->key.len == 0);

    if (parents != NULL && num_parents_out != NULL) {
        *num_parents_out = 0;
        while (*num_parents_out < max_parents && token_p->parent_ctx_off != INVALID_OFFSET && token_p->parent_ctx_off != 0) {
            token_p = (json_token_t*)(it->ctx + token_p->parent_ctx_off);
            parents[*num_parents_out].key = (const char*)(it->json + token_p->key.json_off);
            parents[*num_parents_out++].len = token_p->key.len;
        }
    }

    return true;
}

