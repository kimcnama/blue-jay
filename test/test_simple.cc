#include <gtest/gtest.h>
#include "../blu-json/blu_json.h"

#define MIN(_a, _b) ((_a) < (_b) ? (_a) : (_b))

static const char simple_json[] = "{\"a\": 1, \"b\": 2.4, \"c\" : { \"d\": [[2, -4], [8, 9]] } , \"e\" : true}";

TEST(tsuite_simple, test_parse) {
  
  unsigned char ctx[1024]; 

  EXPECT_TRUE(0 < blu_json_parse(ctx, sizeof(ctx), simple_json, sizeof(simple_json)));
  blu_json_print_tokens(ctx, simple_json);
  
  struct key_it_state_t it;
  EXPECT_EQ(0, blu_json_key_it_init(&it, ctx, simple_json, sizeof(simple_json)));

  struct blu_json_key_t key_info;
  struct blu_json_key_str_t parents[3];
  uint8_t num_parents;

  char key[12];
  while (blu_json_key_it(&it, &key_info, parents, 3, &num_parents)) {
    memset(key, '\0', sizeof(key));
    memcpy(key, key_info.key.key, MIN(sizeof(key), key_info.key.len));
    printf("Key=%s \n", key);
  }

  float x;
  blu_json_get_primitive_value(ctx, simple_json, sizeof(simple_json), &x, sizeof(x), 1, "b");
  printf("Key=%.4f \n", x);
}