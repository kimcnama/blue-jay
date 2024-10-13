#include <gtest/gtest.h>
#include "../blu-json/blu_json.h"

static const char simple_json[] = "{\"a\": 1, \"b\": 2.4, \"c\" : { \"d\": [[2, 4], [8, 9]] } , \"e\" : true}";

TEST(tsuite_simple, test_parse) {
  
  unsigned char ctx[1024]; 

  EXPECT_EQ(0, blu_json_parse(ctx, sizeof(ctx), simple_json, sizeof(simple_json)));
}