
set(BLU_JSON_SRCS blu_json.c)

add_library(blu_json STATIC ${BLU_JSON_SRCS})

target_include_directories(blu_json PUBLIC {CMAKE_CURRENT_LIST_DIR})