cmake_minimum_required(VERSION 3.14)
project(my_project)

# GoogleTest requires at least C++14
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_subdirectory(blu-json)

add_executable(
  blu_json_tests
  test/test_simple.cc
)

target_link_libraries(blu_json_tests
  blu_json
  GTest::gtest_main
)

target_include_directories(blu_json_tests PUBLIC {CMAKE_CURRENT_LIST_DIR}/blu-json)

include(GoogleTest)
gtest_discover_tests(blu_json_tests)
