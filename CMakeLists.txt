cmake_minimum_required(VERSION 3.21)
project(variant)

set(CMAKE_CXX_STANDARD 20)

find_package(GTest REQUIRED)

file(GLOB TEST_SRC test/*.cpp)
file(GLOB SRC src/*.cpp)

add_executable(tests ${TEST_SRC} ${SRC})

target_include_directories(tests PRIVATE src test)

if (MSVC)
    target_compile_options(tests PRIVATE /W4 /permissive-)
    if (TREAT_WARNINGS_AS_ERRORS)
        target_compile_options(tests PRIVATE /WX)
    endif ()
else ()
    target_compile_options(tests PRIVATE -Wall -pedantic -Wextra -Wno-sign-compare)
    target_compile_options(tests PRIVATE -Wold-style-cast -Wextra-semi -Woverloaded-virtual -Wzero-as-null-pointer-constant)
    if (TREAT_WARNINGS_AS_ERRORS)
        target_compile_options(tests PRIVATE -Werror -pedantic-errors)
    endif ()
    target_compile_options(tests PRIVATE -Wno-self-move)
endif ()

# Compiler specific warnings
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(tests PRIVATE -Wshadow=compatible-local)
    target_compile_options(tests PRIVATE -Wduplicated-branches)
    target_compile_options(tests PRIVATE -Wduplicated-cond)
    # Disabled due to GCC bug
    # target_compile_options(tests PRIVATE -Wnull-dereference)
    # False positives
    target_compile_options(tests PRIVATE -Wno-maybe-uninitialized)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(tests PRIVATE -Wshadow-uncaptured-local)
    target_compile_options(tests PRIVATE -Wloop-analysis)
    target_compile_options(tests PRIVATE -Wno-self-assign-overloaded)
    target_compile_options(tests PRIVATE -Wno-missing-braces)
endif ()

option(USE_SANITIZERS "Enable to build with undefined,leak and address sanitizers" OFF)
if (USE_SANITIZERS)
    message(STATUS "Enabling sanitizers...")
    target_compile_options(tests PUBLIC -fsanitize=address,undefined,leak -fno-sanitize-recover=all)
    target_link_options(tests PUBLIC -fsanitize=address,undefined,leak)
endif ()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    message(STATUS "Enabling libc++...")
    target_compile_options(tests PUBLIC -stdlib=libc++)
    target_link_options(tests PUBLIC -stdlib=libc++)
endif ()

if (CMAKE_BUILD_TYPE MATCHES "Debug")
    message(STATUS "Enabling _GLIBCXX_DEBUG...")
    target_compile_options(tests PUBLIC -D_GLIBCXX_DEBUG)
endif ()

target_link_libraries(tests GTest::gtest GTest::gtest_main)
