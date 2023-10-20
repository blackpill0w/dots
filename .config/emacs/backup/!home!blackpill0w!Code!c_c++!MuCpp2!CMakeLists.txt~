cmake_minimum_required(VERSION 3.14)
project(app CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
string(CONCAT CMAKE_CXX_FLAGS
             "${CMAKE_CXX_FLAGS}"
             " -Wall -Wextra"
             " -Wfloat-conversion -Wsign-conversion"
             " -Wshadow -Wduplicated-cond"
             " -Wduplicated-branches -Wlogical-op"
             " -Wnull-dereference -Wuseless-cast"
             " -Wformat=2")

option(BUILD_WITH_NIX "Whether to build with nix" ON)

#fmt
find_package(fmt)
if ((NOT fmt_FOUND) AND (NOT BUILD_WITH_NIX))
   include(FetchContent)
   FetchContent_Declare(
      fmt
      GIT_REPOSITORY https://github.com/fmtlib/fmt.git
      GIT_TAG master
   )
   FetchContent_MakeAvailable(fmt)
endif()

# raylib
find_package((raylib) AND (NOT BUILD_WITH_NIX))
if (NOT raylib_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        raylib
        GIT_REPOSITORY https://github.com/raysan5/raylib.git
        GIT_TAG 4.2.0
    )
    FetchContent_MakeAvailable(raylib)
endif()

# SFML
find_package(SFML COMPONENTS window graphics)
if ((NOT SFML_FOUND) AND (NOT BUILD_WITH_NIX))
   include(FetchContent)
   FetchContent_Declare(
      SFML
      GIT_REPOSITORY https://github.com/SFML/SFML.git
      GIT_TAG 2.5.1
   )
   FetchContent_MakeAvailable(SFML)
endif()

# JsonCpp
set(JSONCPP_WITH_POST_BUILD_UNITTEST OFF)
set(JSONCPP_WITH_POST_BUILD_UNITTEST OFF)
set(JSONCPP_WITH_TESTS OFF)
find_package(PkgConfig REQUIRED)
pkg_check_modules(JSONCPP jsoncpp)
if ((NOT JSONCPP_FOUND) AND (NOT BUILD_WITH_NIX))
   include(FetchContent)
   FetchContent_Declare(
      JSONCPP
      GIT_REPOSITORY https://github.com/open-source-parsers/jsoncpp.git
      GIT_TAG 1.9.5
   )
   FetchContent_MakeAvailable(JSONCPP)
endif()

# sdplog
find_package(spdlog)
if ((NOT spdlog_FOUND) AND (NOT BUILD_WITH_NIX))
   include(FetchContent)
   FetchContent_Declare(
      spdlog
      GIT_REPOSITORY https://github.com/gabime/spdlog.git
      GIT_TAG v1.11.0
   )
   FetchContent_MakeAvailable(spdlog)
endif()

add_executable(app
   src/main.cpp)
target_link_libraries(app
   sfml-window sfml-graphics OpenGL
   raylib
   fmt
   jsoncpp
)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# Copy root/build/compile_commands.json to the root directory of the project
if (EXISTS "${CMAKE_BINARY_DIR}/compile_commands.json")
   FILE(COPY ${CMAKE_BINARY_DIR}/compile_commands.json DESTINATION ${CMAKE_HOME_DIRECTORY})
endif()
