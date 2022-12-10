cmake_minimum_required(VERSION 3.16)

project(lzhcc)

set(CMAKE_CXX_STANDARD 20)

include_directories(compiler)

file(GLOB_RECURSE LZHCC_SRC compiler/*.cc)

add_executable(lzhcc main.cc ${LZHCC_SRC})