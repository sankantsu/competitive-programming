cmake_minimum_required(VERSION 3.20)

project(ahc030 LANGUAGES CXX)

SET(CMAKE_CXX_STANDARD 20)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

SET(CMAKE_CXX_FLAGS_RELEASE "-O2")

add_subdirectory(src)
add_subdirectory(after-contest)
