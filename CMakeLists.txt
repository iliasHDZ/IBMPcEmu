cmake_minimum_required(VERSION 3.22.1)

set(PROJECT_NAME IBMPcEmu)

project(${PROJECT_NAME} VERSION 1.0.0)

file(GLOB_RECURSE CXX_SRCS src/**.cpp)

set(CXX_FLAGS -g)

find_package(SDL2 REQUIRED)
include_directories(${SDL2_INCLUDE_DIRS})

add_executable(IBMPcEmu ${CXX_SRCS})
target_link_libraries(IBMPcEmu ${SDL2_LIBRARIES})