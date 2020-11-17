cmake_minimum_required(VERSION 3.10)

# set the project name
project(dbEngine)
set(CMAKE_CXX_STANDARD 14)

# add the executable

set(MAIN_FILE src/backend/DBEngine/main.cpp )
SET(WORK_DIRECTORY src/backend/DBEngine/)
SET(HEAD_DIRECTORY ${WORK_DIRECTORY}include/)
SET(SRC_DIRECTORY ${WORK_DIRECTORY}src/)

add_executable(dbEngine ${MAIN_FILE}
        ${HEAD_DIRECTORY}helloword.h
        ${SRC_DIRECTORY}helloworld.cpp)