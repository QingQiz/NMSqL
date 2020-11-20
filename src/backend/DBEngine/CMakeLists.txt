cmake_minimum_required(VERSION 3.10)

# set the project name
project(dbEngine)
set(CMAKE_CXX_STANDARD 14)

# add the executable

set(MAIN_FILE main.cpp)
SET(LIB_DIRECTORY ../include/)
SET(WORK_DIRECTORY ./)
SET(HEAD_DIRECTORY ${WORK_DIRECTORY}include/)
SET(SRC_DIRECTORY ${WORK_DIRECTORY}src/)

INCLUDE_DIRECTORIES(${LIB_DIRECTORY})

add_executable(dbEngine ${MAIN_FILE}
        ${LIB_DIRECTORY}BTreeInterface.h
        ${LIB_DIRECTORY}PagerInterface.h
        ${LIB_DIRECTORY}DbEngineInterface.h
        ${HEAD_DIRECTORY}DBEngine.h
        ${SRC_DIRECTORY}DBEngine.cpp
        )