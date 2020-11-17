cmake_minimum_required(VERSION 3.10)

# set the project name
project(dbEngine)
set(CMAKE_CXX_STANDARD 14)

# add the executable

set(MAIN_FILE src/backend/DBEngine/main.cpp )
SET(WORK_DIRECTORY src/backend/DBEngine/)

add_executable(dbEngine ${MAIN_FILE}
        ${WORK_DIRECTORY}.include/helloword.h
        ${WORK_DIRECTORY}.src/helloworld.cpp)