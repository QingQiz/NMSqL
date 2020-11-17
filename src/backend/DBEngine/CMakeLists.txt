cmake_minimum_required(VERSION 3.10)

# set the project name
project(dbEngine)
set(CMAKE_CXX_STANDARD 14)

# add the executable
add_executable(dbEngine main.cpp
		include/helloword.h
		src/helloworld.cpp include/DBEngine.h)