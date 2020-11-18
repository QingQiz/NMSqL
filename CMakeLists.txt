cmake_minimum_required(VERSION 3.10)

# set the project name
project(dbEngine)
set(CMAKE_CXX_STANDARD 14)

# add the executable

set(MAIN_FILE src/backend/DBEngine/main.cpp)
SET(LIB_DIRECTORY src/backend/include/)
SET(WORK_DIRECTORY src/backend/DBEngine/)
SET(HEAD_DIRECTORY ${WORK_DIRECTORY}include/)
SET(SRC_DIRECTORY ${WORK_DIRECTORY}src/)

add_executable(dbEngine ${MAIN_FILE}
		${LIB_DIRECTORY}BTreeInterface.h
		${LIB_DIRECTORY}PagerInterface.h
		${LIB_DIRECTORY}DbEngineInterface.h
		${HEAD_DIRECTORY}DBEngine.h
		${SRC_DIRECTORY}DBEngine.cpp
		)