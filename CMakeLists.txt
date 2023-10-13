cmake_minimum_required(VERSION 3.27 FATAL_ERROR)

project(mixdyn
    VERSION 0.1
    DESCRIPTION "A LINEAR STATIC AND DYNAMIC FINITE ELEMENT ANALYSIS PROGRAM"
    HOMEPAGE_URL https://github.com/PaoloBertin/dlearn
    LANGUAGES Fortran
    )

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -std=f95 -fimplicit-none")
    set(bounds "-fbounds-check")
endif()

set(CMAKE_BUILD_TYPE Debug)

# if(CMAKE_BUILD_TYPE STREQUAL Debug)
#     set (CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/build-debug")
#     message(STATUS "CMAKE_RUNTIME_OUTPUT_DIRECTORY: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
# set(CMAKE_BUILD_TYPE Release)
# elseif(CMAKE_BUILD_TYPE STREQUAL Release)    
#     set (CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/build-release")
#     message(STATUS "CMAKE_RUNTIME_OUTPUT_DIRECTORY: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
# endif()

add_subdirectory(src)

               
