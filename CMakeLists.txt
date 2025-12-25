cmake_minimum_required(VERSION 3.31)

project(FortranPlayground LANGUAGES CXX Fortran)

set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Ensure proper logical representation when interfacing with C/C++
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpscomp logicals")
endif()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find googletest
find_package(GTest REQUIRED)

add_executable(hello_world src/main.f90)

# Enable testing
enable_testing()

# Add test executable
find_package(GTest CONFIG REQUIRED)
add_subdirectory(f90tw)
add_subdirectory(tests)
