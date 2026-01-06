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

# Set install directory
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/install" CACHE PATH "Installation directory")


# Find googletest
find_package(GTest REQUIRED)

# Enable testing
enable_testing()

# Add test executable
find_package(GTest CONFIG REQUIRED)
add_subdirectory(f90tw)
add_subdirectory(src/util)
add_subdirectory(src/advent_of_code/day1)
add_subdirectory(src/advent_of_code/day2)