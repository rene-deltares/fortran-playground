add_library(util STATIC error.f90)
target_include_directories(util PUBLIC ${CMAKE_CURRENT_BINARY_DIR})