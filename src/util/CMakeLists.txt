add_library(util STATIC error.f90 comparison_sort.f90)
set_property(TARGET util PROPERTY POSITION_INDEPENDENT_CODE ON)
target_include_directories(util PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

F90TWTEST(test_comparison_sort
    CFILES test_comparison_sort.cpp
    F90FILES test_comparison_sort.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_comparison_sort.f90
    LIBRARIES util
)