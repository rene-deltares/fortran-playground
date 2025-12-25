add_library(example STATIC example.f90)
set_property(TARGET example PROPERTY POSITION_INDEPENDENT_CODE ON)

F90TWTEST(test_example_gtest 
    CFILES test_example_gtest.cpp
    F90FILES test_example_gtest.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_example_gtest.f90
    LIBRARIES example
)