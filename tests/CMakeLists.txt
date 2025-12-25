add_library(example STATIC example.f90)
set_property(TARGET example PROPERTY POSITION_INDEPENDENT_CODE ON)

F90TWTEST(test_example_gtest 
    CFILES test_example_gtest.cpp
    F90FILES test_example_gtest.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_example_gtest.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    INCLUDES ${f90tw_INCLUDE_DIR}
    MODULES ${f90tw_MODULES_DIR}
    LIBRARIES example f90tw_gtest gtest
)