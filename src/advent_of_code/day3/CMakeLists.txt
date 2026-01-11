add_library(joltage STATIC joltage.f90)
set_property(TARGET joltage PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(joltage PRIVATE util)

F90TWTEST(test_day3
    CFILES test_day3.cpp
    F90FILES test_day3.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_day3.f90
    LIBRARIES joltage util
)

add_executable(day3 main.f90)
target_link_libraries(day3 PRIVATE joltage util)
install(TARGETS day3 RUNTIME DESTINATION bin)
install(FILES input.txt DESTINATION share/day3)
