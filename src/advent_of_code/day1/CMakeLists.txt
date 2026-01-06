add_library(rotations STATIC rotations.f90)
set_property(TARGET rotations PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(rotations PRIVATE util)

F90TWTEST(test_day1
    CFILES test_day1.cpp
    F90FILES test_day1.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_day1.f90
    LIBRARIES rotations util
)

add_executable(day1 main.f90)
target_link_libraries(day1 PRIVATE util rotations)
install(TARGETS day1 RUNTIME DESTINATION bin)
install(FILES input.txt DESTINATION share/day1)
