add_library(inventory STATIC inventory.f90)
set_property(TARGET inventory PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(inventory PRIVATE util)

F90TWTEST(test_day5
    CFILES test_day5.cpp
    F90FILES test_day5.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_day5.f90
    LIBRARIES inventory util
)

add_executable(day5 main.f90)
target_link_libraries(day5 PRIVATE inventory util)
install(TARGETS day5 RUNTIME DESTINATION bin)
install(FILES input.txt DESTINATION share/day5)
