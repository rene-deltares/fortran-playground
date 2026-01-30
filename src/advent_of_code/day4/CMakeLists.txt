add_library(scrolls STATIC scrolls.f90)
set_property(TARGET scrolls PROPERTY POSITION_INDEPENDENT_CODE ON)
target_link_libraries(scrolls PRIVATE util)

F90TWTEST(test_day4
    CFILES test_day4.cpp
    F90FILES test_day4.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_day4.f90
    LIBRARIES scrolls util
)

add_executable(day4 main.f90)
target_link_libraries(day4 PRIVATE scrolls util)
install(TARGETS day4 RUNTIME DESTINATION bin)
install(FILES input.txt DESTINATION share/day4)
