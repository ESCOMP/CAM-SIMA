# Test orbital_data module
add_executable(test_physics_utils_orbital_data test_orbital_data.F90 test_orbital_data_stubs.F90)

target_sources(test_physics_utils_orbital_data
  PUBLIC
    ${CAM_SIMA_SRC_PATH}/physics/utils/orbital_data.F90
)

target_compile_options(test_physics_utils_orbital_data PRIVATE -ffree-line-length-none)

add_test(
  NAME test_physics_utils_orbital_data
  COMMAND $<TARGET_FILE:test_physics_utils_orbital_data>
  WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
)

add_memory_check_test(test_physics_utils_orbital_data $<TARGET_FILE:test_physics_utils_orbital_data> "" ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})