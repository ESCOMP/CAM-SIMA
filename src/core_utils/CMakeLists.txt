set(CORE_UTILS_SRC string_core_utils.F90)

# core_utils is not integrated into the CMake build of any top level
# project yet and this CMake is for testing purposes only.
# Making a change to this project's CMake will not impact the build of
# a parent project at this time.
add_library(core_utils ${CORE_UTILS_SRC})
target_include_directories(core_utils PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

