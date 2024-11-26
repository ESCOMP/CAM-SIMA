set(CORE_UTILS_SRC string_core_utils.F90)

add_library(core_utils ${CORE_UTILS_SRC})
target_compile_options(core_utils PRIVATE -ffree-line-length-none)
target_include_directories(core_utils PUBLIC ${CMAKE_CURRENT_BINARY_DIR})