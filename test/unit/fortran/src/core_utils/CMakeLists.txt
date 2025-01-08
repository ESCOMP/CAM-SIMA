add_pfunit_ctest(core_utils_tests
                 TEST_SOURCES test_string_core_utils.pf
                 LINK_LIBRARIES core_utils)
target_compile_options(core_utils_tests PRIVATE -ffree-line-length-none)