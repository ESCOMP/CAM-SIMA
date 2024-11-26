message(STATUS "Adding pfunit test target")
add_pfunit_ctest(core_utilis_tests
                 test_string_core_utils.pf
                 LINK_LIBRARIES core_utils)
