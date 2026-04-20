# Unit tests for aerosol modules
#
# Organized by dependency complexity:
#   Tier 1: Pure functions (table_interp): no mocking needed
#   Tier 2: Parsing/definitions (parse_rad_specifier, list_populate): minimal mocking
#   Tier 3: Concrete BAM implementations: mock at radiative_aerosol facade level
#   Tier 4: Concrete optics types: uses Tier 3 infrastructure end-to-end
#
# Libraries are structured to avoid duplicate compilation:
#   aerosol_test_mocks   - CAM infrastructure stubs (cam_abortutils, etc.)
#   table_interp_testlib - Tier 1 (pure functions, no mock deps)
#   rad_aer_defs_testlib - Tier 2 (radiative_aerosol_definitions parsing)
#   bulk_aero_testlib    - Tier 3 (BAM properties + state + all their deps)
#   insoluble_optics_testlib - Tier 4 (adds optics module on top of Tier 3)

set(AEROSOL_SRC ${CMAKE_SOURCE_DIR}/../../../src/aerosol)
set(UTILS_SRC   ${CMAKE_SOURCE_DIR}/../../../src/utils)
set(CORE_UTILS  ${CMAKE_SOURCE_DIR}/../../../src/core_utils)
# ccpp_kinds.F90 stub. When CAM_SIMA_ENABLE_IO_TESTS is also ON, the
# ccpp_framework target is built separately. We compile our own copy to
# avoid depending on that target being present.
set(CCPP_STUB   ${CMAKE_SOURCE_DIR}/ccpp_framework_stub/ccpp_kinds.F90)

# ========================================================================
# Mock library: lightweight stubs for CAM infrastructure modules
# ========================================================================
add_library(aerosol_test_mocks
    shr_kind_mod.F90
    mock_cam_abortutils.F90
    mock_cam_logfile.F90
    mock_spmd_utils.F90
    mock_physconst.F90
    mock_shr_infnan_mod.F90
    mock_shr_string_mod.F90
    mock_shr_spfn_mod.F90
    mock_cam_constituents.F90
    mock_vert_coord.F90
)
target_compile_options(aerosol_test_mocks PRIVATE -ffree-line-length-none)
target_include_directories(aerosol_test_mocks PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# ========================================================================
# Tier 1: table_interp: pure interpolation functions
# ========================================================================
add_library(table_interp_testlib
    ${UTILS_SRC}/table_interp_mod.F90
)
target_link_libraries(table_interp_testlib PUBLIC aerosol_test_mocks)
target_compile_options(table_interp_testlib PRIVATE -ffree-line-length-none)

add_pfunit_ctest(test_table_interp
    TEST_SOURCES test_table_interp.pf
    LINK_LIBRARIES table_interp_testlib)

# ========================================================================
# Tier 2: radiative_aerosol_definitions parsing and list population
# ========================================================================
add_library(rad_aer_defs_testlib
    mock_phys_prop.F90
    ${AEROSOL_SRC}/radiative_aerosol_definitions.F90
)
target_link_libraries(rad_aer_defs_testlib PUBLIC aerosol_test_mocks)
target_compile_options(rad_aer_defs_testlib PRIVATE -ffree-line-length-none)

add_pfunit_ctest(test_parse_rad_specifier
    TEST_SOURCES test_parse_rad_specifier.pf
    LINK_LIBRARIES rad_aer_defs_testlib)

add_pfunit_ctest(test_list_populate
    TEST_SOURCES test_list_populate.pf
    LINK_LIBRARIES rad_aer_defs_testlib)

# ========================================================================
# Tier 3: Shared library for BAM properties + state + mocks
# ========================================================================
add_library(bulk_aero_testlib
    ${AEROSOL_SRC}/aerosol_properties_mod.F90
    ${AEROSOL_SRC}/aerosol_state_mod.F90
    ${AEROSOL_SRC}/bulk_aerosol_properties_mod.F90
    ${AEROSOL_SRC}/bulk_aerosol_state_mod.F90
    ${UTILS_SRC}/string_utils.F90
    ${CORE_UTILS}/string_core_utils.F90
    mock_phys_prop.F90
    mock_radiative_aerosol.F90
    mock_aerosol_mmr_ccpp.F90
    ${CCPP_STUB}
)
target_link_libraries(bulk_aero_testlib PUBLIC aerosol_test_mocks)
target_compile_options(bulk_aero_testlib PRIVATE -ffree-line-length-none)

add_pfunit_ctest(test_bulk_aerosol_properties
    TEST_SOURCES test_bulk_aerosol_properties.pf
    LINK_LIBRARIES bulk_aero_testlib)

add_pfunit_ctest(test_bulk_aerosol_state
    TEST_SOURCES test_bulk_aerosol_state.pf
    LINK_LIBRARIES bulk_aero_testlib)

# ========================================================================
# Tier 4: insoluble_aerosol_optics end-to-end with real BAM objects
# ========================================================================
add_library(insoluble_optics_testlib
    ${AEROSOL_SRC}/aerosol_optics_mod.F90
    ${AEROSOL_SRC}/insoluble_aerosol_optics_mod.F90
)
target_link_libraries(insoluble_optics_testlib PUBLIC bulk_aero_testlib)
target_compile_options(insoluble_optics_testlib PRIVATE -ffree-line-length-none)

add_pfunit_ctest(test_insoluble_aerosol_optics
    TEST_SOURCES test_insoluble_aerosol_optics.pf
    LINK_LIBRARIES insoluble_optics_testlib)

# ========================================================================
# Tier 3b: Modal aerosol properties + state + mocks
# ========================================================================
add_library(modal_aero_testlib
    ${AEROSOL_SRC}/modal_aerosol_properties_mod.F90
    ${AEROSOL_SRC}/modal_aerosol_state_mod.F90
    modal_test_helpers.F90
)
target_link_libraries(modal_aero_testlib PUBLIC bulk_aero_testlib)
target_compile_options(modal_aero_testlib PRIVATE -ffree-line-length-none)

add_pfunit_ctest(test_modal_aerosol_properties
    TEST_SOURCES test_modal_aerosol_properties.pf
    LINK_LIBRARIES modal_aero_testlib)

add_pfunit_ctest(test_modal_aerosol_state
    TEST_SOURCES test_modal_aerosol_state.pf
    LINK_LIBRARIES modal_aero_testlib)
