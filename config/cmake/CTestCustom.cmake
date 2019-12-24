#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
set (CTEST_CUSTOM_MAXIMUM_NUMBER_OF_WARNINGS 3000)
# Allow full output to go to CDash set to 0
set (CTEST_CUSTOM_MAXIMUM_PASSED_TEST_OUTPUT_SIZE 50000)
set (CTEST_CUSTOM_MAXIMUM_FAILED_TEST_OUTPUT_SIZE 50000)
# WARNING!  This could be a lot of output and could overwhelm CDash and the
# MySQL DB so this might not be a good idea!

set (CTEST_CUSTOM_WARNING_EXCEPTION
    ${CTEST_CUSTOM_WARNING_EXCEPTION}
#    "H5detect.c.[0-9]+.[ \t]*:[ \t]*warning C4090:"
#    "H5detect.c.[0-9]+.[ \t]*:[ \t]*warning:[ \t]*passing argument"
#    "H5detect.c[0-9 \t:]*warning:[ \t]*passing argument"
#    "note.*expected.*void.*but argument is of type.*volatile"
#    "H5Tconv.c[0-9 \t:]*warning:[ \t]*comparison is always false due to limited range of data type"
#    "H5Ztrans.c.[0-9]+.[ \t]*:[ \t]*warning C4244"
#    "SZIP.src.*:[ \t]*warning"
#    "POSIX name for this item is deprecated"
    "disabling jobserver mode"
    "config.cmake.xlatefile.c"
#    "warning.*implicit declaration of function"
#    "note: expanded from macro"
#    "fpp:[ \t]*warning:[ \t]*cannot remove H5_DEBUG_API - not a predefined macro"
)

set (CTEST_CUSTOM_MEMCHECK_IGNORE
    ${CTEST_CUSTOM_MEMCHECK_IGNORE}
    H5TEST-flush1           #designed to fail
    H5TEST-flush2           #designed to need flush1
    H5TEST-error_test       #uses runTest.cmake
    H5TEST-err_compat       #uses runTest.cmake
    H5TEST-testlibinfo      #uses grepTest.cmake
    #########
    H5TEST-clear-objects
    H5TEST-cache-clear-objects
    H5TEST-cache_image-clear-objects
    H5TEST-del_many_dense_attrs-clear-objects
    H5TEST-external_env-clear-objects
    H5TEST-filenotclosed-clear-objects
    H5TEST-flush-clear-objects
    H5TEST-links_env-clear-objects
    H5TEST-testflushrefresh-clear-objects
    H5TEST-testhdf5-clear-objects
    H5TEST-vds_env-clear-objects
    PERFORM_h5perform-clear-objects
    HL_test-clear-objects
    HL_FORTRAN_test-clear-objects
    FORTRAN_testhdf5-clear-objects
    FORTRAN_flush1-clear-objects
    CPP_testhdf5-clear-objects
    ######### examples #########
    EXAMPLES-clear-objects
    CPP_ex-clear-objects
    CPP_ex_tutr-clear-objects
    HL_ex-clear-objects
    f90_ex-clear-objects
    HL_CPP_ptableTest-clear-objects
    HL_CPP_ex_ptExampleFL-clear-objects
    HL_FORTRAN_f90_ex-clear-objects
    ######### tools/h5clear #########
    H5CLEAR-clearall-objects
    H5CLEAR-copy_h5clear_fsm_persist_equal.h5
    H5CLEAR-copy_h5clear_fsm_persist_greater.h5
    H5CLEAR-copy_h5clear_fsm_persist_less.h5
    H5CLEAR-copy_h5clear_fsm_persist_noclose.h5
    H5CLEAR-copy_h5clear_fsm_persist_user_equal.h5
    H5CLEAR-copy_h5clear_fsm_persist_user_greater.h5
    H5CLEAR-copy_h5clear_fsm_persist_user_less.h5
    H5CLEAR-copy_h5clear_log_v3.h5
    H5CLEAR-copy_h5clear_mdc_image.h5
    H5CLEAR-copy_h5clear_sec2_v0.h5
    H5CLEAR-copy_h5clear_sec2_v2.h5
    H5CLEAR-copy_h5clear_sec2_v3.h5
    H5CLEAR-copy_h5clear_status_noclear.h5
    H5CLEAR-copy_latest_h5clear_log_v3.h5
    H5CLEAR-copy_latest_h5clear_sec2_v3.h5
    H5CLEAR-copy_mod_h5clear_mdc_image.h5
    H5CLEAR-copy_mod_h5clr_mdc_image2.h5
    H5CLEAR-copy_orig_h5clear_sec2_v0.h5
    H5CLEAR-copy_orig_h5clear_sec2_v2.h5
    H5CLEAR-copy_orig_h5clear_sec2_v3.h5
    H5CLEAR-h5clear_gentest      # does not close ids by design
    ######### tools/h5copy #########
    H5COPY-clearall-objects
    ######### tools/h5diff #########
    H5DIFF-clearall-objects
    ######### tools/h5dump #########
    H5DUMP-t128bit_float      #uses grepTest.cmake
    ######### tools/h5format_convert #########
    H5FC-clearall-objects
    ######### tools/h5import #########
    H5IMPORT-h5importtest-clear-objects
    H5IMPORT-clear-objects
    ######### tools/h5jam #########
    ######### tools/h5ls #########
    H5LS-clearall-objects
    H5LS_VDS-clearall-objects
    ######### tools/h5repack #########
    H5REPACK-clearall-objects
    H5REPACK-add_alignment-clear-objects
    H5REPACK-add_userblock-clear-objects
    H5REPACK-all_filters-clear-objects
    H5REPACK-attr-clear-objects
    H5REPACK-committed_dt-clear-objects
    H5REPACK-deflate_convert-clear-objects
    H5REPACK-deflate_copy-clear-objects
    H5REPACK-deflate_file-clear-objects
    H5REPACK-deflate_remove-clear-objects
    H5REPACK-early-clear-objects
    H5REPACK-error4-clear-objects
    H5REPACK-family-clear-objects
    H5REPACK-fill-clear-objects
    H5REPACK-fletcher_all-clear-objects
    H5REPACK-fletcher_copy-clear-objects
    H5REPACK-fletcher_individual-clear-objects
    H5REPACK-fletcher_remove-clear-objects
    H5REPACK-global_filters-clear-objects
    H5REPACK-gt_mallocsize-clear-objects
    H5REPACK-gzip_all-clear-objects
    H5REPACK-gzip_individual-clear-objects
    H5REPACK-hlink-clear-objects
    H5REPACK-layout-clear-objects
    H5REPACK-native_attr-clear-objects
    H5REPACK-native_fill-clear-objects
    H5REPACK-nbit_add-clear-objects
    H5REPACK-nbit_copy-clear-objects
    H5REPACK-nbit_remove-clear-objects
    H5REPACK-nested_8bit_enum-clear-objects
    H5REPACK-objs-clear-objects
    H5REPACK-remove_all-clear-objects
    H5REPACK-scale_add-clear-objects
    H5REPACK-scale_copy-clear-objects
    H5REPACK-scale_remove-clear-objects
    H5REPACK-shuffle_all-clear-objects
    H5REPACK-shuffle_copy-clear-objects
    H5REPACK-shuffle_individual-clear-objects
    H5REPACK-shuffle_remove-clear-objects
    H5REPACK-szip_all-clear-objects
    H5REPACK-szip_convert-clear-objects
    H5REPACK-szip_copy-clear-objects
    H5REPACK-szip_individual-clear-objects
    H5REPACK-szip_remove-clear-objects
    H5REPACK-upgrade_layout-clear-objects
    H5REPACK_DMP-crtorder-clear-objects
    H5REPACK_DMP-deflate_limit-clear-objects
    H5REPACK-bug1814-clear-objects
    H5REPACK-HDFFV-5932-clear-objects
    H5REPACK-HDFFV-7840-clear-objects
    H5REPACK_META-meta_long_N-clear-objects
    H5REPACK_META-meta_short_N-clear-objects
    H5REPACK_STAT-GS_AGGR-clear-objects
    H5REPACK_STAT-S_AGGR-clear-objects
    H5REPACK_STAT-SP_NONE-clear-objects
    H5REPACK_STAT-SP_PAGE-clear-objects
    H5REPACK_STAT-SPT_FSM_AGGR-clear-objects
    H5REPACK_STAT-STG_PAGE-clear-objects
    #########
    H5REPACK_META-meta_long
    H5REPACK_META-meta_short
    #########
    H5REPACK-gzip_verbose_filters                       #uses runTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_chunk_20x10            #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-chunk_20x10              #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_conti                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-conti                    #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_compa                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-compa                    #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_conti             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_chunk             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_compa             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_compa             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_chunk             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_conti             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_compa                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_conti                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_18x13                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-contig_small_compa           #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-contig_small_fixed_compa     #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-layout_long_switches     #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-layout_short_switches    #uses grepTest.cmake
    H5REPACK-plugin
    H5REPACK_CMP-plugin_zero
    #########
    ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-latest_latest_invalid-clear-objects
    H5REPACK_VERIFY_SUPERBLOCK-SB_IS_0-clear-objects
    H5REPACK_VERIFY_SUPERBLOCK-SB_IS_2-clear-objects
    H5REPACK_VERIFY_SUPERBLOCK-SB_IS_3-clear-objects
    ######### tools/h5stat #########
    H5STAT-clearall-objects
    ######### tools/misc #########
    H5REPART-clearall-objects
    H5MKGRP-clearall-objects
    ######### tools/perform #########
    PERFORM_h5perform-clearall-objects
    ######### hl/tools #########
    HL_TOOLS-clear-objects
    H5WATCH-clearall-objects
)
