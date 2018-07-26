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
SET(CTEST_CUSTOM_MAXIMUM_PASSED_TEST_OUTPUT_SIZE 50000)
SET(CTEST_CUSTOM_MAXIMUM_FAILED_TEST_OUTPUT_SIZE 50000)
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
    H5TEST-links_env        #uses runTest.cmake
    H5TEST-testlibinfo      #uses grepTest.cmake
    H5TEST-clear-testhdf5-objects
    H5TEST-clear-objects
    H5TEST-clear-cache-objects
    H5TEST-clear-cache_api-objects
    H5TEST-clear-cache_image-objects
    H5TEST-clear-cache_tagging-objects
    H5TEST-clear-err_compat-objects
    H5TEST-clear-error_test-objects
    H5TEST-clear-filenotclosed-objects
    H5TEST-clear-links_env-objects
    H5TEST-clear-ttsafe-objects
    PERFORM_h5perform-clear-objects
    HL_TOOLS-clear-objects
    HL_test-clear-objects
    HL_FORTRAN_test-clear-objects
    FORTRAN_testhdf5-clear-objects
    CPP_testhdf5-clear-objects
    ######### tools/h5clear #########
    H5CLEAR-clearall-objects
    H5CLEAR-copy_h5clear_log_v3.h5
    H5CLEAR-copy_h5clear_mdc_image.h5
    H5CLEAR-copy_h5clear_sec2_v0.h5
    H5CLEAR-copy_h5clear_sec2_v2.h5
    H5CLEAR-copy_h5clear_sec2_v3.h5
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
    H5DUMP-clearall-objects
    H5DUMP_PACKED_BITS-clearall-objects
    H5DUMP-XML-clearall-objects
    H5DUMP_VDS-clearall-objects
    ######### tools/h5format_convert #########
    H5FC-clearall-objects
    ######### tools/h5import #########
    H5IMPORT-h5importtest-clear-objects
    H5IMPORT-clear-objects
    ######### tools/h5jam #########
    H5JAM-SETUP-N_twithub_u10_c-clear-objects
    H5JAM-SETUP-N_twithub_u10_c
    H5JAM-N_twithub_u10_c-clear-objects
    H5JAM-NONE_COPY-N_twithub_u10_c
    H5JAM-CHECKFILE-N_twithub_u10_c-clear-objects
    H5JAM-SETUP-N_twithub_u511_c-clear-objects
    H5JAM-SETUP-N_twithub_u511_c
    H5JAM-N_twithub_u511_c-clear-objects
    H5JAM-NONE_COPY-N_twithub_u511_c
    H5JAM-CHECKFILE-N_twithub_u511_c-clear-objects
    H5JAM-SETUP-N_twithub_u512_c-clear-objects
    H5JAM-SETUP-N_twithub_u512_c
    H5JAM-N_twithub_u512_c-clear-objects
    H5JAM-NONE_COPY-N_twithub_u512_c
    H5JAM-CHECKFILE-N_twithub_u512_c-clear-objects
    H5JAM-SETUP-N_twithub_u513_c-clear-objects
    H5JAM-SETUP-N_twithub_u513_c
    H5JAM-N_twithub_u513_c-clear-objects
    H5JAM-NONE_COPY-N_twithub_u513_c
    H5JAM-CHECKFILE-N_twithub_u513_c-clear-objects
    H5JAM-SETUP-N_twithub513_u10_c-clear-objects
    H5JAM-SETUP-N_twithub513_u10_c
    H5JAM-N_twithub513_u10_c-clear-objects
    H5JAM-NONE_COPY-N_twithub513_u10_c
    H5JAM-CHECKFILE-N_twithub513_u10_c-clear-objects
    H5JAM-SETUP-N_twithub513_u511_c-clear-objects
    H5JAM-SETUP-N_twithub513_u511_c
    H5JAM-N_twithub513_u511_c-clear-objects
    H5JAM-NONE_COPY-N_twithub513_u511_c
    H5JAM-CHECKFILE-N_twithub513_u511_c-clear-objects
    H5JAM-SETUP-N_twithub513_u512_c-clear-objects
    H5JAM-SETUP-N_twithub513_u512_c
    H5JAM-N_twithub513_u512_c-clear-objects
    H5JAM-NONE_COPY-N_twithub513_u512_c
    H5JAM-CHECKFILE-N_twithub513_u512_c-clear-objects
    H5JAM-SETUP-N_twithub513_u513_c-clear-objects
    H5JAM-SETUP-N_twithub513_u513_c
    H5JAM-N_twithub513_u513_c-clear-objects
    H5JAM-NONE_COPY-N_twithub513_u513_c
    H5JAM-CHECKFILE-N_twithub513_u513_c-clear-objects
    H5JAM-CHECKFILE-twithub_u10_c-clear-objects
    H5JAM-twithub_u511_c-clear-objects
    H5JAM-CHECKFILE-twithub_u511_c-clear-objects
    H5JAM-twithub_u512_c-clear-objects
    H5JAM-CHECKFILE-twithub_u512_c-clear-objects
    H5JAM-twithub_u513_c-clear-objects
    H5JAM-CHECKFILE-twithub_u513_c-clear-objects
    H5JAM-twithub513_u10_c-clear-objects
    H5JAM-CHECKFILE-twithub513_u10_c-clear-objects
    H5JAM-twithub513_u511_c-clear-objects
    H5JAM-CHECKFILE-twithub513_u511_c-clear-objects
    H5JAM-twithub513_u512_c-clear-objects
    H5JAM-CHECKFILE-twithub513_u512_c-clear-objects
    H5JAM-twithub513_u513_c-clear-objects
    H5JAM-CHECKFILE-twithub513_u513_c-clear-objects
    H5JAM-SETUP-twithub_tall-clear-objects
    H5JAM-SETUP-twithub_tall
    H5JAM-UNJAM-twithub_tall-clear-objects
    H5JAM-UNJAM_D-twithub_tall-clear-objects
    H5JAM-CHECKFILE-twithub_tall-clear-objects
    H5JAM-SETUP-twithub513_tall-clear-objects
    H5JAM-SETUP-twithub513_tall
    H5JAM-UNJAM-twithub513_tall-clear-objects
    H5JAM-UNJAM_D-twithub513_tall-clear-objects
    H5JAM-CHECKFILE-twithub513_tall-clear-objects
    H5JAM-SETUP-N_twithub_tall-clear-objects
    H5JAM-SETUP-N_twithub_tall
    H5JAM-UNJAM-N_twithub_tall-clear-objects
    H5JAM-UNJAM_D-N_twithub_tall-clear-objects
    H5JAM-CHECKFILE-N_twithub_tall-clear-objects
    H5JAM-SETUP-N_twithub513_tall-clear-objects
    H5JAM-SETUP-N_twithub513_tall
    H5JAM-UNJAM-N_twithub513_tall-clear-objects
    H5JAM-UNJAM_D-N_twithub513_tall-clear-objects
    H5JAM-CHECKFILE-N_twithub513_tall-clear-objects
    H5JAM-SETUP-D_twithub_tall-clear-objects
    H5JAM-SETUP-D_twithub_tall
    H5JAM-UNJAM-D_twithub_tall-clear-objects
    H5JAM-UNJAM_D-D_twithub_tall-clear-objects
    H5JAM-CHECKFILE-D_twithub_tall-clear-objects
    H5JAM-SETUP-D_twithub513_tall-clear-objects
    H5JAM-SETUP-D_twithub513_tall
    H5JAM-UNJAM-D_twithub513_tall-clear-objects
    H5JAM-UNJAM_D-D_twithub513_tall-clear-objects
    H5JAM-CHECKFILE-D_twithub513_tall-clear-objects
    H5JAM-CHECKFILE-ta_u513-clear-objects
    H5JAM-twithub_u10-clear-objects
    H5JAM-CHECKFILE-twithub_u10-clear-objects
    H5JAM-twithub_u511-clear-objects
    H5JAM-CHECKFILE-twithub_u511-clear-objects
    H5JAM-twithub_u512-clear-objects
    H5JAM-CHECKFILE-twithub_u512-clear-objects
    H5JAM-twithub_u513-clear-objects
    H5JAM-CHECKFILE-twithub_u513-clear-objects
    H5JAM-twithub513_u10-clear-objects
    H5JAM-CHECKFILE-twithub513_u10-clear-objects
    H5JAM-twithub513_u511-clear-objects
    H5JAM-CHECKFILE-twithub513_u511-clear-objects
    H5JAM-twithub513_u512-clear-objects
    H5JAM-CHECKFILE-twithub513_u512-clear-objects
    H5JAM-twithub513_u513-clear-objects
    H5JAM-CHECKFILE-twithub513_u513-clear-objects
    H5JAM-twithub_u10_c-clear-objects
    H5JAM-tall_u10-clear-objects
    H5JAM-CHECKFILE-tall_u10-clear-objects
    H5JAM-tall_u511-clear-objects
    H5JAM-CHECKFILE-tall_u511-clear-objects
    H5JAM-tall_u512-clear-objects
    H5JAM-CHECKFILE-tall_u512-clear-objects
    H5JAM-tall_u513-clear-objects
    H5JAM-CHECKFILE-tall_u513-clear-objects
    H5JAM-SETUP-ta_u10-clear-objects
    H5JAM-SETUP-ta_u10
    H5JAM-ta_u10-clear-objects
    H5JAM-NONE_COPY-ta_u10
    H5JAM-CHECKFILE-ta_u10-clear-objects
    H5JAM-SETUP-ta_u511-clear-objects
    H5JAM-SETUP-ta_u511
    H5JAM-ta_u511-clear-objects
    H5JAM-NONE_COPY-ta_u511
    H5JAM-CHECKFILE-ta_u511-clear-objects
    H5JAM-SETUP-ta_u512-clear-objects
    H5JAM-SETUP-ta_u512
    H5JAM-ta_u512-clear-objects
    H5JAM-NONE_COPY-ta_u512
    H5JAM-CHECKFILE-ta_u512-clear-objects
    H5JAM-SETUP-ta_u513-clear-objects
    H5JAM-SETUP-ta_u513
    H5JAM-ta_u513-clear-objects
    H5JAM-NONE_COPY-ta_u513
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
    H5REPACK_OLD-old_style_layout_short_switches-clear-objects
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
    ######### tools/h5stat #########
    H5STAT-clearall-objects
    ######### tools/misc #########
    H5REPART-clearall-objects
    H5MKGRP-clearall-objects
    ######### tools/perform #########
    PERFORM_h5perform-clearall-objects
    ######### hl/tools/h5watch #########
    H5WATCH-clearall-objects
    ######### examples #########
    EXAMPLES-clear-objects
    CPP_ex-clear-objects
    CPP_ex_tutr-clear-objects
    HL_ex-clear-objects
    f90_ex-clear-objects
    HL_CPP_ex_ptExampleFL-clear-objects
    HL_FORTRAN_f90_ex-clear-objects
)
