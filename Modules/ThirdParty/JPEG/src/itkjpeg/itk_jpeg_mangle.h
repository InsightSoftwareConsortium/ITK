#ifndef itk_jpeg_mangle_h
#define itk_jpeg_mangle_h

/*

This header file mangles all symbols exported from the jpeg library.
It is included in all files while building the jpeg library.  Due to
namespace pollution, no jpeg headers should be included in .h files in
itk.

The following command was used to obtain the symbol list:

nm libitkjpeg.a |grep " [TRD] "

nm libitkjpeg.a |grep " [TRD] " | awk '{ print "#define "$3" itk_jpeg_"$3 }'

*/

#define jcopy_block_row              itk_jpeg_jcopy_block_row
#define jcopy_sample_rows            itk_jpeg_jcopy_sample_rows
#define jdiv_round_up                itk_jpeg_jdiv_round_up
#define jinit_1pass_quantizer        itk_jpeg_jinit_1pass_quantizer
#define jinit_2pass_quantizer        itk_jpeg_jinit_2pass_quantizer
#define jinit_c_coef_controller      itk_jpeg_jinit_c_coef_controller
#define jinit_c_main_controller      itk_jpeg_jinit_c_main_controller
#define jinit_c_master_control       itk_jpeg_jinit_c_master_control
#define jinit_c_prep_controller      itk_jpeg_jinit_c_prep_controller
#define jinit_color_converter        itk_jpeg_jinit_color_converter
#define jinit_color_deconverter      itk_jpeg_jinit_color_deconverter
#define jinit_compress_master        itk_jpeg_jinit_compress_master
#define jinit_d_coef_controller      itk_jpeg_jinit_d_coef_controller
#define jinit_d_main_controller      itk_jpeg_jinit_d_main_controller
#define jinit_d_post_controller      itk_jpeg_jinit_d_post_controller
#define jinit_downsampler            itk_jpeg_jinit_downsampler
#define jinit_forward_dct            itk_jpeg_jinit_forward_dct
#define jinit_huff_decoder           itk_jpeg_jinit_huff_decoder
#define jinit_huff_encoder           itk_jpeg_jinit_huff_encoder
#define jinit_input_controller       itk_jpeg_jinit_input_controller
#define jinit_inverse_dct            itk_jpeg_jinit_inverse_dct
#define jinit_marker_reader          itk_jpeg_jinit_marker_reader
#define jinit_marker_writer          itk_jpeg_jinit_marker_writer
#define jinit_master_decompress      itk_jpeg_jinit_master_decompress
#define jinit_memory_mgr             itk_jpeg_jinit_memory_mgr
#define jinit_merged_upsampler       itk_jpeg_jinit_merged_upsampler
#define jinit_upsampler              itk_jpeg_jinit_upsampler
#define jpeg_CreateCompress          itk_jpeg_jpeg_CreateCompress
#define jpeg_CreateDecompress        itk_jpeg_jpeg_CreateDecompress
#define jpeg_abort                   itk_jpeg_jpeg_abort
#define jpeg_abort_compress          itk_jpeg_jpeg_abort_compress
#define jpeg_abort_decompress        itk_jpeg_jpeg_abort_decompress
#define jpeg_add_quant_table         itk_jpeg_jpeg_add_quant_table
#define jpeg_alloc_huff_table        itk_jpeg_jpeg_alloc_huff_table
#define jpeg_alloc_quant_table       itk_jpeg_jpeg_alloc_quant_table
#define jpeg_calc_jpeg_dimensions    itk_jpeg_jpeg_calc_jpeg_dimensions
#define jpeg_calc_output_dimensions  itk_jpeg_jpeg_calc_output_dimensions
#define jpeg_consume_input           itk_jpeg_jpeg_consume_input
#define jpeg_copy_critical_parametersitk_jpeg_jpeg_copy_critical_parameters
#define jpeg_core_output_dimensions  itk_jpeg_jpeg_core_output_dimensions
#define jpeg_default_colorspace      itk_jpeg_jpeg_default_colorspace
#define jpeg_default_qtables         itk_jpeg_jpeg_default_qtables
#define jpeg_destroy                 itk_jpeg_jpeg_destroy
#define jpeg_destroy_compress        itk_jpeg_jpeg_destroy_compress
#define jpeg_destroy_decompress      itk_jpeg_jpeg_destroy_decompress
#define jpeg_fdct_10x10              itk_jpeg_jpeg_fdct_10x10
#define jpeg_fdct_10x5               itk_jpeg_jpeg_fdct_10x5
#define jpeg_fdct_11x11              itk_jpeg_jpeg_fdct_11x11
#define jpeg_fdct_12x12              itk_jpeg_jpeg_fdct_12x12
#define jpeg_fdct_12x6               itk_jpeg_jpeg_fdct_12x6
#define jpeg_fdct_13x13              itk_jpeg_jpeg_fdct_13x13
#define jpeg_fdct_14x14              itk_jpeg_jpeg_fdct_14x14
#define jpeg_fdct_14x7               itk_jpeg_jpeg_fdct_14x7
#define jpeg_fdct_15x15              itk_jpeg_jpeg_fdct_15x15
#define jpeg_fdct_16x16              itk_jpeg_jpeg_fdct_16x16
#define jpeg_fdct_16x8               itk_jpeg_jpeg_fdct_16x8
#define jpeg_fdct_1x1                itk_jpeg_jpeg_fdct_1x1
#define jpeg_fdct_1x2                itk_jpeg_jpeg_fdct_1x2
#define jpeg_fdct_2x1                itk_jpeg_jpeg_fdct_2x1
#define jpeg_fdct_2x2                itk_jpeg_jpeg_fdct_2x2
#define jpeg_fdct_2x4                itk_jpeg_jpeg_fdct_2x4
#define jpeg_fdct_3x3                itk_jpeg_jpeg_fdct_3x3
#define jpeg_fdct_3x6                itk_jpeg_jpeg_fdct_3x6
#define jpeg_fdct_4x2                itk_jpeg_jpeg_fdct_4x2
#define jpeg_fdct_4x4                itk_jpeg_jpeg_fdct_4x4
#define jpeg_fdct_4x8                itk_jpeg_jpeg_fdct_4x8
#define jpeg_fdct_5x10               itk_jpeg_jpeg_fdct_5x10
#define jpeg_fdct_5x5                itk_jpeg_jpeg_fdct_5x5
#define jpeg_fdct_6x12               itk_jpeg_jpeg_fdct_6x12
#define jpeg_fdct_6x3                itk_jpeg_jpeg_fdct_6x3
#define jpeg_fdct_6x6                itk_jpeg_jpeg_fdct_6x6
#define jpeg_fdct_7x14               itk_jpeg_jpeg_fdct_7x14
#define jpeg_fdct_7x7                itk_jpeg_jpeg_fdct_7x7
#define jpeg_fdct_8x16               itk_jpeg_jpeg_fdct_8x16
#define jpeg_fdct_8x4                itk_jpeg_jpeg_fdct_8x4
#define jpeg_fdct_9x9                itk_jpeg_jpeg_fdct_9x9
#define jpeg_fdct_float              itk_jpeg_jpeg_fdct_float
#define jpeg_fdct_ifast              itk_jpeg_jpeg_fdct_ifast
#define jpeg_fdct_islow              itk_jpeg_jpeg_fdct_islow
#define jpeg_finish_compress         itk_jpeg_jpeg_finish_compress
#define jpeg_finish_decompress       itk_jpeg_jpeg_finish_decompress
#define jpeg_finish_output           itk_jpeg_jpeg_finish_output
#define jpeg_free_large              itk_jpeg_jpeg_free_large
#define jpeg_free_small              itk_jpeg_jpeg_free_small
#define jpeg_get_large               itk_jpeg_jpeg_get_large
#define jpeg_get_small               itk_jpeg_jpeg_get_small
#define jpeg_has_multiple_scans      itk_jpeg_jpeg_has_multiple_scans
#define jpeg_idct_10x10              itk_jpeg_jpeg_idct_10x10
#define jpeg_idct_10x5               itk_jpeg_jpeg_idct_10x5
#define jpeg_idct_11x11              itk_jpeg_jpeg_idct_11x11
#define jpeg_idct_12x12              itk_jpeg_jpeg_idct_12x12
#define jpeg_idct_12x6               itk_jpeg_jpeg_idct_12x6
#define jpeg_idct_13x13              itk_jpeg_jpeg_idct_13x13
#define jpeg_idct_14x14              itk_jpeg_jpeg_idct_14x14
#define jpeg_idct_14x7               itk_jpeg_jpeg_idct_14x7
#define jpeg_idct_15x15              itk_jpeg_jpeg_idct_15x15
#define jpeg_idct_16x16              itk_jpeg_jpeg_idct_16x16
#define jpeg_idct_16x8               itk_jpeg_jpeg_idct_16x8
#define jpeg_idct_1x1                itk_jpeg_jpeg_idct_1x1
#define jpeg_idct_1x2                itk_jpeg_jpeg_idct_1x2
#define jpeg_idct_2x1                itk_jpeg_jpeg_idct_2x1
#define jpeg_idct_2x2                itk_jpeg_jpeg_idct_2x2
#define jpeg_idct_2x4                itk_jpeg_jpeg_idct_2x4
#define jpeg_idct_3x3                itk_jpeg_jpeg_idct_3x3
#define jpeg_idct_3x6                itk_jpeg_jpeg_idct_3x6
#define jpeg_idct_4x2                itk_jpeg_jpeg_idct_4x2
#define jpeg_idct_4x4                itk_jpeg_jpeg_idct_4x4
#define jpeg_idct_4x8                itk_jpeg_jpeg_idct_4x8
#define jpeg_idct_5x10               itk_jpeg_jpeg_idct_5x10
#define jpeg_idct_5x5                itk_jpeg_jpeg_idct_5x5
#define jpeg_idct_6x12               itk_jpeg_jpeg_idct_6x12
#define jpeg_idct_6x3                itk_jpeg_jpeg_idct_6x3
#define jpeg_idct_6x6                itk_jpeg_jpeg_idct_6x6
#define jpeg_idct_7x14               itk_jpeg_jpeg_idct_7x14
#define jpeg_idct_7x7                itk_jpeg_jpeg_idct_7x7
#define jpeg_idct_8x16               itk_jpeg_jpeg_idct_8x16
#define jpeg_idct_8x4                itk_jpeg_jpeg_idct_8x4
#define jpeg_idct_9x9                itk_jpeg_jpeg_idct_9x9
#define jpeg_idct_float              itk_jpeg_jpeg_idct_float
#define jpeg_idct_ifast              itk_jpeg_jpeg_idct_ifast
#define jpeg_idct_islow              itk_jpeg_jpeg_idct_islow
#define jpeg_input_complete          itk_jpeg_jpeg_input_complete
#define jpeg_mem_available           itk_jpeg_jpeg_mem_available
#define jpeg_mem_dest                itk_jpeg_jpeg_mem_dest
#define jpeg_mem_init                itk_jpeg_jpeg_mem_init
#define jpeg_mem_src                 itk_jpeg_jpeg_mem_src
#define jpeg_mem_term                itk_jpeg_jpeg_mem_term
#define jpeg_natural_order           itk_jpeg_jpeg_natural_order
#define jpeg_natural_order2          itk_jpeg_jpeg_natural_order2
#define jpeg_natural_order3          itk_jpeg_jpeg_natural_order3
#define jpeg_natural_order4          itk_jpeg_jpeg_natural_order4
#define jpeg_natural_order5          itk_jpeg_jpeg_natural_order5
#define jpeg_natural_order6          itk_jpeg_jpeg_natural_order6
#define jpeg_natural_order7          itk_jpeg_jpeg_natural_order7
#define jpeg_new_colormap            itk_jpeg_jpeg_new_colormap
#define jpeg_open_backing_store      itk_jpeg_jpeg_open_backing_store
#define jpeg_quality_scaling         itk_jpeg_jpeg_quality_scaling
#define jpeg_read_coefficients       itk_jpeg_jpeg_read_coefficients
#define jpeg_read_header             itk_jpeg_jpeg_read_header
#define jpeg_read_raw_data           itk_jpeg_jpeg_read_raw_data
#define jpeg_read_scanlines          itk_jpeg_jpeg_read_scanlines
#define jpeg_resync_to_restart       itk_jpeg_jpeg_resync_to_restart
#define jpeg_save_markers            itk_jpeg_jpeg_save_markers
#define jpeg_set_colorspace          itk_jpeg_jpeg_set_colorspace
#define jpeg_set_defaults            itk_jpeg_jpeg_set_defaults
#define jpeg_set_linear_quality      itk_jpeg_jpeg_set_linear_quality
#define jpeg_set_marker_processor    itk_jpeg_jpeg_set_marker_processor
#define jpeg_set_quality             itk_jpeg_jpeg_set_quality
#define jpeg_simple_progression      itk_jpeg_jpeg_simple_progression
#define jpeg_start_compress          itk_jpeg_jpeg_start_compress
#define jpeg_start_decompress        itk_jpeg_jpeg_start_decompress
#define jpeg_start_output            itk_jpeg_jpeg_start_output
#define jpeg_std_error               itk_jpeg_jpeg_std_error
#define jpeg_std_message_table       itk_jpeg_jpeg_std_message_table
#define jpeg_stdio_dest              itk_jpeg_jpeg_stdio_dest
#define jpeg_stdio_src               itk_jpeg_jpeg_stdio_src
#define jpeg_suppress_tables         itk_jpeg_jpeg_suppress_tables
#define jpeg_write_coefficients      itk_jpeg_jpeg_write_coefficients
#define jpeg_write_m_byte            itk_jpeg_jpeg_write_m_byte
#define jpeg_write_m_header          itk_jpeg_jpeg_write_m_header
#define jpeg_write_marker            itk_jpeg_jpeg_write_marker
#define jpeg_write_raw_data          itk_jpeg_jpeg_write_raw_data
#define jpeg_write_scanlines         itk_jpeg_jpeg_write_scanlines
#define jpeg_write_tables            itk_jpeg_jpeg_write_tables
#define jround_up                    itk_jpeg_jround_up
#define jzero_far                    itk_jpeg_jzero_far

#endif
