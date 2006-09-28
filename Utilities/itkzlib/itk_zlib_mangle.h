#ifndef itk_zlib_mangle_h
#define itk_zlib_mangle_h

/*

This header file mangles all symbols exported from the zlib library.
It is included in all files while building the zlib library.  Due to
namespace pollution, no zlib headers should be included in .h files in
ITK.

The following command was used to obtain the symbol list:

nm libitkzlib.a |grep " [TR] "

*/

#define deflate_copyright itk_zlib_deflate_copyright
#define _length_code itk_zlib__length_code
#define _dist_code itk_zlib__dist_code
#define _tr_align itk_zlib__tr_align
#define _tr_flush_block itk_zlib__tr_flush_block
#define _tr_init itk_zlib__tr_init
#define _tr_stored_block itk_zlib__tr_stored_block
#define _tr_tally itk_zlib__tr_tally
#define adler32 itk_zlib_adler32
#define compress itk_zlib_compress
#define compress2 itk_zlib_compress2
#define crc32 itk_zlib_crc32
#define deflate itk_zlib_deflate
#define deflateCopy itk_zlib_deflateCopy
#define deflateEnd itk_zlib_deflateEnd
#define deflateInit2_ itk_zlib_deflateInit2_
#define deflateInit_ itk_zlib_deflateInit_
#define deflateParams itk_zlib_deflateParams
#define deflateReset itk_zlib_deflateReset
#define deflateSetDictionary itk_zlib_deflateSetDictionary
#define get_crc_table itk_zlib_get_crc_table
#define gzclose itk_zlib_gzclose
#define gzdopen itk_zlib_gzdopen
#define gzeof itk_zlib_gzeof
#define gzerror itk_zlib_gzerror
#define gzflush itk_zlib_gzflush
#define gzgetc itk_zlib_gzgetc
#define gzgets itk_zlib_gzgets
#define gzopen itk_zlib_gzopen
#define gzprintf itk_zlib_gzprintf
#define gzputc itk_zlib_gzputc
#define gzputs itk_zlib_gzputs
#define gzread itk_zlib_gzread
#define gzrewind itk_zlib_gzrewind
#define gzseek itk_zlib_gzseek
#define gzsetparams itk_zlib_gzsetparams
#define gztell itk_zlib_gztell
#define gzwrite itk_zlib_gzwrite
#define inflate itk_zlib_inflate
#define inflateEnd itk_zlib_inflateEnd
#define inflateInit2_ itk_zlib_inflateInit2_
#define inflateInit_ itk_zlib_inflateInit_
#define inflateReset itk_zlib_inflateReset
#define inflateSetDictionary itk_zlib_inflateSetDictionary
#define inflateSync itk_zlib_inflateSync
#define inflateSyncPoint itk_zlib_inflateSyncPoint
#define inflate_blocks itk_zlib_inflate_blocks
#define inflate_blocks_free itk_zlib_inflate_blocks_free
#define inflate_blocks_new itk_zlib_inflate_blocks_new
#define inflate_blocks_reset itk_zlib_inflate_blocks_reset
#define inflate_blocks_sync_point itk_zlib_inflate_blocks_sync_point
#define inflate_codes itk_zlib_inflate_codes
#define inflate_codes_free itk_zlib_inflate_codes_free
#define inflate_codes_new itk_zlib_inflate_codes_new
#define inflate_copyright itk_zlib_inflate_copyright
#define inflate_fast itk_zlib_inflate_fast
#define inflate_flush itk_zlib_inflate_flush
#define inflate_mask itk_zlib_inflate_mask
#define inflate_set_dictionary itk_zlib_inflate_set_dictionary
#define inflate_trees_bits itk_zlib_inflate_trees_bits
#define inflate_trees_dynamic itk_zlib_inflate_trees_dynamic
#define inflate_trees_fixed itk_zlib_inflate_trees_fixed
#define uncompress itk_zlib_uncompress
#define zError itk_zlib_zError
#define zcalloc itk_zlib_zcalloc
#define zcfree itk_zlib_zcfree
#define zlibVersion itk_zlib_zlibVersion
#define z_errmsg itk_zlib_z_errmsg

#endif
