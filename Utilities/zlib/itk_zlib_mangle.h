#ifndef itk_zlib_mangle_h
#define itk_zlib_mangle_h

/*

This header file mangles all symbols exported from the zlib library.
It is included in all files while building the zlib library.  Due to
namespace pollution, no zlib headers should be included in .h files in
ITK.

The following command was used to obtain the symbol list:

nm libitkzlib.a |grep " T "

*/

#define deflate_copyright itk_deflate_copyright
#define _length_code itk__length_code
#define _dist_code itk__dist_code
#define _tr_align itk__tr_align
#define _tr_flush_block itk__tr_flush_block
#define _tr_init itk__tr_init
#define _tr_stored_block itk__tr_stored_block
#define _tr_tally itk__tr_tally
#define adler32 itk_adler32
#define compress itk_compress
#define compress2 itk_compress2
#define crc32 itk_crc32
#define deflate itk_deflate
#define deflateCopy itk_deflateCopy
#define deflateEnd itk_deflateEnd
#define deflateInit2_ itk_deflateInit2_
#define deflateInit_ itk_deflateInit_
#define deflateParams itk_deflateParams
#define deflateReset itk_deflateReset
#define deflateSetDictionary itk_deflateSetDictionary
#define get_crc_table itk_get_crc_table
#define gzclose itk_gzclose
#define gzdopen itk_gzdopen
#define gzeof itk_gzeof
#define gzerror itk_gzerror
#define gzflush itk_gzflush
#define gzgetc itk_gzgetc
#define gzgets itk_gzgets
#define gzopen itk_gzopen
#define gzprintf itk_gzprintf
#define gzputc itk_gzputc
#define gzputs itk_gzputs
#define gzread itk_gzread
#define gzrewind itk_gzrewind
#define gzseek itk_gzseek
#define gzsetparams itk_gzsetparams
#define gztell itk_gztell
#define gzwrite itk_gzwrite
#define inflate itk_inflate
#define inflateEnd itk_inflateEnd
#define inflateInit2_ itk_inflateInit2_
#define inflateInit_ itk_inflateInit_
#define inflateReset itk_inflateReset
#define inflateSetDictionary itk_inflateSetDictionary
#define inflateSync itk_inflateSync
#define inflateSyncPoint itk_inflateSyncPoint
#define inflate_blocks itk_inflate_blocks
#define inflate_blocks_free itk_inflate_blocks_free
#define inflate_blocks_new itk_inflate_blocks_new
#define inflate_blocks_reset itk_inflate_blocks_reset
#define inflate_blocks_sync_point itk_inflate_blocks_sync_point
#define inflate_codes itk_inflate_codes
#define inflate_codes_free itk_inflate_codes_free
#define inflate_codes_new itk_inflate_codes_new
#define inflate_copyright itk_inflate_copyright
#define inflate_fast itk_inflate_fast
#define inflate_flush itk_inflate_flush
#define inflate_mask itk_inflate_mask
#define inflate_set_dictionary itk_inflate_set_dictionary
#define inflate_trees_bits itk_inflate_trees_bits
#define inflate_trees_dynamic itk_inflate_trees_dynamic
#define inflate_trees_fixed itk_inflate_trees_fixed
#define uncompress itk_uncompress
#define zError itk_zError
#define zcalloc itk_zcalloc
#define zcfree itk_zcfree
#define zlibVersion itk_zlibVersion
#define z_errmsg itk_z_errmsg

#endif
