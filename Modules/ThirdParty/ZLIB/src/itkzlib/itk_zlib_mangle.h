#ifndef itk_zlib_mangle_h
#define itk_zlib_mangle_h

/*

This header file mangles all symbols exported from the zlib library.
It is included in all files while building the zlib library.  Due to
namespace pollution, no zlib headers should be included in .h files in
VTK.

The following command was used to obtain the symbol list:

nm libitkzlib.so |grep " [TRD] "

This is the way to recreate the whole list:

nm libitkzlib.so |grep " [TRD] " | awk '{ print "#define "$3" itk_zlib_"$3 }'

REMOVE the "_init" and "_fini" entries.

*/

#define adler32 itk_zlib_adler32
#define adler32_combine itk_zlib_adler32_combine
#define compress itk_zlib_compress
#define compress2 itk_zlib_compress2
#define compressBound itk_zlib_compressBound
#define crc32 itk_zlib_crc32
#define crc32_combine itk_zlib_crc32_combine
#define get_crc_table itk_zlib_get_crc_table
#define deflate itk_zlib_deflate
#define deflateBound itk_zlib_deflateBound
#define deflateCopy itk_zlib_deflateCopy
#define deflateEnd itk_zlib_deflateEnd
#define deflateInit2_ itk_zlib_deflateInit2_
#define deflateInit_ itk_zlib_deflateInit_
#define deflateParams itk_zlib_deflateParams
#define deflatePrime itk_zlib_deflatePrime
#define deflateReset itk_zlib_deflateReset
#define deflateSetDictionary itk_zlib_deflateSetDictionary
#define deflateSetHeader itk_zlib_deflateSetHeader
#define deflateTune itk_zlib_deflateTune
#define deflate_copyright itk_zlib_deflate_copyright
#define gzclearerr itk_zlib_gzclearerr
#define gzclose itk_zlib_gzclose
#define gzdirect itk_zlib_gzdirect
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
#define gzungetc itk_zlib_gzungetc
#define gzwrite itk_zlib_gzwrite
#define inflate_fast itk_zlib_inflate_fast
#define inflate itk_zlib_inflate
#define inflateCopy itk_zlib_inflateCopy
#define inflateEnd itk_zlib_inflateEnd
#define inflateGetHeader itk_zlib_inflateGetHeader
#define inflateInit2_ itk_zlib_inflateInit2_
#define inflateInit_ itk_zlib_inflateInit_
#define inflatePrime itk_zlib_inflatePrime
#define inflateReset itk_zlib_inflateReset
#define inflateSetDictionary itk_zlib_inflateSetDictionary
#define inflateSync itk_zlib_inflateSync
#define inflateSyncPoint itk_zlib_inflateSyncPoint
#define inflate_copyright itk_zlib_inflate_copyright
#define inflate_table itk_zlib_inflate_table
#define _dist_code itk_zlib__dist_code
#define _length_code itk_zlib__length_code
#define _tr_align itk_zlib__tr_align
#define _tr_flush_block itk_zlib__tr_flush_block
#define _tr_init itk_zlib__tr_init
#define _tr_stored_block itk_zlib__tr_stored_block
#define _tr_tally itk_zlib__tr_tally
#define uncompress itk_zlib_uncompress
#define zError itk_zlib_zError
#define z_errmsg itk_zlib_z_errmsg
#define zcalloc itk_zlib_zcalloc
#define zcfree itk_zlib_zcfree
#define zlibCompileFlags itk_zlib_zlibCompileFlags
#define zlibVersion itk_zlib_zlibVersion

#endif
