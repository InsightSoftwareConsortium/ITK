#ifndef vcl_generic_cstdio_h_
#define vcl_generic_cstdio_h_

// THIS IS A GENERATED FILE. DO NOT EDIT! -- Instead, edit vcl_cstdio.hhh and run make

// [27.8.2.1]
//
// macros:
//   BUFSIZ        FOPEN_MAX SEEK_CUR TMP_MAX _IONBF stdout
//   EOF           L_tmpnam  SEEK_END _IOFBF  stderr
//   FILENAME_MAX  NULL      SEEK_SET _IOLBF  stdin

// FILE
#ifndef vcl_FILE
#define vcl_FILE vcl_generic_cstdio_STD :: FILE
#endif
// fpos_t
#ifndef vcl_fpos_t
#define vcl_fpos_t vcl_generic_cstdio_STD :: fpos_t
#endif
// NB: size_t is declared in <cstddef>, not <cstdio>
// fopen
#ifndef vcl_fopen
#define vcl_fopen vcl_generic_cstdio_STD :: fopen
#endif
// fclose
#ifndef vcl_fclose
#define vcl_fclose vcl_generic_cstdio_STD :: fclose
#endif
// feof
#ifndef vcl_feof
#define vcl_feof vcl_generic_cstdio_STD :: feof
#endif
// ferror
#ifndef vcl_ferror
#define vcl_ferror vcl_generic_cstdio_STD :: ferror
#endif
// fflush
#ifndef vcl_fflush
#define vcl_fflush vcl_generic_cstdio_STD :: fflush
#endif
// fgetc
#ifndef vcl_fgetc
#define vcl_fgetc vcl_generic_cstdio_STD :: fgetc
#endif
// fgetpos
#ifndef vcl_fgetpos
#define vcl_fgetpos vcl_generic_cstdio_STD :: fgetpos
#endif
// fgets
#ifndef vcl_fgets
#define vcl_fgets vcl_generic_cstdio_STD :: fgets
#endif
// fwrite
#ifndef vcl_fwrite
#define vcl_fwrite vcl_generic_cstdio_STD :: fwrite
#endif
// fread
#ifndef vcl_fread
#define vcl_fread vcl_generic_cstdio_STD :: fread
#endif
// fseek
#ifndef vcl_fseek
#define vcl_fseek vcl_generic_cstdio_STD :: fseek
#endif
// ftell
#ifndef vcl_ftell
#define vcl_ftell vcl_generic_cstdio_STD :: ftell
#endif
// perror
#ifndef vcl_perror
#define vcl_perror vcl_generic_cstdio_STD :: perror
#endif
// clearerr
#ifndef vcl_clearerr
#define vcl_clearerr vcl_generic_cstdio_STD :: clearerr
#endif
// rename
#ifndef vcl_rename
#define vcl_rename vcl_generic_cstdio_STD :: rename
#endif
// fputc
#ifndef vcl_fputc
#define vcl_fputc vcl_generic_cstdio_STD :: fputc
#endif
// fputs
#ifndef vcl_fputs
#define vcl_fputs vcl_generic_cstdio_STD :: fputs
#endif
// freopen
#ifndef vcl_freopen
#define vcl_freopen vcl_generic_cstdio_STD :: freopen
#endif
// fsetpos
#ifndef vcl_fsetpos
#define vcl_fsetpos vcl_generic_cstdio_STD :: fsetpos
#endif
// getc
#ifndef vcl_getc
#define vcl_getc vcl_generic_cstdio_STD :: getc
#endif
// getchar
#ifndef vcl_getchar
#define vcl_getchar vcl_generic_cstdio_STD :: getchar
#endif
// gets
#ifndef vcl_gets
#define vcl_gets vcl_generic_cstdio_STD :: gets
#endif
// putc
#ifndef vcl_putc
#define vcl_putc vcl_generic_cstdio_STD :: putc
#endif
// putchar
#ifndef vcl_putchar
#define vcl_putchar vcl_generic_cstdio_STD :: putchar
#endif
// puts
#ifndef vcl_puts
#define vcl_puts vcl_generic_cstdio_STD :: puts
#endif
// remove
#ifndef vcl_remove
#define vcl_remove vcl_generic_cstdio_STD :: remove
#endif
// rewind
#ifndef vcl_rewind
#define vcl_rewind vcl_generic_cstdio_STD :: rewind
#endif
// setbuf
#ifndef vcl_setbuf
#define vcl_setbuf vcl_generic_cstdio_STD :: setbuf
#endif
// setvbuf
#ifndef vcl_setvbuf
#define vcl_setvbuf vcl_generic_cstdio_STD :: setvbuf
#endif
// tmpfile
#ifndef vcl_tmpfile
#define vcl_tmpfile vcl_generic_cstdio_STD :: tmpfile
#endif
// tmpnam
#ifndef vcl_tmpnam
#define vcl_tmpnam vcl_generic_cstdio_STD :: tmpnam
#endif
// ungetc
#ifndef vcl_ungetc
#define vcl_ungetc vcl_generic_cstdio_STD :: ungetc
#endif

// printf() family
// printf
#ifndef vcl_printf
#define vcl_printf vcl_generic_cstdio_STD :: printf
#endif
// sprintf
#ifndef vcl_sprintf
#define vcl_sprintf vcl_generic_cstdio_STD :: sprintf
#endif
// snprintf
#ifndef vcl_snprintf
#define vcl_snprintf vcl_generic_cstdio_STD :: snprintf
#endif
// fprintf
#ifndef vcl_fprintf
#define vcl_fprintf vcl_generic_cstdio_STD :: fprintf
#endif
// vprintf
#ifndef vcl_vprintf
#define vcl_vprintf vcl_generic_cstdio_STD :: vprintf
#endif
// vsprintf
#ifndef vcl_vsprintf
#define vcl_vsprintf vcl_generic_cstdio_STD :: vsprintf
#endif
// vfprintf
#ifndef vcl_vfprintf
#define vcl_vfprintf vcl_generic_cstdio_STD :: vfprintf
#endif

// scanf() family
// scanf
#ifndef vcl_scanf
#define vcl_scanf vcl_generic_cstdio_STD :: scanf
#endif
// sscanf
#ifndef vcl_sscanf
#define vcl_sscanf vcl_generic_cstdio_STD :: sscanf
#endif
// fscanf
#ifndef vcl_fscanf
#define vcl_fscanf vcl_generic_cstdio_STD :: fscanf
#endif
// vscanf
#ifndef vcl_vscanf
#define vcl_vscanf vcl_generic_cstdio_STD :: vscanf
#endif
// vsscanf
#ifndef vcl_vsscanf
#define vcl_vsscanf vcl_generic_cstdio_STD :: vsscanf
#endif
// vfscanf
#ifndef vcl_vfscanf
#define vcl_vfscanf vcl_generic_cstdio_STD :: vfscanf
#endif

#endif // vcl_generic_cstdio_h_
