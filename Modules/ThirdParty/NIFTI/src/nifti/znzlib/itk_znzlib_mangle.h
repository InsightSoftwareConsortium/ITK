#ifndef itk_znzlib_mangle_h
#define itk_znzlib_mangle_h

/*
This header file mangles all symbols exported from the znzlib library.
It is intended to be included by ITK's version "znzlib.h".

It is the counterpart of itk_nifti_mangle.h: without it ITKznz exports the
plain znzlib names, which collide with any other znzlib in the same process
(e.g. a system nifti_clib pulled in by another library).

Note: znzclose() is a macro over Xznzclose(), so only Xznzclose is mangled;
the macro expands to the mangled name at the call site.
*/

#define Xznzclose itk_Xznzclose
#define znzdopen itk_znzdopen
#define znzeof itk_znzeof
#define znzflush itk_znzflush
#define znzgetc itk_znzgetc
#define znzgets itk_znzgets
#define znzopen itk_znzopen
#define znzprintf itk_znzprintf
#define znzputc itk_znzputc
#define znzputs itk_znzputs
#define znzread itk_znzread
#define znzrewind itk_znzrewind
#define znzseek itk_znzseek
#define znztell itk_znztell
#define znzwrite itk_znzwrite

#endif
