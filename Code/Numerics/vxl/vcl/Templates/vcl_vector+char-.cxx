// fsm@robots: It is implementation dependent whether a 'char' is
// signed or unsigned. Compilers also differ over whether the 
// unqualified form of char is the same type as the qualified one.

// e.g. 
// gcc thinks that 'char', 'signed char' and 'unsigned char' are
// three different types.
//
// On the other hand, sunpro 5.0 thinks that 'char' and 'signed char'
// are the same, so we mustn't compile the file for sunpro 5.0 (????)

#include <vcl_compiler.h>
#if defined(VCL_GCC_295) || defined(VCL_EGCS) || defined(VCL_GCC_27) || defined(VCL_SUNPRO_CC)

#include <vcl_vector.txx>
VCL_VECTOR_INSTANTIATE(char);

#endif
