#ifndef vcl_functional_h_
#define vcl_functional_h_

#include "vcl_compiler.h"

// -------------------- gcc with non-standard library
#if defined(VCL_GCC) && !defined(VCL_CXX_HAS_HEADER_FUNCTIONAL)
# include <function.h>
# define vcl_functional_h_STD ::

// -------------------- iso
#else
# include "iso/vcl_functional.h"
# define vcl_alloc std::alloc
#endif

// Now #define vcl_blah to std::blah (except for emulation) :
#if defined(vcl_functional_h_STD)
# define  vcl_unary_function      vcl_functional_h_STD unary_function
# define  vcl_binary_function     vcl_functional_h_STD binary_function
# define  vcl_plus                vcl_functional_h_STD plus
# define  vcl_minus               vcl_functional_h_STD minus
# define  vcl_multiplies          vcl_functional_h_STD multiplies
# define  vcl_divides             vcl_functional_h_STD divides
# define  vcl_modulus             vcl_functional_h_STD modulus
# define  vcl_negate              vcl_functional_h_STD negate
# define  vcl_equal_to            vcl_functional_h_STD equal_to
# define  vcl_not_equal_to        vcl_functional_h_STD not_equal_to
# define  vcl_greater             vcl_functional_h_STD greater
# define  vcl_less                vcl_functional_h_STD less
# define  vcl_greater_equal       vcl_functional_h_STD greater_equal
# define  vcl_less_equal          vcl_functional_h_STD less_equal
# define  vcl_logical_and         vcl_functional_h_STD logical_and
# define  vcl_logical_or          vcl_functional_h_STD logical_or
# define  vcl_logical_not         vcl_functional_h_STD logical_not
# define  vcl_identity            vcl_functional_h_STD identity
//nonstd # define  vcl_select1st           vcl_functional_h_STD select1st
//nonstd # define  vcl_select2nd           vcl_functional_h_STD select2nd
# define  vcl_project1st          vcl_functional_h_STD project1st
# define  vcl_project2nd          vcl_functional_h_STD project2nd
# define  vcl_constant_void_fun   vcl_functional_h_STD constant_void_fun
# define  vcl_constant_unary_fun  vcl_functional_h_STD constant_unary_fun
# define  vcl_constant_binary_fun vcl_functional_h_STD constant_binary_fun
#endif

#include "vcl_functional.hxx"

#endif // vcl_functional_h_
