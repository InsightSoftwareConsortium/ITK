#ifndef vcl_functional_h_
#define vcl_functional_h_

#include "vcl_compiler.h"

// -------------------- all emulation
#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_functional.h"
# undef vcl_functional_h_STD

// -------------------- gcc with non-standard library
#elif defined(VCL_GCC) && !defined(VCL_CXX_HAS_HEADER_FUNCTIONAL)
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


#if 0 // who needs this? contact fsm if you do.
// Fixes for SunPro and VisualC++ native STL :
#if VCL_USE_NATIVE_STL
# if defined(xxxVCL_SUNPRO_CC) || defined (_MSC_VER)
// Select1st and Select2nd are extensions: they are not part of the standard.
// fsm: So why do we need them?
template <class _Pair>
struct vcl_Select1st : public vcl_unary_function<_Pair, typename _Pair::first_type> {
  typename _Pair::first_type const & operator()(_Pair const & __x) const {
    return __x.first;
  }
};
 
template <class _Pair>
struct vcl_Select2nd : public vcl_unary_function<_Pair, typename _Pair::second_type> {
  typename _Pair::second_type const & operator()(_Pair const & __x) const {
    return __x.second;
  }
};

// Add select* to std.
namespace std {
  template <class _Pair>
  struct select1st : public vcl_Select1st<_Pair> { };
  template <class _Pair> struct select2nd : public vcl_Select2nd<_Pair> { };
};
# endif
#endif
#endif


#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_functional.txx"
#endif

#endif // vcl_functional_h_
