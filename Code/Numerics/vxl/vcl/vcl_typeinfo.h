#error " EXPERIMENTAL FILE. DO NOT USE !!!"

#ifndef vcl_typeinfo_h_
#define vcl_typeinfo_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME vcl_typeinfo
// .INCLUDE vcl_typeinfo.h
// .SECTION Description
//    Provide a uniform interface to the variously supported
//    RTTI's

#include "vcl_compiler.h"

// avoid cpp warning due to these macros being already defined.
#undef VCL_HAS_DYNAMIC_CAST
#undef VCL_HAS_TYPE_INFO

// gcc 295
#ifdef VCL_GCC_295
# include <typeinfo>
# define VCL_HAS_DYNAMIC_CAST 1
# define VCL_HAS_TYPE_INFO    1
#endif

// egcs
#ifdef VCL_EGCS
//hackorama to get round dodgy 1.1.1 stdexcept.h
//# ifdef __STDEXCEPT__
//#  error "sorry, include vcl_typeinfo.h earlier in your file"
//# endif
//# define exception iue_scl_exception
//struct iue_scl_exception { virtual ~iue_scl_exception() = 0; };
//#  include <stdexcepta>
#  include <typeinfo>
//# undef exception

# define VCL_HAS_DYNAMIC_CAST 1
# define VCL_HAS_TYPE_INFO    1
#endif

// SGI CC
#if defined(VCL_SGI_CC_7)
# include <typeinfo.h>
# define VCL_HAS_DYNAMIC_CAST 1
# define VCL_HAS_TYPE_INFO 1
#endif

// poor old gcc
#ifdef VCL_GCC_27
#define VCL_HAS_DYNAMIC_CAST 0
#define VCL_HAS_TYPE_INFO 0
#endif

// other compilers
#ifndef VCL_HAS_DYNAMIC_CAST
# define VCL_HAS_DYNAMIC_CAST 1
#endif
#ifndef VCL_HAS_TYPE_INFO
# define VCL_HAS_TYPE_INFO    0
#endif


// -----------------------------------------------------------------------------

// Now repair the damage.
#if !VCL_HAS_DYNAMIC_CAST
#define dynamic_cast vcl_dynamic_cast

template <class T>
class vcl_dynamic_cast {
public:
  vcl_dynamic_cast(const void*) {}
  operator T () { return 0; }
};
#define VCL_DYNAMIC_CAST_INSTANTIATE(X) template class vcl_dynamic_cast<X*>;

#else
#define VCL_DYNAMIC_CAST_INSTANTIATE(X) 
#endif

//
#if !VCL_HAS_TYPE_INFO
#define typeinfo(x) (x).type_name().c_str()
struct vcl_dummy_typeid {
  const char* name() const { return ""; }
};
#define typeid(x) vcl_dummy_typeid()
#endif

#endif // vcl_typeinfo_h_ 
