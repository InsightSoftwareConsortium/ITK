#ifndef vcl_memory_h_
#define vcl_memory_h_

#include <memory>
#include  <utility>
#include "vcl_compiler.h"

// Needed to provide backwards compatibility between C++11 and older compilers
// https://softwareengineering.stackexchange.com/questions/291141/how-to-handle-design-changes-for-auto-ptr-deprecation-in-c11
#if __cplusplus >= 201103L || (defined(_CPPLIB_VER) && _CPPLIB_VER >= 520)
    template <typename T>
    using vcl_unique_ptr = std::unique_ptr<T>;
    #define vcl_move( value ) std::move(value)
#else
// NOTE:  THIS DOES NOT MEET THE STANDARDS FOR A UNIQUE POINTER!
#   define vcl_unique_ptr std::auto_ptr
#   define vcl_move( value ) value
#endif

#endif // vcl_memory_h_
