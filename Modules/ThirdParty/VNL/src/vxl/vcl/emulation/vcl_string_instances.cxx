#include <vcl_compiler.h>
#if !VCL_USE_NATIVE_STL

#include <vcl_string.h>
#include <vcl_string.txx>

// We only want the instances to see this
#ifdef VCL_SUNPRO_CC
template <class charT, class traits>
const vcl_size_t basic_string <charT, traits>::npos = (vcl_size_t)(-1);
#endif
// A dummy symbol to avoid missing symbol warnings from ranlib
void vcl_string_instances_dummy_to_avoid_ranlib_warning() {}

//template class vcl_basic_string<char, vcl_char_traits<char> >;
//VCL_BASIC_STRING_INSTANTIATE(char, vcl_char_traits<char>);

#endif // VCL_USE_NATIVE_STL
