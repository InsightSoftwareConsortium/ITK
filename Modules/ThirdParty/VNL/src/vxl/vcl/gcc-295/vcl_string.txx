#ifndef vcl_gcc295_string_txx_
#define vcl_gcc295_string_txx_
/*
  fsm
*/

#include <vcl_string.h>

#undef VCL_BASIC_STRING_INSTANTIATE
#define VCL_BASIC_STRING_INSTANTIATE(charT, Traits) \
template class vcl_basic_string<charT, Traits >

#endif // vcl_gcc295_string_txx_
