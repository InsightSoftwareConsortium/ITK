#ifndef vcl_egcs_string_txx_
#define vcl_egcs_string_txx_
/*
  fsm
*/

#include <vcl_string.h>

#undef VCL_BASIC_STRING_INSTANTIATE

#if VCL_HAS_TEMPLATE_SYMBOLS
# define VCL_BASIC_STRING_INSTANTIATE(charT, Traits) // no need -- in libstdc++
#else
# define VCL_BASIC_STRING_INSTANTIATE(charT, Traits) \
template class basic_string<charT, Traits >; \
template basic_string<charT,Traits > &basic_string<charT,Traits >::replace(char*, char*, char *, char*);\
template basic_string<charT,Traits > &basic_string<charT,Traits >::replace(char*, char*, char const*, char const*);\
template basic_string<charT,Traits > &basic_string<charT,Traits >::replace(vcl_size_t, vcl_size_t,\
                                                                           basic_string<charT,Traits > const&,\
                                                                           vcl_size_t, vcl_size_t);\
template basic_string<charT,Traits > &basic_string<charT,Traits >::replace(vcl_size_t, vcl_size_t, char const*, vcl_size_t);\
template basic_string<charT,Traits > &basic_string<charT,Traits >::replace(vcl_size_t, vcl_size_t, vcl_size_t, char); \
template vcl_ostream& operator<<(vcl_ostream&, basic_string<charT, Traits > const &)
#endif

#endif
