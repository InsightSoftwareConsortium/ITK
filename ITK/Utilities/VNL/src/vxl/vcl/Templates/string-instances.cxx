#include <vcl_string.txx>
#include <vcl_iostream.h>

VCL_BASIC_STRING_INSTANTIATE(char, vcl_char_traits<char> );

#if defined(VCL_SUNPRO_CC)
template class vcl_basic_string<char, vcl_char_traits<char> >;
#endif

// this should work for all compilers. by putting it in the
// library we (a) get the implicit template instances it
// needs and (b) make sure that it does work.
static void vcl_string_instance_tickler(vcl_ostream &os, vcl_string::iterator i, char *a, char const *b)
{
  char ch;
  vcl_char_traits<char>::eq(ch, ch);
  vcl_string s(b, b);
  os << s;
  s.replace(i, i, a, a);
  s.replace(i, i, b, b);
  //s.find(i);
  s.find(a);
  s.find(b);

  vcl_string(s.begin()+3, s.end()) == b;
  s + s;
  vcl_string_instance_tickler(os, i, a, b); // avoid "unused function" warning
}


#if defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# if VCL_HAS_TEMPLATE_SYMBOLS
#  undef bs
#  define bs basic_string<char, string_char_traits<char>, __default_alloc_template<false, 0> >
template bs &bs::replace<char*>(char *, char *, char *, char *);
template bs &bs::replace<char const*>(char *, char *, char const *, char const *);
# else
// The following is needed when using -fguiding-decls.
#  undef inst
#  define inst \
template class __default_alloc_template<true, 0>; \
template bs &bs::replace(char *, char *, char *, char *); \
template bs &bs::replace(char *, char *, char const *, char const *); \
template bs &bs::replace(vcl_size_t, vcl_size_t, bs const &, vcl_size_t, vcl_size_t); \
template bs &bs::replace(vcl_size_t, vcl_size_t, char const *, vcl_size_t); \
template bs &bs::replace(vcl_size_t, vcl_size_t, vcl_size_t, char)

#  undef bs
#  define bs basic_string<char, string_char_traits<char>, __default_alloc_template<true , 0> >
inst;
#  undef bs
#  define bs basic_string<char, string_char_traits<char>, __default_alloc_template<false, 0> >
inst;
# endif
#endif

#if defined(VCL_SGI_CC)
# if VCL_USE_NATIVE_STL
template class std::__string_base<char,std::__default_alloc_template<true,0> >;
//template class std::basic_string<char,std::char_traits<char>,std::alloc>;
template class std::basic_string<char,std::char_traits<char>,std::__default_alloc_template<true,0> >;
template vcl_ostream& std::operator<<(vcl_ostream&,const std::basic_string<char,std::char_traits<char>,std::alloc>&);
# else
#  undef bs
#  define bs vcl_basic_string<char, vcl_char_traits<char> >
#   if 0 // already explicitly instantiated elsewhere ?! but needed on julia?
template bs &bs::replace(vcl_size_t, vcl_size_t, char const*, vcl_size_t);
template bs &bs::replace(vcl_size_t, vcl_size_t, vcl_size_t, char);
template int bs::compare(char const*, vcl_size_t, vcl_size_t) const;
template int bs::compare(bs const&, vcl_size_t, vcl_size_t) const;
#   endif
# endif
#endif
