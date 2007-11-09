// This is vcl/Templates/stream-instances.cxx
#include <vcl_iostream.h>
#include <vcl_fstream.h>

#if defined(VCL_EGCS)
# if !VCL_HAS_TEMPLATE_SYMBOLS
//template class smanip<int>;
template vcl_ostream & operator<<(vcl_ostream &, smanip<int> const &);
# endif
#endif

#if defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# if !VCL_HAS_TEMPLATE_SYMBOLS
//template class smanip<int>;
template vcl_ostream & operator<<(vcl_ostream &, smanip<int> const &);
# endif
#endif

#if 0  // not needed as of 2001/05/19
#if defined(GNU_LIBSTDCXX_V3)
# include <bits/sstream.tcc>
# include <bits/istream.tcc>
# include <bits/ostream.tcc>
// for some reason these templates aren't in libstdc++ (yet).
template class std::basic_fstream<char, std::char_traits<char> >;
template class std::basic_ifstream<char, std::char_traits<char> >;
//template class std::basic_ofstream<char, std::char_traits<char> >;
namespace {
  void tic(vcl_ostream &s, int x) { s << std::setw(14) << x; }
  template std::basic_stringstream<char, std::char_traits<char>, std::allocator<char> >;
}
#endif
#endif


#if defined(VCL_SUNPRO_CC_50)
vcl_ostream &operator<<(vcl_ostream &s, short x)
{ return s << int(x); }

# include <string>
//void blah(vcl_istream &s1, std::string &s2) { s1 >> s2; }
//
#define type0 std::basic_istream<char,std::char_traits<char> >
#define type1 std::basic_string<char,std::char_traits<char>,std::allocator<char> >
#define type2 std::char_traits<char>

template type0 & __rwstd::rw_extract_string<type0, type1, type2>(type0 &, type1 &, type2);

#undef type0
#undef type1
#undef type2

#endif
