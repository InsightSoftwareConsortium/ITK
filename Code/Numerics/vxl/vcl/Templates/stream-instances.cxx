#include <vcl_iostream.h>
#include <vcl_iomanip.h>
#include <vcl_fstream.h>

#if defined(VCL_EGCS)
# if !VCL_HAS_TEMPLATE_SYMBOLS
//template class smanip<int>;
template ostream & operator<<(ostream &, smanip<int> const &);
# endif
#endif

#if defined(VCL_GCC_295)
# if defined(GNU_LIBSTDCXX_V3)
#  include <bits/sstream.tcc>
#  include <bits/istream.tcc>
#  include <bits/ostream.tcc>
// for some reason these templates aren't in libstdc++ (yet).
template class std::basic_fstream<char, std::char_traits<char> >;
template class std::basic_ifstream<char, std::char_traits<char> >;
//template class std::basic_ofstream<char, std::char_traits<char> >;
namespace {
  void tic(std::ostream &s, int x) { s << std::setw(14) << x; }
#if 0
  // my 2.95.2 (linux) has no __mbstate_t.
  // maybe they were needed for 2.96? 2.95.2 is more important.
  // fsm
  void toc(std::basic_ostream<char, std::char_traits<char> > &s,
	   std::fpos<__mbstate_t> x) { s.seekp(x); }
  void tac(std::basic_istream<char, std::char_traits<char> > &s,
	   std::fpos<__mbstate_t> x) { s.seekg(x); }
#endif
  //void tuc(std::basic_stringstream<char, std::char_traits<char>, std::allocator<char> > &s,
  //int x) { s << x; }
  template std::basic_stringstream<char, std::char_traits<char>, std::allocator<char> >;
}
# else
#  if !VCL_HAS_TEMPLATE_SYMBOLS
//template class smanip<int>;
template ostream & operator<<(ostream &, smanip<int> const &);
#  endif
# endif
#endif



#if defined(VCL_SUNPRO_CC_50)
std::ostream &operator<<(std::ostream &s, short x)
{ return s << int(x); }




# include <string>
//void blah(std::istream &s1, std::string &s2) { s1 >> s2; }
//
//__type_0&__rwstd::rw_extract_string<std::basic_istream<char,std::char_traits<char> >,std::basic_string<char,std::char_traits<char>,std::allocator<char> >,std::char_traits<char> >(__type_0&,__type_1&,__type_2)

#define type0 std::basic_istream<char,std::char_traits<char> >
#define type1 std::basic_string<char,std::char_traits<char>,std::allocator<char> >
#define type2 std::char_traits<char>

template type0 & __rwstd::rw_extract_string<type0, type1, type2>(type0 &, type1 &, type2);

#undef type0
#undef type1
#undef type2

#endif
