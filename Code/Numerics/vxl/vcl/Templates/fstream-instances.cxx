#include <vcl_fstream.h>

#ifdef GNU_LIBSTDCXX_V3 
using std::basic_ofstream;
using std::char_traits;

template class basic_ofstream<char, char_traits<char> >;
#endif
