#include <vcl_fstream.h>

#if defined(GNU_LIBSTDCXX_V3) && !defined(__APPLE__) 
template class std::basic_ofstream<char, std::char_traits<char> >;
#endif
