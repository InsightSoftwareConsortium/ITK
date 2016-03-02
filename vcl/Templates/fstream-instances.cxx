#include <vcl_fstream.h>

#if defined(VCL_GCC) && !defined(__APPLE__)
template class std::basic_ofstream<char, std::char_traits<char> >;
#endif
