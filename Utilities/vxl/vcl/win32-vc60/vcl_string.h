#ifndef vcl_win32_vc60_string_h_
#define vcl_win32_vc60_string_h_
/* This is only for Visual Studio 6.
   fsm
*/

#include <string>

#define vcl_basic_string  std::basic_string
#ifndef vcl_char_traits
#define vcl_char_traits   std::char_traits
#endif

#ifndef vcl_getline
#define vcl_getline  std::getline
#endif

//fsm: for some reason, vc60 crashes if vnl_fwd.h is used in conjunction
//with this #define. using a typedef seems to fix it.
//#define vcl_string        std::string
typedef std::string vcl_string;

# include <vcl_iostream.h>

inline vcl_ostream& operator<<(vcl_ostream& os, vcl_string const& s)
{
  return os << s.c_str();
}

#endif // vcl_win32_vc60_string_h_
