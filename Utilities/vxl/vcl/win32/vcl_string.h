#ifndef vcl_vc_string_h_
#define vcl_vc_string_h_
/*
  fsm@robots.ox.ac.uk
*/

#include <string>

#define vcl_basic_string  std::basic_string
#ifndef vcl_char_traits
#define vcl_char_traits   std::char_traits
#endif

//fsm@robots: for some reason, vc60 crashes if vnl_fwd.h is used in conjunction
//with this #define. using a typedef seems to fix it.
//#define vcl_string        std::string
typedef std::string vcl_string;

#if defined(VCL_VC60)
# include <vcl_iostream.h>

inline vcl_ostream& operator<<(vcl_ostream& os, vcl_string const& s)
{
  return os << s.c_str();
}
#endif

#endif // vcl_vc_string_h_
