#ifndef vcl_sgi_string_h_
#define vcl_sgi_string_h_

# include "iso/vcl_string.h"

inline bool operator!=(vcl_string const& a, vcl_string const& b) { return !(a==b); }

#endif // vcl_sgi_string_h_
