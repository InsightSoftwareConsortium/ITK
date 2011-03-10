#ifndef vcl_sgi_iomanip_h_
#define vcl_sgi_iomanip_h_

#include <iomanip.h>
#include <vcl_ios.h>
#define vcl_generic_iomanip_STD /* */

// And now some missing <iomanip> manipulators - PVr :
inline ios& showbase(ios& i) { i.setf(ios::showbase); return i; }
inline ios& noshowbase(ios& i) { i.unsetf(ios::showbase); return i; }
inline ios& showpoint(ios& i) { i.setf(ios::showpoint); return i; }
inline ios& noshowpoint(ios& i) { i.unsetf(ios::showpoint); return i; }
inline ios& showpos(ios& i) { i.setf(ios::showpos); return i; }
inline ios& noshowpos(ios& i) { i.unsetf(ios::showpos); return i; }
inline ios& skipws(ios& i) { i.setf(ios::skipws); return i; }
inline ios& noskipws(ios& i) { i.unsetf(ios::skipws); return i; }
inline ios& uppercase(ios& i) { i.setf(ios::uppercase); return i; }
inline ios& nouppercase(ios& i) { i.unsetf(ios::uppercase); return i; }
inline ios& internal(ios& i) { i.unsetf(ios::right|ios::left); i.setf(ios::internal); return i; }
inline ios& left(ios& i) { i.unsetf(ios::right|ios::internal); i.setf(ios::left); return i; }
inline ios& right(ios& i) { i.unsetf(ios::left|ios::internal); i.setf(ios::right); return i; }
inline ios& fixed(ios& i) { i.unsetf(ios::scientific); i.setf(ios::fixed); return i; }
inline ios& scientific(ios& i) { i.unsetf(ios::fixed); i.setf(ios::scientific); return i; }

#include "generic/vcl_iomanip.h"

#endif
