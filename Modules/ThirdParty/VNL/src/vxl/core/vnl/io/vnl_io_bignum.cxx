// This is core/vnl/io/vnl_io_bignum.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include "vnl_io_bignum.h"
#include <vsl/vsl_binary_io.h>

//=================================================================================
//: Binary save self to stream.
void vsl_b_write(vsl_b_ostream & os, const vnl_bignum & p)
{
  const short io_version_no = 1;
  vsl_b_write(os, io_version_no);
  std::string s;
  vnl_bignum_to_string(s, p);
  vsl_b_write(os, s);
}

//=================================================================================
//: Binary load self from stream.
void vsl_b_read(vsl_b_istream &is, vnl_bignum & p)
{
  if (!is) return;
  short ver;
  std::string s;
  vsl_b_read(is, ver);
  switch (ver)
  {
   case 1:
    vsl_b_read(is, s);
    vnl_bignum_from_string(p, s);
    break;

   default:
    std::cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_bignum&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(std::ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//====================================================================================
//: Output a human readable summary to the stream
void vsl_print_summary(std::ostream & os,const vnl_bignum & p)
{
  os<<p;
}
