// This is core/vnl/io/vnl_io_rational.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include "vnl_io_rational.h"
#include <vsl/vsl_binary_io.h>

//=================================================================================
//: Binary save self to stream.
void vsl_b_write(vsl_b_ostream & os, const vnl_rational & p)
{
  const short io_version_no = 1;
  vsl_b_write(os, io_version_no);
  vsl_b_write(os, p.numerator());
  vsl_b_write(os, p.denominator());
}

//=================================================================================
//: Binary load self from stream.
void vsl_b_read(vsl_b_istream &is, vnl_rational & p)
{
  if (!is) return;
  short ver;
  long n, d;
  vsl_b_read(is, ver);
  switch (ver)
  {
   case 1:
    vsl_b_read(is, n);
    vsl_b_read(is, d);
    p.set(n,d);
    break;

   default:
    std::cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_rational&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(std::ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//====================================================================================
//: Output a human readable summary to the stream
void vsl_print_summary(std::ostream & os,const vnl_rational & p)
{
  os<<p;
}
