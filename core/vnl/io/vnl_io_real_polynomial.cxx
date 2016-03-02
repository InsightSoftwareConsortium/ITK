// This is core/vnl/io/vnl_io_real_polynomial.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include "vnl_io_real_polynomial.h"
#include <vsl/vsl_binary_io.h>
#include <vnl/io/vnl_io_vector.h>
#include <vnl/io/vnl_io_matrix.h>


//==============================================================================
//: Binary save self to stream.
void vsl_b_write(vsl_b_ostream & os, const vnl_real_polynomial & p)
{
  const short io_version_no = 1;
  vsl_b_write(os, io_version_no);

  vsl_b_write(os, p.coefficients());
}

//==============================================================================
//: Binary load self from stream.
void vsl_b_read(vsl_b_istream &is, vnl_real_polynomial & p)
{
  if (!is) return;

  short ver;
  vnl_vector<double> coeffs;
  vsl_b_read(is, ver);
  switch (ver)
  {
   case 1:
    vsl_b_read(is, coeffs);
    p.set_coefficients(coeffs);
    break;

   default:
    vcl_cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_real_polynomial&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(vcl_ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//==============================================================================
//: Output a human readable summary to the stream
void vsl_print_summary(vcl_ostream & os,const vnl_real_polynomial & p)
{
  p.print(os);
}
