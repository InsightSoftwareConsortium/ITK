// This is core/vnl/io/vnl_io_real_polynomial.cxx
//:
// \file

#include <iostream>
#include "vnl_io_real_polynomial.h"
#include <vnl/io/vnl_io_vector.h>
#include <vnl/io/vnl_io_matrix.h>


//==============================================================================
//: Binary save self to stream.
void vsl_b_write(vsl_b_ostream & os, const vnl_real_polynomial & p)
{
  constexpr short io_version_no = 1;
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
    std::cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_real_polynomial&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(std::ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//==============================================================================
//: Output a human readable summary to the stream
void vsl_print_summary(std::ostream & os,const vnl_real_polynomial & p)
{
  p.print(os);
}
