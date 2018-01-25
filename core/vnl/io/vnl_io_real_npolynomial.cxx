// This is core/vnl/io/vnl_io_real_npolynomial.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
#include <iostream>
#include "vnl_io_real_npolynomial.h"
#include <vsl/vsl_binary_io.h>
#include <vnl/vnl_real_npolynomial.h>
#include <vnl/io/vnl_io_vector.h>
#include <vnl/io/vnl_io_matrix.h>


//==============================================================================
//: Binary save self to stream.
void vsl_b_write(vsl_b_ostream & os, const vnl_real_npolynomial & p)
{
  const short io_version_no = 1;
  vsl_b_write(os, io_version_no);

  vsl_b_write(os, p.coefficients());
  vsl_b_write(os, p.polyn());
}

//==============================================================================
//: Binary load self from stream.
void vsl_b_read(vsl_b_istream &is, vnl_real_npolynomial & p)
{
  if (!is) return;

  short ver;
  vnl_vector<double> coeffs;
  vnl_matrix<unsigned int> polyn;
  vsl_b_read(is, ver);
  switch (ver)
  {
   case 1:
    vsl_b_read(is, coeffs);
    vsl_b_read(is, polyn);
    p.set(coeffs, polyn);
    break;

   default:
    std::cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_real_npolynomial&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(std::ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//==============================================================================
//: Output a human readable summary to the stream
void vsl_print_summary(std::ostream & os,const vnl_real_npolynomial & p)
{
  os<<"Coefficients: ";
  vsl_print_summary(os, p.coefficients());
  os<<"Polynomial: ";
  vsl_print_summary(os, p.polyn());
}
