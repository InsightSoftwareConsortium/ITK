// This is core/vnl/io/vnl_io_matrix_fixed.hxx
#ifndef vnl_io_matrix_fixed_hxx_
#define vnl_io_matrix_fixed_hxx_
//:
// \file

#include "vnl_io_matrix_fixed.h"
#include <vnl/vnl_matrix_fixed.h>
#include <vsl/vsl_b_read_block_old.h>
#include <vsl/vsl_block_binary.h>
#include <vsl/vsl_indent.h>

//=================================================================================
//: Binary save self to stream.
template<class T, unsigned m, unsigned n>
void vsl_b_write(vsl_b_ostream & os, const vnl_matrix_fixed<T,m,n> & p)
{
  const short version_no = 2;
  vsl_b_write(os, version_no);
  vsl_b_write(os, p.rows());
  vsl_b_write(os, p.cols());

  // Calling p.begin() on empty matrix_fixed causes segfault
  if (p.size()>0)
    vsl_block_binary_write(os, p.data_block(), m*n);
}

//=================================================================================
//: Binary load self from stream.
template<class T, unsigned m, unsigned n>
void vsl_b_read(vsl_b_istream &is, vnl_matrix_fixed<T,m,n> & p)
{
  if (!is) return;

  short v;
  unsigned stream_m, stream_n;
  vsl_b_read(is, v);
  switch (v)
  {
   case 1:
    vsl_b_read(is, stream_m);
    vsl_b_read(is, stream_n);
    if ( stream_n != n || stream_m != m ) {
      std::cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_matrix_fixed<T>&)\n"
               << "           Expected size " << m << ',' << n << "; got " << stream_m << ',' << stream_n << '\n';
      is.is().clear(std::ios::badbit); // Set an unrecoverable IO error on stream
      return;
    }
    // Calling begin() on empty matrix_fixed causes segfault
    if (m*n>0)
      vsl_b_read_block_old(is, p.begin(), p.size());
    break;

   case 2:
    vsl_b_read(is, stream_m);
    vsl_b_read(is, stream_n);
    if ( stream_n != n || stream_m != m ) {
      std::cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_matrix_fixed<T>&)\n"
               << "           Expected size " << m << ',' << n << "; got " << stream_m << ',' << stream_n << '\n';
      is.is().clear(std::ios::badbit); // Set an unrecoverable IO error on stream
      return;
    }
    // Calling begin() on empty matrix_fixed causes segfault
    if (m*n>0)
      vsl_block_binary_read(is, p.data_block(), m*n);
    break;

   default:
    std::cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_matrix_fixed<T>&)\n"
             << "           Unknown version number "<< v << '\n';
    is.is().clear(std::ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//====================================================================================
//: Output a human readable summary to the stream
template<class T, unsigned nrows, unsigned ncols>
void vsl_print_summary(std::ostream & os,const vnl_matrix_fixed<T,nrows,ncols> & p)
{
  os<<"Size: "<<p.rows()<<" x "<<p.cols()<<std::endl;

  unsigned int m = 5; unsigned int n = 5;


  if (m>p.rows()) m=p.rows();
  if (n>p.cols()) n=p.cols();

  vsl_indent_inc(os);
  for (unsigned int i=0;i<m;i++)
  {
     os<<vsl_indent()<<" (";

     for ( unsigned int j=0; j<n; j++)
        os<<p(i,j)<<' ';
      if (p.cols()>n) os<<"...";
        os<<")\n";
  }
  if (p.rows()>m) os <<vsl_indent()<<" (...\n";
  vsl_indent_dec(os);
}


#define VNL_IO_MATRIX_FIXED_INSTANTIATE(T,m,n) \
template void vsl_print_summary(std::ostream &, const vnl_matrix_fixed<T,m,n > &); \
template void vsl_b_read(vsl_b_istream &, vnl_matrix_fixed<T,m,n > &); \
template void vsl_b_write(vsl_b_ostream &, const vnl_matrix_fixed<T,m,n > &)

#endif // vnl_io_matrix_fixed_hxx_
