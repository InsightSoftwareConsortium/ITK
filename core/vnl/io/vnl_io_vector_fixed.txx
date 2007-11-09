// This is core/vnl/io/vnl_io_vector_fixed.txx
#ifndef vnl_io_vector_fixed_txx_
#define vnl_io_vector_fixed_txx_
//:
// \file

#include "vnl_io_vector_fixed.h"
#include <vsl/vsl_binary_io.h>
#include <vsl/vsl_b_read_block_old.h>
#include <vsl/vsl_block_binary.h>

//=================================================================================
//: Binary save self to stream.
template<class T, unsigned int n>
void vsl_b_write(vsl_b_ostream & os, const vnl_vector_fixed<T,n> & p)
{
  const short io_version_no = 2;
  vsl_b_write(os, io_version_no);
  vsl_b_write(os, p.size());
  if (p.size())
    vsl_block_binary_write(os, p.data_block(), p.size());
}

//=================================================================================
//: Binary load self from stream.
template<class T, unsigned int n>
void vsl_b_read(vsl_b_istream &is, vnl_vector_fixed<T,n> & p)
{
  if (!is) return;

  short ver;
  unsigned stream_n;
  vsl_b_read(is, ver);
  switch (ver)
  {
   case 1:
    vsl_b_read(is, stream_n);
    if ( n == stream_n ) {
      vsl_b_read_block_old(is, p.begin(), n);
    } else {
      vcl_cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_vector_fixed<T,n>&)\n"
               << "           Expected n="<<n<<", got "<<stream_n<<'\n';
      is.is().clear(vcl_ios::badbit); // Set an unrecoverable IO error on stream
      return;
    }
    break;

   case 2:
    vsl_b_read(is, stream_n);
    if ( n == stream_n ) {
      vsl_block_binary_read(is, p.data_block(), n);
    } else {
      vcl_cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_vector_fixed<T,n>&)\n"
               << "           Expected n="<<n<<", got "<<stream_n<<'\n';
      is.is().clear(vcl_ios::badbit); // Set an unrecoverable IO error on stream
      return;
    }
    break;

   default:
    vcl_cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_vector_fixed<T,n>&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(vcl_ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//====================================================================================
//: Output a human readable summary to the stream
template<class T, unsigned int n>
void vsl_print_summary(vcl_ostream & os,const vnl_vector_fixed<T,n> & p)
{
  os<<"Len: "<<p.size()<<" [fixed] (";
  for ( unsigned int i =0; i < p.size() && i < 5; ++i )
    os << p(i) <<' ';
  if (p.size() > 5) os << " ...";
  os << ')';
}

#define VNL_IO_VECTOR_FIXED_INSTANTIATE(T,n) \
template void vsl_print_summary(vcl_ostream &, const vnl_vector_fixed<T,n > &); \
template void vsl_b_read(vsl_b_istream &, vnl_vector_fixed<T,n > &); \
template void vsl_b_write(vsl_b_ostream &, const vnl_vector_fixed<T,n > &)

#endif // vnl_io_vector_fixed_txx_
