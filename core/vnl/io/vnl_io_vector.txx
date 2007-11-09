// This is core/vnl/io/vnl_io_vector.txx
#ifndef vnl_io_vector_txx_
#define vnl_io_vector_txx_
//:
// \file

#include "vnl_io_vector.h"
#include <vsl/vsl_binary_io.h>
#include <vsl/vsl_b_read_block_old.h>
#include <vsl/vsl_block_binary.h>

//=================================================================================
//: Binary save self to stream.
template<class T>
void vsl_b_write(vsl_b_ostream & os, const vnl_vector<T> & p)
{
  const short io_version_no = 2;
  vsl_b_write(os, io_version_no);
  vsl_b_write(os, p.size());
  if (p.size())
    vsl_block_binary_write(os, p.begin(), p.size());
}

//=================================================================================
//: Binary load self from stream.
template<class T>
void vsl_b_read(vsl_b_istream &is, vnl_vector<T> & p)
{
  if (!is) return;

  short ver;
  unsigned n;
  vsl_b_read(is, ver);
  switch (ver)
  {
   case 1:
    vsl_b_read(is, n);
    p.set_size(n);
    if (n)
      vsl_b_read_block_old(is, p.data_block(), n);
    break;

   case 2:
    vsl_b_read(is, n);
    p.set_size(n);
    if (n)
      vsl_block_binary_read(is, p.data_block(), n);
    break;

   default:
    vcl_cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_vector<T>&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(vcl_ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//====================================================================================
//: Output a human readable summary to the stream
template<class T>
void vsl_print_summary(vcl_ostream & os,const vnl_vector<T> & p)
{
  os<<"Len: "<<p.size()<<" (";
  for ( unsigned int i =0; i < p.size() && i < 5; ++i )
    os << p.operator()(i) <<' ';
  if (p.size() > 5) os << " ...";
  os << ')';
}

#define VNL_IO_VECTOR_INSTANTIATE(T) \
template void vsl_print_summary(vcl_ostream &, const vnl_vector<T > &); \
template void vsl_b_read(vsl_b_istream &, vnl_vector<T > &); \
template void vsl_b_write(vsl_b_ostream &, const vnl_vector<T > &)

#endif // vnl_io_vector_txx_
