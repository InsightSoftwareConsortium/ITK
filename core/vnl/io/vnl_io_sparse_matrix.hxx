// This is core/vnl/io/vnl_io_sparse_matrix.hxx
#ifndef vnl_io_sparse_matrix_hxx_
#define vnl_io_sparse_matrix_hxx_
//:
// \file

#include <iostream>
#include "vnl_io_sparse_matrix.h"
#include <vnl/vnl_sparse_matrix.h>
#include <vsl/vsl_binary_io.h>
#include <vcl_cassert.h>

// I/O for vnl_sparse_matrix_pair
//==================================================================================
// IO Helper functions
//==================================================================================

//=================================================================================
//: Binary save self to stream.
template<class T>
void vsl_b_write(vsl_b_ostream &os, const vnl_sparse_matrix_pair<T> & p)
{
  const short io_version_no = 1;
  vsl_b_write(os, io_version_no);
  vsl_b_write(os, p.first);
  vsl_b_write(os, p.second);
}

//=================================================================================
//: Binary load self from stream.
template<class T>
void vsl_b_read(vsl_b_istream &is, vnl_sparse_matrix_pair<T> & p)
{
  if (!is) return;

  short ver;
  vsl_b_read(is, ver);
  switch (ver)
  {
   case 1:
    vsl_b_read(is, p.first);
    vsl_b_read(is, p.second);
    break;

   default:
    std::cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_sparse_matrix_pair<T>&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(std::ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//================================================================================
//: Output a human readable summary to the stream
template<class T>
void vsl_print_summary(std::ostream& os,const vnl_sparse_matrix_pair<T>& p)
{
  os<< "Sparse matrix pair ( " << p.first << ',' << p.second << " )\n";
}

// I/O for vnl_sparse_matrix

//=================================================================================
//: Binary save self to stream.
template<class T>
void vsl_b_write(vsl_b_ostream & os, const vnl_sparse_matrix<T> & p)
{
  typedef vnl_sparse_matrix_pair<T> pair_t;
  typedef std::vector < pair_t > row;

  row rw;
  vnl_sparse_matrix<T> v=p;

  const short io_version_no = 1;
  vsl_b_write(os, io_version_no);
  vsl_b_write(os, v.rows());
  vsl_b_write(os, v.columns());

  for (unsigned int i=0;i<v.rows();i++)
  {
    rw=v.get_row(i);
    vsl_b_write(os, rw.size());
    for (unsigned int j=0;j<rw.size();j++)
    {
      vsl_b_write(os, rw[j]);
    }
  }
}

//=================================================================================
//: Binary load self from stream.
template<class T>
void vsl_b_read(vsl_b_istream &is, vnl_sparse_matrix<T> & p)
{
  if (!is) return;

  typedef vnl_sparse_matrix_pair<T> pair_t;

  short ver;
  unsigned n_rows;
  unsigned n_cols;
  unsigned row_size=0;
  vsl_b_read(is, ver);

  std::vector<int> indexes(row_size);
  std::vector<T> values(row_size);
  switch (ver)
  {
   case 1:
    vsl_b_read(is, n_rows);
    vsl_b_read(is, n_cols);
    // As we cannot resize the matrix, check that it is the correct size.
    assert (n_rows==p.rows());
    assert (n_cols==p.columns());
    for (unsigned i=0;i<n_rows;++i)
    {
      vsl_b_read(is,row_size);
      indexes.resize(row_size);
      values.resize(row_size);

      for (unsigned j=0;j<row_size;j++)
      {
        pair_t q;
        vsl_b_read(is, q);
        indexes[j] = q.first;
        values[j] = q.second;
      }
      p.set_row(i, indexes, values);
    }
    break;

   default:
    std::cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_sparse_matrix<T>&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(std::ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//====================================================================================
//: Output a human readable summary to the stream
template<class T>
void vsl_print_summary(std::ostream & os,const vnl_sparse_matrix<T> & p)
{
  os<<"Rows x Columns: "<<p.rows()<<" x "<<p.columns()<<std::endl;
  vnl_sparse_matrix<T> v=p;
  v.reset();
  v.next();
  for (int i=0;i<5;i++)
  {
    os<<" ("<< v.getrow() <<','<< v.getcolumn() <<") value "<< v.value()<<'\n';
    if (!v.next()) break;
  }
}

#define VNL_IO_SPARSE_MATRIX_INSTANTIATE(T) \
  template VNL_TEMPLATE_EXPORT void vsl_print_summary(std::ostream &, const vnl_sparse_matrix<T > &); \
  template VNL_TEMPLATE_EXPORT void vsl_b_read(vsl_b_istream &, vnl_sparse_matrix<T > &); \
  template VNL_TEMPLATE_EXPORT void vsl_b_write(vsl_b_ostream &, const vnl_sparse_matrix<T > &)

#endif // vnl_io_sparse_matrix_hxx_
