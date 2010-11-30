// This is core/vnl/vnl_matlab_read.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm
#include <vxl_config.h>
#include "vnl_matlab_read.h"
#include <vcl_ios.h> // for vcl_ios_cur
#include <vcl_iostream.h>
#include <vcl_cstring.h> // memset()
#include <vcl_complex.h>
#include <vnl/vnl_c_vector.h>

//--------------------------------------------------------------------------------

// SGI needs??
void vnl_read_bytes(vcl_istream &s, void *p, unsigned bytes)
{
  s.read((char *)p, bytes);
}

VCL_DEFINE_SPECIALIZATION
void vnl_matlab_read_data(vcl_istream &s, float *p, unsigned n)
{ ::vnl_read_bytes(s, p, n*sizeof(*p)); }

VCL_DEFINE_SPECIALIZATION
void vnl_matlab_read_data(vcl_istream &s, double *p, unsigned n)
{ ::vnl_read_bytes(s, p, n*sizeof(*p)); }

#define implement_read_complex_data(T) \
VCL_DEFINE_SPECIALIZATION \
void vnl_matlab_read_data(vcl_istream &s, vcl_complex<T > *ptr, unsigned n) { \
  T *re = vnl_c_vector<T >::allocate_T(n); \
  T *im = vnl_c_vector<T >::allocate_T(n); \
  ::vnl_read_bytes(s, re, n*sizeof(T)); \
  ::vnl_read_bytes(s, im, n*sizeof(T)); \
  for (unsigned i=0; i<n; ++i) \
    ptr[i] = vcl_complex<T >(re[i], im[i]); \
  vnl_c_vector<T >::deallocate(re, n); \
  vnl_c_vector<T >::deallocate(im, n); \
}

implement_read_complex_data(float )
implement_read_complex_data(double)

#undef implement_read_complex_data

//--------------------------------------------------------------------------------

vnl_matlab_readhdr::vnl_matlab_readhdr(vcl_istream &s_) : s(s_), varname(0), data_read(false), need_swap(false)
{
  read_hdr();
}

vnl_matlab_readhdr::~vnl_matlab_readhdr()
{
  if (varname)
    delete [] varname;
  varname = 0;
}

vnl_matlab_readhdr::operator vnl_matlab_readhdr::safe_bool () const
{
  return (s.good() && !s.eof())? VCL_SAFE_BOOL_TRUE : 0; // FIXME
}

bool vnl_matlab_readhdr::operator!() const
{
  return (s.good() && !s.eof())? false : true; // FIXME
}

bool vnl_matlab_readhdr::is_single() const
{
  return (hdr.type % (10*vnl_matlab_header::vnl_SINGLE_PRECISION)) >= vnl_matlab_header::vnl_SINGLE_PRECISION;
}

bool vnl_matlab_readhdr::is_rowwise() const
{
  return (hdr.type % (10*vnl_matlab_header::vnl_ROW_WISE)) >= vnl_matlab_header::vnl_ROW_WISE;
}

bool vnl_matlab_readhdr::is_bigendian() const
{
  return (hdr.type % (10*vnl_matlab_header::vnl_BIG_ENDIAN)) >= vnl_matlab_header::vnl_BIG_ENDIAN;
}

//: internal
// increment 'current', record the file position and read the header.
void vnl_matlab_readhdr::read_hdr()
{
  vcl_memset(&hdr, 0, sizeof hdr);
  ::vnl_read_bytes(s, &hdr, sizeof(hdr));

  // determine if data needs swapping when read
  // Everything else depends on this; if the header needs swapping
  // and is not, nothing good will happen.
  switch (hdr.type)
  {
    case 0:
      // 0 means double-precision values, column-major, little-endian,
      // so you need to swap if the system is big-endian
#if VXL_BIG_ENDIAN
      need_swap = true;
#endif
      break;
    case 10:
      // Regardless of endian-ness, these flag values are
      // what the writer puts in the header in the native format,
      // therefore if you see any of them, the file is the same-endian
      // as the system you're reading on.
    case 100:
    case 110:
    case 1000:
    case 1100:
    case 1110:
      need_swap = false;
      break;
    default:
      // any other values are either gibberish, or need to be byte-swapped
      // we hope that it means the file needs byte-swapping, and not that
      // the file is corrupt.
      need_swap = true;
  }
  if (need_swap)
  {
    byteswap::swap32(&hdr.type);
    byteswap::swap32(&hdr.rows);
    byteswap::swap32(&hdr.cols);
    byteswap::swap32(&hdr.imag);
    byteswap::swap32(&hdr.namlen);
  }
  if (varname)
    delete [] varname;
  varname = new char[hdr.namlen+1];
#ifdef DEBUG
  vcl_cerr << "type:" << hdr.type << vcl_endl
           << "rows:" << hdr.rows << vcl_endl
           << "cols:" << hdr.cols << vcl_endl
           << "imag:" << hdr.imag << vcl_endl
           << "namlen:" << hdr.namlen << vcl_endl;
#endif
  ::vnl_read_bytes(s, varname, hdr.namlen);
  varname[hdr.namlen] = '\0';

  data_read = false;
}

void vnl_matlab_readhdr::read_next()
{
  if (!data_read) {
    // number of bytes to skip :
    unsigned long n = rows()*cols()*sizeof(float);

    if (!is_single())
      n *= 2;

    if (is_complex())
      n *= 2;
    s.seekg(n, vcl_ios_cur);
  }

  read_hdr();
}

//--------------------------------------------------------------------------------

bool vnl_matlab_readhdr::type_chck(float &) { return is_single() && !is_complex(); }
bool vnl_matlab_readhdr::type_chck(double &) { return !is_single() && !is_complex(); }
bool vnl_matlab_readhdr::type_chck(vcl_complex<float> &) { return is_single() && is_complex(); }
bool vnl_matlab_readhdr::type_chck(vcl_complex<double> &) { return !is_single() && is_complex(); }

#define fsm_define_methods(T) \
bool vnl_matlab_readhdr::read_data(T &v) { \
  if (!type_chck(v)) { vcl_cerr << "type_check\n"; return false; }\
  if (rows()!=1U || cols()!=1U) { vcl_cerr << "size0\n"; return false; } \
  vnl_matlab_read_data(s, &v, 1); \
  if (need_swap) { \
    if (sizeof(v) == 4U) byteswap::swap32(&v); else byteswap::swap64(&v); \
  } \
  data_read = true; return *this; \
} \
bool vnl_matlab_readhdr::read_data(T *p) { \
  if (!type_chck(p[0])) { vcl_cerr << "type_check\n"; return false; } \
  if (rows()!=1U && cols()!=1U) { vcl_cerr << "size1\n"; return false; } \
  vnl_matlab_read_data(s, p, rows()*cols()); \
  if (need_swap) { \
    for (long i = 0; i < rows()*cols(); ++i) { \
      if (sizeof(*p) == 4U) byteswap::swap32(&(p[i])); else byteswap::swap64(&(p[i])); \
    } \
  } \
  data_read = true; return *this; \
} \
bool vnl_matlab_readhdr::read_data(T * const *m) { \
  if (!type_chck(m[0][0])) { vcl_cerr << "type_check\n"; return false; } \
  T *tmp = vnl_c_vector<T >::allocate_T(rows()*cols()); \
  vnl_matlab_read_data(s, tmp, rows()*cols()); \
  if (need_swap) { \
    for (long i = 0; i < rows()*cols(); ++i) { \
      if (sizeof(T) == 4U) byteswap::swap32(&(tmp[i])); else byteswap::swap64(&(tmp[i])); \
    } \
  } \
  int a, b; \
  if (is_rowwise()) { a = cols(); b = 1; } \
  else { a = 1; b = rows(); } \
  for (long i=0; i<rows(); ++i) \
    for (long j=0; j<cols(); ++j) \
      m[i][j] = tmp[a*i + b*j]; \
  vnl_c_vector<T >::deallocate(tmp, rows()*cols()); \
  data_read = true; return *this; \
}

fsm_define_methods(float);
fsm_define_methods(double);
fsm_define_methods(vcl_complex<float>);
fsm_define_methods(vcl_complex<double>);
#undef fsm_define_methods

//--------------------------------------------------------------------------------

#include <vcl_cstdlib.h>
#include <vcl_new.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

template <class T>
bool vnl_matlab_read_or_die(vcl_istream &s,
                            vnl_vector<T> &v,
                            char const *name)
{
  vnl_matlab_readhdr h(s);
  if (!s) // eof?
    return false;
  if (name && *name) {
    if (vcl_strcmp(name, h.name())!=0) { /*wrong name?*/
      vcl_cerr << "vnl_matlab_read_or_die: names do not match\n";
      vcl_abort();
    }
  }
  if (v.size() != (unsigned long)(h.rows()*h.cols()))
  {
    vcl_destroy(&v);
    new (&v) vnl_vector<T>(h.rows()*h.cols());
  }
  if ( ! h.read_data(v.begin()) ) { /*wrong type?*/
    vcl_cerr << "vnl_matlab_read_or_die: failed to read data\n";
    vcl_abort();
  }
  return true;
}

template <class T>
bool vnl_matlab_read_or_die(vcl_istream &s,
                            vnl_matrix<T> &M,
                            char const *name)
{
  vnl_matlab_readhdr h(s);
  if (!s) // eof?
    return false;
  if (name && *name) {
    if (vcl_strcmp(name, h.name())!=0) { /*wrong name?*/
      vcl_cerr << "vnl_matlab_read_or_die: names do not match\n";
      vcl_abort();
    }
  }
  if (M.rows() != (unsigned long)(h.rows()) || M.cols() != (unsigned long)(h.cols()))
  {
    vcl_destroy(&M);
    new (&M) vnl_matrix<T>(h.rows(), h.cols());
  }
  if ( ! h.read_data(M.data_array()) ) { /*wrong type?*/
    vcl_cerr << "vnl_matlab_read_or_die: failed to read data\n";
    vcl_abort();
  }
  return true;
}

#define inst(T) \
template bool vnl_matlab_read_or_die(vcl_istream &, vnl_vector<T> &, char const *); \
template bool vnl_matlab_read_or_die(vcl_istream &, vnl_matrix<T> &, char const *);

inst(double);
inst(float);
