/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif
#include "vnl_matlab_read.h"
#include <vcl_iostream.h>
#include <vcl_cstdlib.h> // abort()
#include <vcl_cstring.h> // memset()
#include <vnl/vnl_c_vector.h>
#include <vnl/vnl_complex.h>
#include <vnl/vnl_complex_traits.h>

// FIXME: Currently ignores the byte ordering of the MAT file header, effectively
// assuming the MAT file was written with the native byte ordering.

//--------------------------------------------------------------------------------

// SGI needs??
void vnl_read_bytes(vcl_istream &s, void *p, unsigned bytes) {
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
void vnl_matlab_read_data(vcl_istream &s, vnl_complex<T > *ptr, unsigned n) { \
  T *re = vnl_c_vector<T >::allocate_T(n); \
  T *im = vnl_c_vector<T >::allocate_T(n); \
  ::vnl_read_bytes(s, re, n*sizeof(T)); \
  ::vnl_read_bytes(s, im, n*sizeof(T)); \
  for (unsigned i=0; i<n; ++i) \
    ptr[i] = vnl_complex<T >(re[i], im[i]); \
  vnl_c_vector<T >::deallocate(re, n); \
  vnl_c_vector<T >::deallocate(im, n); \
}

//#if VCL_CAN_DO_PARTIAL_SPECIALIZATION
//template <class T> implement_read_complex_data(T)
//template void vnl_matlab_read_data(istream &, vnl_complex<float > *, unsigned);
//template void vnl_matlab_read_data(istream &, vnl_complex<double> *, unsigned);
//#else
implement_read_complex_data(float )
implement_read_complex_data(double)
//#endif

#undef implement_read_complex_data

//--------------------------------------------------------------------------------

vnl_matlab_readhdr::vnl_matlab_readhdr(vcl_istream &s_) : s(s_), varname(0), data_read(false) {
  read_hdr();
}

vnl_matlab_readhdr::~vnl_matlab_readhdr() {
  if (varname)
    delete [] varname;
  varname = 0;
}

vnl_matlab_readhdr::operator bool () const {
  return s.good() && !s.eof(); // FIXME || s.good(); // FIXME
}

bool vnl_matlab_readhdr::is_single() const {
  return (hdr.type % (10*vnl_matlab_header::vnl_SINGLE_PRECISION)) >= vnl_matlab_header::vnl_SINGLE_PRECISION;
}

bool vnl_matlab_readhdr::is_rowwise() const {
  return (hdr.type % (10*vnl_matlab_header::vnl_ROW_WISE)) >= vnl_matlab_header::vnl_ROW_WISE;
}

bool vnl_matlab_readhdr::is_bigendian() const {
  return (hdr.type % (10*vnl_matlab_header::vnl_BIG_ENDIAN)) >= vnl_matlab_header::vnl_BIG_ENDIAN;
}

//: internal
// increment 'current', record the file position and read the header.
void vnl_matlab_readhdr::read_hdr() {
  memset(&hdr, 0, sizeof hdr);
  ::vnl_read_bytes(s, &hdr, sizeof(hdr));
  if (varname)
    delete [] varname;
  varname = new char[hdr.namlen+1];
// cerr << "type:" << hdr.type << endl;
// cerr << "rows:" << hdr.rows << endl;
// cerr << "cols:" << hdr.cols << endl;
// cerr << "imag:" << hdr.imag << endl;
// cerr << "namlen:" << hdr.namlen << endl;

  ::vnl_read_bytes(s, varname, hdr.namlen);
  varname[hdr.namlen] = '\0';

  data_read = false;
}

void vnl_matlab_readhdr::read_next() {
  if (!data_read) {
    // number of bytes to skip :
    unsigned n = rows()*cols()*sizeof(float);

    if (!is_single())
      n *= 2;

    if (is_complex())
      n *= 2;
    s.seekg(n, vcl_ios::cur);
  }
  
  read_hdr();
}

//--------------------------------------------------------------------------------

bool vnl_matlab_readhdr::type_chck(float &) { return is_single() && !is_complex(); }
bool vnl_matlab_readhdr::type_chck(double &) { return !is_single() && !is_complex(); }
bool vnl_matlab_readhdr::type_chck(vnl_float_complex &) { return is_single() && is_complex(); }
bool vnl_matlab_readhdr::type_chck(vnl_double_complex &) { return !is_single() && is_complex(); }

#define fsm_define_methods(T) \
bool vnl_matlab_readhdr::read_data(T &v) { \
  if (!type_chck(v)) { vcl_cerr << "type_check" << vcl_endl; return false; }\
  if (rows()!=1 || cols()!=1) { vcl_cerr << "size0" << vcl_endl; return false; } \
  vnl_matlab_read_data(s, &v, 1); \
  data_read = true; return *this; \
} \
bool vnl_matlab_readhdr::read_data(T *p) { \
  if (!type_chck(p[0])) { vcl_cerr << "type_check" << vcl_endl; return false; } \
  if (rows()!=1 && cols()!=1) { vcl_cerr << "size1" << vcl_endl; return false; } \
  vnl_matlab_read_data(s, p, rows()*cols()); \
  data_read = true; return *this; \
} \
bool vnl_matlab_readhdr::read_data(T * const *m) { \
  if (!type_chck(m[0][0])) { vcl_cerr << "type_check" << vcl_endl; return false; } \
  T *tmp = vnl_c_vector<T>::allocate_T(rows()*cols()); \
  /*vnl_c_vector<T >::fill(tmp, rows()*cols(), 3.14159);*/ \
  vnl_matlab_read_data(s, tmp, rows()*cols()); \
  int a, b; \
  if (is_rowwise()) { \
    a = cols(); \
    b = 1; \
  } \
  else { \
    a = 1; \
    b = rows(); \
  } \
  for (int i=0; i<rows(); ++i) \
    for (int j=0; j<cols(); ++j) \
      m[i][j] = tmp[a*i + b*j]; \
  vnl_c_vector<T>::deallocate(tmp, rows()*cols()); \
  data_read = true; return *this; \
}
fsm_define_methods(float);
fsm_define_methods(double);
fsm_define_methods(vnl_float_complex);
fsm_define_methods(vnl_double_complex);
#undef fsm_define_methods

//--------------------------------------------------------------------------------

#include <vcl_cassert.h>
#include <vcl_new.h>
#include <vcl_algorithm.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

template <class T> 
bool vnl_matlab_read_or_die(vcl_istream &s, 
			    vnl_vector<T> &v,
			    char const *name VCL_DEFAULT_VALUE(0)) 
{
  vnl_matlab_readhdr h(s);
  if (!s) // eof?
    return false;
  //assert(s/*bad stream?*/);
  if (name && *name)
    assert(strcmp(name, h.name())==0/*wrong name?*/);
  if (v.size() != unsigned(h.rows()*h.cols())) {
    vcl_destroy(&v);
    new (&v) vnl_vector<T>(h.rows()*h.cols());
  }
  assert(h.read_data(v.begin())/*wrong type?*/);
  return true;
}
  
template <class T> 
bool vnl_matlab_read_or_die(vcl_istream &s, 
			    vnl_matrix<T> &M, 
			    char const *name VCL_DEFAULT_VALUE(0))
{
  vnl_matlab_readhdr h(s);
  if (!s) // eof?
    return false;
  //assert(s/*bad stream?*/);
  if (name && *name)
    assert(strcmp(name, h.name())==0/*wrong name?*/);
  if (M.rows() != unsigned(h.rows()) || M.cols() != unsigned(h.cols())) {
    vcl_destroy(&M);
    new (&M) vnl_matrix<T>(h.rows(), h.cols());
  }
  assert(h.read_data(M.data_array())/*wrong type?*/);
  return true;
}

#define inst(T) \
template bool vnl_matlab_read_or_die(vcl_istream &, vnl_vector<T> &, char const *); \
template bool vnl_matlab_read_or_die(vcl_istream &, vnl_matrix<T> &, char const *); 

inst(double);
inst(float);
