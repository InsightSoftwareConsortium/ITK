// This is core/vnl/vnl_matlab_write.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm

#include "vnl_matlab_write.h"

#include <vcl_iostream.h>
#include <vcl_cstring.h>  // strlen()
#include <vcl_complex.h>
#include <vnl/vnl_matlab_header.h>

#include <vxl_config.h>
#if VXL_LITTLE_ENDIAN // #ifdef i386
# define native_BYTE_ORDER vnl_matlab_header::vnl_LITTLE_ENDIAN
#else
# define native_BYTE_ORDER vnl_matlab_header::vnl_BIG_ENDIAN
#endif

// SGI needs char * as first argument to vcl_ostream::write
void vnl_write_bytes(vcl_ostream &s, void const *p, unsigned bytes)
{
  s.write((char const *)p, bytes);
}

// ------------------------------ traits without tears ------------------------------

//awf: these cannot be static for sunpro 5...

// template <class T> long scalar_precision(T const &);
long vnl_scalar_precision(float  const &) { return vnl_matlab_header::vnl_SINGLE_PRECISION; }
long vnl_scalar_precision(double const &) { return vnl_matlab_header::vnl_DOUBLE_PRECISION; }
long vnl_scalar_precision(vcl_complex<float>  const &) { return vnl_matlab_header::vnl_SINGLE_PRECISION; }
long vnl_scalar_precision(vcl_complex<double> const &) { return vnl_matlab_header::vnl_DOUBLE_PRECISION; }

// template <class T> long is_complex(T const &);
long vnl_is_complex(float  const &) { return 0; }
long vnl_is_complex(double const &) { return 0; }
long vnl_is_complex(vcl_complex<float>  const &) { return 1; }
long vnl_is_complex(vcl_complex<double> const &) { return 1; }

// template <class T> void vnl_write_real(vcl_ostream &, T const *, unsigned );
void vnl_write_real(vcl_ostream &s, float const *data, unsigned n)
{ ::vnl_write_bytes(s, data, n*sizeof(*data)); }

void vnl_write_real(vcl_ostream &s, double const *data, unsigned n)
{ ::vnl_write_bytes(s, data, n*sizeof(*data)); }

void vnl_write_real(vcl_ostream &s, vcl_complex<float> const *data, unsigned n)
{
  float dummy;
  for (unsigned i=0; i<n; ++i) { // real block
    dummy = vcl_real(data[i]);
    ::vnl_write_bytes(s, &dummy, sizeof(dummy));
  }
}

void vnl_write_real(vcl_ostream &s, vcl_complex<double> const *data, unsigned n)
{
  double dummy;
  for (unsigned i=0; i<n; ++i) { // real block
    dummy = vcl_real(data[i]);
    ::vnl_write_bytes(s, &dummy, sizeof(dummy));
  }
}

// template <class T> void vnl_write_imag(vcl_ostream &, T const *, unsigned );

void vnl_write_imag(vcl_ostream &, float const *, unsigned ) { }

void vnl_write_imag(vcl_ostream &, double const *, unsigned ) { }

void vnl_write_imag(vcl_ostream &s, vcl_complex<float> const *data, unsigned n)
{
  float dummy;
  for (unsigned i=0; i<n; ++i) { // imag block
    dummy = vcl_imag(data[i]);
    ::vnl_write_bytes(s, &dummy, sizeof(dummy));
  }
}

void vnl_write_imag(vcl_ostream &s, vcl_complex<double> const *data, unsigned n)
{
  double dummy;
  for (unsigned i=0; i<n; ++i) { // imag block
    dummy = vcl_imag(data[i]);
    ::vnl_write_bytes(s, &dummy, sizeof(dummy));
  }
}

//--------------------------------------------------------------------------------

//: scalars
template <class T>
bool vnl_matlab_write(vcl_ostream &s, T const & x, char const *name)
{
  vnl_matlab_header hdr;
  hdr.type = native_BYTE_ORDER + vnl_matlab_header::vnl_COLUMN_WISE + vnl_scalar_precision(x);
  hdr.rows = 1;
  hdr.cols = 1;
  hdr.imag = vnl_is_complex(x);
  hdr.namlen = (unsigned long)vcl_strlen(name)+1L;

  ::vnl_write_bytes(s, &hdr, sizeof(hdr));
  ::vnl_write_bytes(s, name, hdr.namlen);
  vnl_write_real(s, &x, 1);
  vnl_write_imag(s, &x, 1);

  return s.good() != 0;
}
#define scalar_instantiate(T) \
template bool vnl_matlab_write(vcl_ostream &, T const &, char const *);

//: 1D array
template <class T>
bool vnl_matlab_write(vcl_ostream &s, T const *v, unsigned n, char const *name)
{
  vnl_matlab_header hdr;
  hdr.type = native_BYTE_ORDER + vnl_matlab_header::vnl_COLUMN_WISE + vnl_scalar_precision(v[0]);
  hdr.rows = (long)n;
  hdr.cols = 1L;
  hdr.imag = vnl_is_complex(v[0]);
  hdr.namlen = (unsigned long)vcl_strlen(name)+1L;

  ::vnl_write_bytes(s, &hdr, sizeof(hdr));
  ::vnl_write_bytes(s, name, hdr.namlen);
  vnl_write_real(s, v, n);
  vnl_write_imag(s, v, n);

  return s.good() != 0;
}
#define array1D_instantiate(T) \
template bool vnl_matlab_write(vcl_ostream &, T const *, unsigned, char const *);

//: 2D array
template <class T>
bool vnl_matlab_write(vcl_ostream &s,
                      T const * const *data,
                      unsigned rows, unsigned cols,
                      char const *name)
{
  vnl_matlab_header hdr;
  hdr.type = native_BYTE_ORDER + vnl_matlab_header::vnl_ROW_WISE + vnl_scalar_precision(data[0][0]);
  hdr.rows = (long)rows;
  hdr.cols = (long)cols;
  hdr.imag = vnl_is_complex(data[0][0]);
  hdr.namlen = (unsigned long)vcl_strlen(name)+1L;

  ::vnl_write_bytes(s, &hdr, sizeof(hdr));
  ::vnl_write_bytes(s, name, hdr.namlen);
  for (unsigned i=0; i<rows; ++i)
    vnl_write_real(s, data[i], cols);
  for (unsigned i=0; i<rows; ++i)
    vnl_write_imag(s, data[i], cols);

  return s.good() != 0;
}
#define array2D_instantiate(T) \
template bool vnl_matlab_write(vcl_ostream &, T const * const *, unsigned, unsigned, char const *);

//--------------------------------------------------------------------------------

scalar_instantiate(float);
scalar_instantiate(double);
scalar_instantiate(vcl_complex<float>);
scalar_instantiate(vcl_complex<double>);

array1D_instantiate(float);
array1D_instantiate(double);
array1D_instantiate(vcl_complex<float>);
array1D_instantiate(vcl_complex<double>);

array2D_instantiate(float);
array2D_instantiate(double);
array2D_instantiate(vcl_complex<float>);
array2D_instantiate(vcl_complex<double>);
