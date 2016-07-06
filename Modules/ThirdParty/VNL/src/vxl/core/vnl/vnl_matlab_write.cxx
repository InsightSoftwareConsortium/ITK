// This is core/vnl/vnl_matlab_write.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm

#include <iostream>
#include <cstring>
#include <complex>
#include "vnl_matlab_write.h"

#include <vcl_compiler.h>
#include <vnl/vnl_matlab_header.h>

#include <vxl_config.h>
#if VXL_LITTLE_ENDIAN // #ifdef i386
# define native_BYTE_ORDER vnl_matlab_header::vnl_LITTLE_ENDIAN
#else
# define native_BYTE_ORDER vnl_matlab_header::vnl_BIG_ENDIAN
#endif

static void vnl_write_bytes(std::ostream &s, void const *p, unsigned bytes)
{
  s.write((char const *)p, bytes);
}

// ------------------------------ traits without tears ------------------------------

// template <class T> long scalar_precision(T const &);
static long vnl_scalar_precision(float  const &) { return vnl_matlab_header::vnl_SINGLE_PRECISION; }
static long vnl_scalar_precision(double const &) { return vnl_matlab_header::vnl_DOUBLE_PRECISION; }
static long vnl_scalar_precision(std::complex<float>  const &) { return vnl_matlab_header::vnl_SINGLE_PRECISION; }
static long vnl_scalar_precision(std::complex<double> const &) { return vnl_matlab_header::vnl_DOUBLE_PRECISION; }

// template <class T> long is_complex(T const &);
static long vnl_is_complex(float  const &) { return 0; }
static long vnl_is_complex(double const &) { return 0; }
static long vnl_is_complex(std::complex<float>  const &) { return 1; }
static long vnl_is_complex(std::complex<double> const &) { return 1; }

// template <class T> void vnl_write_real(std::ostream &, T const *, unsigned );
static void vnl_write_real(std::ostream &s, float const *data, unsigned n)
{ ::vnl_write_bytes(s, data, n*sizeof(*data)); }

static void vnl_write_real(std::ostream &s, double const *data, unsigned n)
{ ::vnl_write_bytes(s, data, n*sizeof(*data)); }

static void vnl_write_real(std::ostream &s, std::complex<float> const *data, unsigned n)
{
  float dummy;
  for (unsigned i=0; i<n; ++i) { // real block
    dummy = std::real(data[i]);
    ::vnl_write_bytes(s, &dummy, sizeof(dummy));
  }
}

static void vnl_write_real(std::ostream &s, std::complex<double> const *data, unsigned n)
{
  double dummy;
  for (unsigned i=0; i<n; ++i) { // real block
    dummy = std::real(data[i]);
    ::vnl_write_bytes(s, &dummy, sizeof(dummy));
  }
}

// template <class T> void vnl_write_imag(std::ostream &, T const *, unsigned );

static void vnl_write_imag(std::ostream &, float const *, unsigned ) { }

static void vnl_write_imag(std::ostream &, double const *, unsigned ) { }

static void vnl_write_imag(std::ostream &s, std::complex<float> const *data, unsigned n)
{
  float dummy;
  for (unsigned i=0; i<n; ++i) { // imag block
    dummy = std::imag(data[i]);
    ::vnl_write_bytes(s, &dummy, sizeof(dummy));
  }
}

static void vnl_write_imag(std::ostream &s, std::complex<double> const *data, unsigned n)
{
  double dummy;
  for (unsigned i=0; i<n; ++i) { // imag block
    dummy = std::imag(data[i]);
    ::vnl_write_bytes(s, &dummy, sizeof(dummy));
  }
}

//--------------------------------------------------------------------------------

//: scalars
template <class T>
bool vnl_matlab_write(std::ostream &s, T const & x, char const *name)
{
  vnl_matlab_header hdr;
  hdr.type = native_BYTE_ORDER + vnl_matlab_header::vnl_COLUMN_WISE + vnl_scalar_precision(x);
  hdr.rows = 1;
  hdr.cols = 1;
  hdr.imag = vnl_is_complex(x);
  hdr.namlen = (unsigned long)std::strlen(name)+1L;

  ::vnl_write_bytes(s, &hdr, sizeof(hdr));
  ::vnl_write_bytes(s, name, hdr.namlen);
  vnl_write_real(s, &x, 1);
  vnl_write_imag(s, &x, 1);

  return s.good() != 0;
}
#define scalar_instantiate(T) \
template VNL_EXPORT bool vnl_matlab_write(std::ostream &, T const &, char const *);

//: 1D array
template <class T>
bool vnl_matlab_write(std::ostream &s, T const *v, unsigned n, char const *name)
{
  vnl_matlab_header hdr;
  hdr.type = native_BYTE_ORDER + vnl_matlab_header::vnl_COLUMN_WISE + vnl_scalar_precision(v[0]);
  hdr.rows = (long)n;
  hdr.cols = 1L;
  hdr.imag = vnl_is_complex(v[0]);
  hdr.namlen = (unsigned long)std::strlen(name)+1L;

  ::vnl_write_bytes(s, &hdr, sizeof(hdr));
  ::vnl_write_bytes(s, name, hdr.namlen);
  vnl_write_real(s, v, n);
  vnl_write_imag(s, v, n);

  return s.good() != 0;
}
#define array1D_instantiate(T) \
template VNL_EXPORT bool vnl_matlab_write(std::ostream &, T const *, unsigned, char const *);

//: 2D array
template <class T>
bool vnl_matlab_write(std::ostream &s,
                      T const * const *data,
                      unsigned rows, unsigned cols,
                      char const *name)
{
  vnl_matlab_header hdr;
  hdr.type = native_BYTE_ORDER + vnl_matlab_header::vnl_ROW_WISE + vnl_scalar_precision(data[0][0]);
  hdr.rows = (long)rows;
  hdr.cols = (long)cols;
  hdr.imag = vnl_is_complex(data[0][0]);
  hdr.namlen = (unsigned long)std::strlen(name)+1L;

  ::vnl_write_bytes(s, &hdr, sizeof(hdr));
  ::vnl_write_bytes(s, name, hdr.namlen);
  for (unsigned i=0; i<rows; ++i)
    vnl_write_real(s, data[i], cols);
  for (unsigned i=0; i<rows; ++i)
    vnl_write_imag(s, data[i], cols);

  return s.good() != 0;
}
#define array2D_instantiate(T) \
template VNL_EXPORT bool vnl_matlab_write(std::ostream &, T const * const *, unsigned, unsigned, char const *);

//--------------------------------------------------------------------------------

scalar_instantiate(float);
scalar_instantiate(double);
scalar_instantiate(std::complex<float>);
scalar_instantiate(std::complex<double>);

array1D_instantiate(float);
array1D_instantiate(double);
array1D_instantiate(std::complex<float>);
array1D_instantiate(std::complex<double>);

array2D_instantiate(float);
array2D_instantiate(double);
array2D_instantiate(std::complex<float>);
array2D_instantiate(std::complex<double>);
