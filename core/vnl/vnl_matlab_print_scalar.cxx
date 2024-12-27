// This is core/vnl/vnl_matlab_print_scalar.cxx

#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <complex>

#include "vnl_matlab_print_scalar.h"
#include "vcl_compiler.h"


void
vnl_matlab_print_scalar(int v, char * buf, size_t buf_len, vnl_matlab_print_format)
{
  std::snprintf(buf, buf_len, "%4d ", v);
}

#if !VXL_LEGACY_FUTURE_REMOVE
void
vnl_matlab_print_scalar(int v, char * buf, vnl_matlab_print_format)
{
  vnl_matlab_print_scalar(v, buf, SIZE_MAX);
}
#endif

void
vnl_matlab_print_scalar(unsigned v, char * buf, size_t buf_len, vnl_matlab_print_format)
{
  std::snprintf(buf, buf_len, "%4u ", v);
}

#if !VXL_LEGACY_FUTURE_REMOVE
void
vnl_matlab_print_scalar(unsigned v, char * buf, vnl_matlab_print_format)
{
  vnl_matlab_print_scalar(v, buf, SIZE_MAX);
}
#endif

void
vnl_matlab_print_scalar(float v, char * buf, size_t buf_len, vnl_matlab_print_format format)
{
  if (format == vnl_matlab_print_format_default)
    format = vnl_matlab_print_format_top();
  switch (format)
  {
    case vnl_matlab_print_format_long:
      if (v == 0.0)
        std::snprintf(buf, buf_len, "%8d ", 0);
      else
        std::snprintf(buf, buf_len, "%8.5f ", v);
      break;
    case vnl_matlab_print_format_short:
      if (v == 0.0)
        std::snprintf(buf, buf_len, "%6d ", 0);
      else
        std::snprintf(buf, buf_len, "%6.3f ", v);
      break;
    case vnl_matlab_print_format_long_e:
      std::snprintf(buf, buf_len, "%11.7e ", v);
      break;
    case vnl_matlab_print_format_short_e:
      std::snprintf(buf, buf_len, "%8.4e ", v);
      break;
    default: /*vnl_matlab_print_format_default:*/
      std::abort();
  }
}

#if !VXL_LEGACY_FUTURE_REMOVE
void
vnl_matlab_print_scalar(float v, char * buf, vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(v, buf, SIZE_MAX, format);
}
#endif

void
vnl_matlab_print_scalar(double v, char * buf, size_t buf_len, vnl_matlab_print_format format)
{
  if (format == vnl_matlab_print_format_default)
    format = vnl_matlab_print_format_top();
  switch (format)
  {
    case vnl_matlab_print_format_long:
      if (v == 0.0)
        std::snprintf(buf, buf_len, "%16d ", 0);
      else
        std::snprintf(buf, buf_len, "%16.13f ", v);
      break;
    case vnl_matlab_print_format_short:
      if (v == 0.0)
        std::snprintf(buf, buf_len, "%8d ", 0);
      else
        std::snprintf(buf, buf_len, "%8.4f ", v);
      break;
    case vnl_matlab_print_format_long_e:
      std::snprintf(buf, buf_len, "%20.14e ", v);
      break;
    case vnl_matlab_print_format_short_e:
      std::snprintf(buf, buf_len, "%10.4e ", v);
      break;
    default: /*vnl_matlab_print_format_default:*/
      std::abort();
  }
}

#if !VXL_LEGACY_FUTURE_REMOVE
void
vnl_matlab_print_scalar(double v, char * buf, vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(v, buf, SIZE_MAX, format);
}
#endif

void
vnl_matlab_print_scalar(long double v, char * buf, size_t buf_len, vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(double(v), buf, buf_len, format); // FIXME
}

#if !VXL_LEGACY_FUTURE_REMOVE
void
vnl_matlab_print_scalar(long double v, char * buf, vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(v, buf, SIZE_MAX, format);
}
#endif

void
vnl_matlab_print_scalar(std::complex<double> v, char * buf, size_t buf_len, vnl_matlab_print_format format)
{
  if (format == vnl_matlab_print_format_default)
    format = vnl_matlab_print_format_top();
  int width = 16;
  int precision = 12;
  char conv = 'f';

  switch (format)
  {
    case vnl_matlab_print_format_long:
    case vnl_matlab_print_format_long_e:
      width = 16;
      precision = 12;
      break;
    case vnl_matlab_print_format_short:
    case vnl_matlab_print_format_short_e:
      width = 8;
      precision = 4;
      break;
    default: /*vnl_matlab_print_format_default:*/
      std::abort();
  }

  switch (format)
  {
    case vnl_matlab_print_format_long:
    case vnl_matlab_print_format_short:
      conv = 'f';
      break;
    case vnl_matlab_print_format_long_e:
    case vnl_matlab_print_format_short_e:
      conv = 'e';
      break;
    default: /*vnl_matlab_print_format_default:*/
      std::abort();
  }

  const double r = std::real(v);
  double i = std::imag(v);

  char fmt[1024];
  // Real part
  if (r == 0)
  {
    std::snprintf(fmt,
                  sizeof(fmt),
                  "%%"
                  "%d"
                  "d ",
                  width);
    std::snprintf(buf, buf_len, fmt, 0);
  }
  else
  {
    std::snprintf(fmt,
                  sizeof(fmt),
                  "%%"
                  "%d"
                  "."
                  "%d"
                  "%c ",
                  width,
                  precision,
                  conv);
    std::snprintf(buf, buf_len, fmt, r);
  }

  buf += std::strlen(buf);

  // Imaginary part.  Width is reduced as sign is taken care of separately
  if (i == 0)
  {
    std::snprintf(fmt,
                  sizeof(fmt),
                  " %%"
                  "%d"
                  "s  ",
                  width - 1);
    std::snprintf(buf, buf_len, fmt, "");
  }
  else
  {
    char sign = '+';
    if (i < 0)
    {
      sign = '-';
      i = -i;
    }
    std::snprintf(fmt,
                  sizeof(fmt),
                  "%c%%"
                  "%d.%d%ci ",
                  sign,
                  width - 1,
                  precision,
                  conv);
    std::snprintf(buf, buf_len, fmt, i);
  }
}

#if !VXL_LEGACY_FUTURE_REMOVE
void
vnl_matlab_print_scalar(std::complex<double> v, char * buf, vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(v, buf, SIZE_MAX, format);
}
#endif

void
vnl_matlab_print_scalar(std::complex<float> v, char * buf, size_t buf_len, vnl_matlab_print_format format)
{
  if (format == vnl_matlab_print_format_default)
    format = vnl_matlab_print_format_top();
  int width = 10;
  int precision = 6;
  char conv = 'f';

  switch (format)
  {
    case vnl_matlab_print_format_long:
    case vnl_matlab_print_format_long_e:
      width = 10;
      precision = 6;
      break;
    case vnl_matlab_print_format_short:
    case vnl_matlab_print_format_short_e:
      width = 8;
      precision = 4;
      break;
    default: /*vnl_matlab_print_format_default:*/
      std::abort();
  }

  switch (format)
  {
    case vnl_matlab_print_format_long:
    case vnl_matlab_print_format_short:
      conv = 'f';
      break;
    case vnl_matlab_print_format_long_e:
    case vnl_matlab_print_format_short_e:
      conv = 'e';
      break;
    default: /*vnl_matlab_print_format_default:*/
      std::abort();
  }

  const float r = std::real(v);
  float i = std::imag(v);

  char fmt[1024];
  // Real part
  if (r == 0)
  {
    std::snprintf(fmt,
                  sizeof(fmt),
                  "%%"
                  "%d"
                  "d ",
                  width);
    std::snprintf(buf, buf_len, fmt, 0);
  }
  else
  {
    std::snprintf(fmt,
                  sizeof(fmt),
                  "%%"
                  "%d"
                  "."
                  "%d"
                  "%c ",
                  width,
                  precision,
                  conv);
    std::snprintf(buf, buf_len, fmt, r);
  }

  buf += std::strlen(buf);

  // Imaginary part.  Width is reduced as sign is taken care of separately
  if (i == 0)
  {
    std::snprintf(fmt,
                  sizeof(fmt),
                  " %%"
                  "%d"
                  "s  ",
                  width - 1);
    std::snprintf(buf, buf_len, fmt, "");
  }
  else
  {
    char sign = '+';
    if (i < 0)
    {
      sign = '-';
      i = -i;
    }
    std::snprintf(fmt,
                  sizeof(fmt),
                  "%c%%"
                  "%d.%d%ci ",
                  sign,
                  width - 1,
                  precision,
                  conv);
    std::snprintf(buf, buf_len, fmt, i);
  }
}

#if !VXL_LEGACY_FUTURE_REMOVE
void
vnl_matlab_print_scalar(std::complex<float> v, char * buf, vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(v, buf, SIZE_MAX, format);
}
#endif

void
vnl_matlab_print_scalar(std::complex<long double> v, char * buf, size_t buf_len, vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(std::complex<double>(std::real(v), std::imag(v)), buf, buf_len, format); // FIXME
}

#if !VXL_LEGACY_FUTURE_REMOVE
void
vnl_matlab_print_scalar(std::complex<long double> v, char * buf, vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(v, buf, SIZE_MAX, format);
}
#endif

template <class T>
std::ostream &
vnl_matlab_print_scalar(std::ostream & s, T value, vnl_matlab_print_format format)
{
  char buf[1024];
  vnl_matlab_print_scalar(value, buf, sizeof(buf), format);
  return s << buf;
}

#define inst(T) template std::ostream & vnl_matlab_print_scalar(std::ostream &, T, vnl_matlab_print_format)
inst(int);
inst(float);
inst(double);
inst(long double);
inst(std::complex<float>);
inst(std::complex<double>);
inst(std::complex<long double>);
