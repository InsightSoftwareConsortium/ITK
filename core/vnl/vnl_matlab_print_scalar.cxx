// This is core/vnl/vnl_matlab_print_scalar.cxx

#include "vnl_matlab_print_scalar.h"

#include <vcl_cstdio.h>  // sprintf()
#include <vcl_cstdlib.h> // abort()
#include <vcl_cstring.h> // strlen()
#include <vcl_complex.h>

void vnl_matlab_print_scalar(int v,
                             char *buf,
                             vnl_matlab_print_format)
{
  vcl_sprintf(buf, "%4d ", v);
}

void vnl_matlab_print_scalar(unsigned v,
                             char *buf,
                             vnl_matlab_print_format)
{
  vcl_sprintf(buf, "%4u ", v);
}

void vnl_matlab_print_scalar(float v,
                             char *buf,
                             vnl_matlab_print_format format)
{
  if (format == vnl_matlab_print_format_default)
    format = vnl_matlab_print_format_top();
  switch (format) {
  case vnl_matlab_print_format_long:
    if (v == 0.0)
      vcl_sprintf(buf, "%8d ", 0);
    else
      vcl_sprintf(buf, "%8.5f ", v);
    break;
  case vnl_matlab_print_format_short:
    if (v == 0.0)
      vcl_sprintf(buf, "%6d ", 0);
    else
      vcl_sprintf(buf, "%6.3f ", v);
    break;
  case vnl_matlab_print_format_long_e:
    vcl_sprintf(buf, "%11.7e ", v);
    break;
  case vnl_matlab_print_format_short_e:
    vcl_sprintf(buf, "%8.4e ", v);
    break;
  default:/*vnl_matlab_print_format_default:*/ vcl_abort(); break;
  }
}

void vnl_matlab_print_scalar(double v,
                             char *buf,
                             vnl_matlab_print_format format)
{
  if (format == vnl_matlab_print_format_default)
    format = vnl_matlab_print_format_top();
  switch (format) {
  case vnl_matlab_print_format_long:
    if (v == 0.0)
      vcl_sprintf(buf, "%16d ", 0);
    else
      vcl_sprintf(buf, "%16.13f ", v);
    break;
  case vnl_matlab_print_format_short:
    if (v == 0.0)
      vcl_sprintf(buf, "%8d ", 0);
    else
      vcl_sprintf(buf, "%8.4f ", v);
    break;
  case vnl_matlab_print_format_long_e:
    vcl_sprintf(buf, "%20.14e ", v);
    break;
  case vnl_matlab_print_format_short_e:
    vcl_sprintf(buf, "%10.4e ", v);
    break;
  default:/*vnl_matlab_print_format_default:*/ vcl_abort(); break;
  }
}

void vnl_matlab_print_scalar(long double v,
                             char *buf,
                             vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(double(v), buf, format); // FIXME
}

void vnl_matlab_print_scalar(vcl_complex<double> v,
                             char *buf,
                             vnl_matlab_print_format format)
{
  if (format == vnl_matlab_print_format_default)
    format = vnl_matlab_print_format_top();
  int width = 16;
  int precision = 12;
  char conv = 'f';

  switch (format) {
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
  default:/*vnl_matlab_print_format_default:*/ vcl_abort(); break;
  }

  switch (format) {
  case vnl_matlab_print_format_long:
  case vnl_matlab_print_format_short:
    conv = 'f';
    break;
  case vnl_matlab_print_format_long_e:
  case vnl_matlab_print_format_short_e:
    conv = 'e';
    break;
  default:/*vnl_matlab_print_format_default:*/ vcl_abort(); break;
  }

  double r = vcl_real(v);
  double i = vcl_imag(v);

  char fmt[1024];
  // Real part
  if (r == 0) {
    vcl_sprintf(fmt, "%%" "%d" "d ", width);
    vcl_sprintf(buf, fmt, 0);

  } else {
    vcl_sprintf(fmt, "%%" "%d" "." "%d" "%c ", width, precision, conv);
    vcl_sprintf(buf, fmt, r);
  }

  buf += vcl_strlen(buf);

  // Imaginary part.  Width is reduced as sign is taken care of separately
  if (i == 0) {
    vcl_sprintf(fmt, " %%" "%d" "s  ", width-1);
    vcl_sprintf(buf, fmt, "");
  } else {
    char sign = '+';
    if (i < 0) {
      sign = '-';
      i = -i;
    }
    vcl_sprintf(fmt, "%c%%" "%d.%d%ci ", sign, width-1, precision, conv);
    vcl_sprintf(buf, fmt, i);
  }
}

void vnl_matlab_print_scalar(vcl_complex<float> v,
                             char *buf,
                             vnl_matlab_print_format format)
{
  if (format == vnl_matlab_print_format_default)
    format = vnl_matlab_print_format_top();
  int width = 10;
  int precision = 6;
  char conv = 'f';

  switch (format) {
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
  default:/*vnl_matlab_print_format_default:*/ vcl_abort(); break;
  }

  switch (format) {
  case vnl_matlab_print_format_long:
  case vnl_matlab_print_format_short:
    conv = 'f';
    break;
  case vnl_matlab_print_format_long_e:
  case vnl_matlab_print_format_short_e:
    conv = 'e';
    break;
  default:/*vnl_matlab_print_format_default:*/ vcl_abort(); break;
  }

  float r = vcl_real(v);
  float i = vcl_imag(v);

  char fmt[1024];
  // Real part
  if (r == 0) {
    vcl_sprintf(fmt, "%%" "%d" "d ", width);
    vcl_sprintf(buf, fmt, 0);

  } else {
    vcl_sprintf(fmt, "%%" "%d" "." "%d" "%c ", width, precision, conv);
    vcl_sprintf(buf, fmt, r);
  }

  buf += vcl_strlen(buf);

  // Imaginary part.  Width is reduced as sign is taken care of separately
  if (i == 0) {
    vcl_sprintf(fmt, " %%" "%d" "s  ", width-1);
    vcl_sprintf(buf, fmt, "");
  } else {
    char sign = '+';
    if (i < 0) {
      sign = '-';
      i = -i;
    }
    vcl_sprintf(fmt, "%c%%" "%d.%d%ci ", sign, width-1, precision, conv);
    vcl_sprintf(buf, fmt, i);
  }
}

void vnl_matlab_print_scalar(vcl_complex<long double> v,
                             char *buf,
                             vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(vcl_complex<double>(vcl_real(v), vcl_imag(v)), buf, format); // FIXME
}


#include <vcl_iostream.h>
template <class T>
vcl_ostream &vnl_matlab_print_scalar(vcl_ostream &s,
                                     T value,
                                     vnl_matlab_print_format format)
{
  char buf[1024];
  vnl_matlab_print_scalar(value, buf, format);
  return s << buf;
}

#define inst(T) template vcl_ostream &vnl_matlab_print_scalar(vcl_ostream &, T, vnl_matlab_print_format)
inst(int);
inst(float);
inst(double);
inst(long double);
inst(vcl_complex<float>);
inst(vcl_complex<double>);
inst(vcl_complex<long double>);

