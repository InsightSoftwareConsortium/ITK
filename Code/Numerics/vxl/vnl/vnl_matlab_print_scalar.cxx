// This is vxl/vnl/vnl_matlab_print_scalar.cxx

#include "vnl_matlab_print_scalar.h"

#include <vcl_cstdio.h>  // sprintf()
#include <vcl_cstdlib.h> // abort()
#include <vcl_cstring.h> // strlen()
#include <vcl_cassert.h>
#include <vcl_complex.h>


// moved here because 2.7 choked

void vnl_matlab_print_scalar(int v,
                             char *buf,
                             vnl_matlab_print_format)
{
  sprintf(buf, "%4d ", v);
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
      sprintf(buf, "%8d ", 0);
    else
      sprintf(buf, "%8.5f ", v);
    break;
  case vnl_matlab_print_format_short:
    if (v == 0.0)
      sprintf(buf, "%6d ", 0);
    else
      sprintf(buf, "%6.3f ", v);
    break;
  case vnl_matlab_print_format_long_e:
    sprintf(buf, "%11.7e ", v);
    break;
  case vnl_matlab_print_format_short_e:
    sprintf(buf, "%8.4e ", v);
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
      sprintf(buf, "%16d ", 0);
    else
      sprintf(buf, "%16.13f ", v);
    break;
  case vnl_matlab_print_format_short:
    if (v == 0.0)
      sprintf(buf, "%8d ", 0);
    else
      sprintf(buf, "%8.4f ", v);
    break;
  case vnl_matlab_print_format_long_e:
    sprintf(buf, "%20.14e ", v);
    break;
  case vnl_matlab_print_format_short_e:
    sprintf(buf, "%10.4e ", v);
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

  double r = v.real();
  double i = v.imag();

  char fmt[1024];
  // Real part
  if (r == 0) {
    sprintf(fmt, "%%" "%d" "d ", width);
    sprintf(buf, fmt, 0);

  } else {
    sprintf(fmt, "%%" "%d" "." "%d" "%c ", width, precision, conv);
    sprintf(buf, fmt, r);
  }

  buf += strlen(buf);

  // Imaginary part.  Width is reduced as sign is taken care of separately
  if (i == 0) {
    sprintf(fmt, " %%" "%d" "s  ", width-1);
    sprintf(buf, fmt, "");
  } else {
    char sign = '+';
    if (i < 0) {
      sign = '-';
      i = -i;
    }
    sprintf(fmt, "%c%%" "%d.%d%ci ", sign, width-1, precision, conv);
    sprintf(buf, fmt, i);
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

  float r = v.real();
  float i = v.imag();

  char fmt[1024];
  // Real part
  if (r == 0) {
    sprintf(fmt, "%%" "%d" "d ", width);
    sprintf(buf, fmt, 0);

  } else {
    sprintf(fmt, "%%" "%d" "." "%d" "%c ", width, precision, conv);
    sprintf(buf, fmt, r);
  }

  buf += strlen(buf);

  // Imaginary part.  Width is reduced as sign is taken care of separately
  if (i == 0) {
    sprintf(fmt, " %%" "%d" "s  ", width-1);
    sprintf(buf, fmt, "");
  } else {
    char sign = '+';
    if (i < 0) {
      sign = '-';
      i = -i;
    }
    sprintf(fmt, "%c%%" "%d.%d%ci ", sign, width-1, precision, conv);
    sprintf(buf, fmt, i);
  }
}

void vnl_matlab_print_scalar(vcl_complex<long double> v,
                             char *buf,
                             vnl_matlab_print_format format)
{
  vnl_matlab_print_scalar(vcl_complex<double>(v.real(), v.imag()), buf, format); // FIXME
}
