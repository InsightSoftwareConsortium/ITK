//:
// \file
// \brief definition and instantiations macro for complex<TYPE>
//
// \verbatim
// Modifications
//     200498 AWF Now GCC uses this complex as well.
// \endverbatim


#ifndef vcl_emulation_complex_h
#define vcl_emulation_complex_h
#define VCL_COMPLEX_AVAILABLE

#include <vcl_cmath.h>
#include <vcl_iostream.h>

// The Sunpro 4.2 compiler has a bug that it will not instantiate
// inline friend functions with global linkage, so we remove
// all the inline's while instantiation is taking place.

#if defined(VCL_SUNPRO_CC) && defined(INSTANTIATE_TEMPLATES)
# define inline
#endif

// implementation of class vcl_complex<FLOAT>, copied from g++ 2.7.2 - PVR

#if defined (VCL_VC50)
template <class FLOAT> class vcl_complex;
#endif

template <class FLOAT>
class vcl_complex
{
 public:
  typedef FLOAT value_type;
  vcl_complex (FLOAT r = 0, FLOAT i = 0): re (r), im (i) { }
#if VCL_HAS_MEMBER_TEMPLATES
  template <class T>
  vcl_complex (vcl_complex<T> const& that): re(that.real()), im(that.imag()) {}
#else
  vcl_complex (vcl_complex<float> const& that): re(FLOAT(that.real())), im(FLOAT(that.imag())) {}
  vcl_complex (vcl_complex<double>const& that): re(FLOAT(that.real())), im(FLOAT(that.imag())) {}
#endif

  vcl_complex& operator += (const vcl_complex&);
  vcl_complex& operator -= (const vcl_complex&);
  vcl_complex& operator *= (const vcl_complex&);
  vcl_complex& operator /= (const vcl_complex&);
  FLOAT real () const { return re; }
  FLOAT imag () const { return im; }
 private:
  FLOAT re, im;
};


template <class FLOAT>
inline vcl_complex<FLOAT>&
vcl_complex<FLOAT>::operator += (const vcl_complex<FLOAT>& r)
{
  re += r.real();
  im += r.imag();
  return *this;
}

template <class FLOAT>
inline vcl_complex<FLOAT>&
vcl_complex<FLOAT>::operator -= (const vcl_complex<FLOAT>& r)
{
  re -= r.real();
  im -= r.imag();
  return *this;
}

template <class FLOAT>
inline vcl_complex<FLOAT>&
vcl_complex<FLOAT>::operator *= (const vcl_complex<FLOAT>& r)
{
  FLOAT f = re * r.real() - im * r.imag();
  im = re * r.imag() + im * r.real();
  re = f;
  return *this;
}

template <class FLOAT>
inline vcl_complex<FLOAT>&
vcl_complex<FLOAT>::operator /= (const vcl_complex<FLOAT>& y)
{
  FLOAT ar = (FLOAT) vcl_abs (y.real());
  FLOAT ai = (FLOAT) vcl_abs (y.imag());
  FLOAT nr, ni;
  FLOAT t, d;
  if (ar <= ai)
    {
      t = y.real() / y.imag();
      d = y.imag() * (t*t + 1L);
      nr = (re * t + im) / d;
      ni = (im * t - re) / d;
    }
  else
    {
      t = y.imag() / y.real();
      d = y.real() * (t*t + 1L);
      nr = (re + im * t) / d;
      ni = (im - re * t) / d;
    }
  re = nr;
  im = ni;
  return *this;
}

template <class FLOAT> inline FLOAT
vcl_real (vcl_complex<FLOAT> const& x) { return x.real(); }

template <class FLOAT> inline FLOAT
vcl_imag (vcl_complex<FLOAT> const& x) { return x.imag(); }

template <class FLOAT> inline vcl_complex<FLOAT>
operator + (const vcl_complex<FLOAT>& x, const vcl_complex<FLOAT>& y)
{
  return vcl_complex<FLOAT> (x.real() + y.real(), x.imag() + y.imag());
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator + (const vcl_complex<FLOAT>& x, FLOAT y)
{
  return vcl_complex<FLOAT> (x.real() + y, x.imag());
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator + (FLOAT x, const vcl_complex<FLOAT>& y)
{
  return vcl_complex<FLOAT> (x + y.real(), y.imag());
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator - (const vcl_complex<FLOAT>& x, const vcl_complex<FLOAT>& y)
{
  return vcl_complex<FLOAT> (x.real() - y.real(), x.imag() - y.imag());
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator - (const vcl_complex<FLOAT>& x, FLOAT y)
{
  return vcl_complex<FLOAT> (x.real() - y, x.imag());
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator - (FLOAT x, const vcl_complex<FLOAT>& y)
{
  return vcl_complex<FLOAT> (x - y.real(), - y.imag());
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator * (const vcl_complex<FLOAT>& x, const vcl_complex<FLOAT>& y)
{
  return vcl_complex<FLOAT> (x.real() * y.real() - x.imag() * y.imag(),
                             x.real() * y.imag() + x.imag() * y.real());
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator * (const vcl_complex<FLOAT>& x, FLOAT y)
{
  return vcl_complex<FLOAT> (x.real() * y, x.imag() * y);
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator * (FLOAT x, const vcl_complex<FLOAT>& y)
{
  return vcl_complex<FLOAT> (x * y.real(), x * y.imag());
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator / (const vcl_complex<FLOAT>& x, FLOAT y)
{
  return vcl_complex<FLOAT> (x.real() / y, x.imag() / y);
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator + (const vcl_complex<FLOAT>& x)
{
  return x;
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator - (const vcl_complex<FLOAT>& x)
{
  return vcl_complex<FLOAT> (-x.real(), -x.imag());
}

template <class FLOAT> inline bool
operator == (const vcl_complex<FLOAT>& x, const vcl_complex<FLOAT>& y)
{
  return x.real() == y.real() && x.imag() == y.imag();
}

template <class FLOAT> inline bool
operator == (const vcl_complex<FLOAT>& x, FLOAT y)
{
  return x.real() == y && x.imag() == 0;
}

template <class FLOAT> inline bool
operator == (FLOAT x, const vcl_complex<FLOAT>& y)
{
  return x == y.real() && y.imag() == 0;
}

template <class FLOAT> inline bool
operator != (const vcl_complex<FLOAT>& x, const vcl_complex<FLOAT>& y)
{
  return x.real() != y.real() || x.imag() != y.imag();
}

template <class FLOAT> inline bool
operator != (const vcl_complex<FLOAT>& x, FLOAT y)
{
  return x.real() != y || x.imag() != 0;
}

template <class FLOAT> inline bool
operator != (FLOAT x, const vcl_complex<FLOAT>& y)
{
  return x != y.real() || y.imag() != 0;
}

template <class FLOAT> inline FLOAT
vcl_abs (const vcl_complex<FLOAT>& x)
{
  return (FLOAT) hypot (x.real(), x.imag());
}

template <class FLOAT> inline FLOAT
vcl_arg (const vcl_complex<FLOAT>& x)
{
  return (FLOAT) atan2 (x.imag(), x.real());
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_polar (FLOAT r, FLOAT t)
{
  return vcl_complex<FLOAT> (r * cos (t), r * sin (t));
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_conj (const vcl_complex<FLOAT>& x)
{
  return vcl_complex<FLOAT> (x.real(), -x.imag());
}

template <class FLOAT> inline FLOAT
vcl_norm (const vcl_complex<FLOAT>& x)
{
  return x.real() * x.real() + x.imag() * x.imag();
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_cos (const vcl_complex<FLOAT>& x)
{
  return vcl_complex<FLOAT> (cos (x.real()) * cosh (x.imag()),
                           - sin (x.real()) * sinh (x.imag()));
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_cosh (const vcl_complex<FLOAT>& x)
{
  return vcl_complex<FLOAT> (cosh (x.real()) * cos (x.imag()),
                           sinh (x.real()) * sin (x.imag()));
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_exp (const vcl_complex<FLOAT>& x)
{
  return vcl_polar (FLOAT (exp (x.real())), x.imag());
}


template <class FLOAT> inline vcl_complex<FLOAT>
vcl_log (const vcl_complex<FLOAT>& x)
{
  return vcl_complex<FLOAT> (log (abs (x)), (FLOAT) atan2 (x.imag(), x.real()));
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_pow (const vcl_complex<FLOAT>& x, const vcl_complex<FLOAT>& y)
{
  FLOAT logr = log (abs (x));
  FLOAT t = (FLOAT) atan2 (x.imag(), x.real()); // was arg, but sunCC messed up WAH

  return vcl_polar (FLOAT (exp (logr * y.real() - y.imag() * t)),
                    FLOAT (y.imag() * logr + y.real() * t));
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_pow (const vcl_complex<FLOAT>& x, FLOAT y)
{
  return exp (FLOAT (y) * log (x));
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_pow (FLOAT x, const vcl_complex<FLOAT>& y)
{
  return exp (y * FLOAT (log (x)));
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_sin (const vcl_complex<FLOAT>& x)
{
  return vcl_complex<FLOAT> (sin (x.real()) * cosh (x.imag()),
                           cos (x.real()) * sinh (x.imag()));
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_sinh (const vcl_complex<FLOAT>& x)
{
  return vcl_complex<FLOAT> (sinh (x.real()) * cos (x.imag()),
                           cosh (x.real()) * sin (x.imag()));
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator / (const vcl_complex<FLOAT>& x, const vcl_complex<FLOAT>& y)
{
  FLOAT ar = vcl_abs (y.real());
  FLOAT ai = vcl_abs (y.imag());
  FLOAT nr, ni;
  FLOAT t, d;
  if (ar <= ai)
    {
      t = y.real() / y.imag();
      d = y.imag() * (t*t + 1L);
      nr = (x.real() * t + x.imag()) / d;
      ni = (x.imag() * t - x.real()) / d;
    }
  else
    {
      t = y.imag() / y.real();
      d = y.real() * (t*t + 1L);
      nr = (x.real() + x.imag() * t) / d;
      ni = (x.imag() - x.real() * t) / d;
    }
  return vcl_complex<FLOAT> (nr, ni);
}

template <class FLOAT> inline vcl_complex<FLOAT>
operator / (FLOAT x, const vcl_complex<FLOAT>& y)
{
  FLOAT ar = vcl_abs (y.real());
  FLOAT ai = vcl_abs (y.imag());
  FLOAT nr, ni;
  FLOAT t, d;
  if (ar <= ai)
    {
      t = y.real() / y.imag();
      d = y.imag() * (1 + t*t);
      nr = x * t / d;
      ni = -x / d;
    }
  else
    {
      t = y.imag() / y.real();
      d = y.real() * (1 + t*t);
      nr = x / d;
      ni = -x * t / d;
    }
  return vcl_complex<FLOAT> (nr, ni);
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_pow (const vcl_complex<FLOAT>& xin, int y)
{
  vcl_complex<FLOAT> r = 1.0;
  vcl_complex<FLOAT> x = xin;
  if (y < 0)
    {
      y = -y;
      x = ((FLOAT)1)/x;
    }
  while (y)
    {
      if (y & 1)   r *= x;
      if (y >>= 1) x *= x;
    }
  return r;
}

template <class FLOAT> inline vcl_complex<FLOAT>
vcl_sqrt (const vcl_complex<FLOAT>& x)
{
  FLOAT r = vcl_abs (x);
  FLOAT nr, ni;
  if (r == 0.0)
    nr = ni = r;
  else if (x.real() > 0)
    {
      nr = sqrt (0.5 * (r + x.real()));
      ni = x.imag() / nr / 2;
    }
  else
    {
      ni = sqrt (0.5 * (r - x.real()));
      if (x.imag() < 0)
        ni = - ni;
      nr = x.imag() / ni / 2;
    }
  return vcl_complex<FLOAT> (nr, ni);
}

template <class FLOAT>
inline
vcl_ostream& operator << (vcl_ostream& o, vcl_complex<FLOAT> const& x)
{
  o << x.real();
  if (x.imag()) {
    if (x.imag() > 0)
      o << '+';
    o << x.imag() << 'i';
  }
  return o;
}

template <class FLOAT>
inline
vcl_istream& operator >> (vcl_istream& o, vcl_complex<FLOAT>& x)
{
  FLOAT r, i;
  o >> r >> i;
  x = vcl_complex<FLOAT>(r,i);
  return o;
}

#ifdef VCL_SUNPRO_CC
#ifdef INSTANTIATE_TEMPLATES
#undef inline
#endif
#endif
// ANSI complex types
#define __STD_COMPLEX

#endif // vcl_emulation_complex_h
