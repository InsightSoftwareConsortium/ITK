#ifndef vcl_win32_complex_h_
#define vcl_win32_complex_h_

// fsm@robots: complex<T> is derived from _Complex_base<T>, which is not
// declared with __declspec(dllimport). So complex<T> shouldn't be either
// (else the compiler will emit an error). Whether or not it is depends on
// the value of _CRTIMP being set, e.g. in <math.h>
# include <vcl_iostream.h>
# include <vcl_cmath.h>
# pragma warning (push)
# pragma warning (disable: 4273)
# undef _CRTIMP
# define _CRTIMP
# include <ctype.h>
# include <complex>
# pragma warning (pop)

#ifndef vcl_complex
# define vcl_complex std :: complex
#endif
//using std :: complex; // better to #define vcl_complex std::complex?
//using std :: sqrt;
//using std :: arg;
//using std :: polar;
//using std :: conj;
//using std :: real;
//using std :: imag;

template <class T> inline
T norm(vcl_complex<T> const &z) 
{ return z.real()*z.real() + z.imag()*z.imag(); }
template <class T> inline
T abs(vcl_complex<T> const &z)
{ return sqrt(norm(z)); }

template <class T> 
inline  vcl_complex<T> sqrt(vcl_complex<T> const &x)
{
  T r = abs (x);
  T nr, ni;
  if (r == 0.0)
    nr = ni = r;
  else if (x.real () > 0)
    {
      nr = vcl_sqrt (0.5 * (r + x.real ()));
      ni = x.imag() / nr / 2;
    }
  else
    {
      ni = vcl_sqrt (0.5 * (r - x.real()));
      if (x.imag() < 0)
        ni = - ni;
      nr = x.imag() / ni / 2;
    }
  return vcl_complex<T> (nr, ni);
}

// now for the stream operators :
// these were needed with old iostreams, they cause ambiguity now
// template <class T> inline 
// vcl_ostream &operator<<(vcl_ostream &os, vcl_complex<T> const &z)
// { return os << '(' << z.real() << ',' << ')'; }
//template <class T> inline 
//vcl_istream &operator>>(vcl_istream &is, vcl_complex<T> &z) 
//{ abort(); T r(0), i(0); is >> r >> i; z = vcl_complex<T>(r,i); return is; } // FIXME

# ifndef vcl_abs
#  define vcl_abs  std::abs
# endif
# ifndef vcl_sqrt
#  define vcl_sqrt ::sqrt
# endif
# define vcl_arg   std::arg
# define vcl_conj  std::conj
# define vcl_norm  std::norm
# define vcl_polar std::polar

#endif // vcl_win32_complex_h_
