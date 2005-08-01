// This is core/vnl/algo/vnl_fft_prime_factors.h
#ifndef vnl_fft_prime_factors_h_
#define vnl_fft_prime_factors_h_
//:
// \file
// \brief Holds prime factor information
// \author Veit U.B. Schenk, Oxford RRG
// \date   19 Mar 98
//
// \verbatim
//  Modifications
//   10/4/2001 Ian Scott (Manchester) Converted perceps header to doxygen
// \endverbatim

#include <vcl_compiler.h> // for "export" keyword

//: Holds prime factor information
// Helper class used by the vnl_fft_xd<> FFT routines
//
// Given an integer N of the form
//   $N = 2^P 3^Q 5^R$
// split N into its primefactors (2, 3, 5)

export template <class T>
struct vnl_fft_prime_factors
{
private:
  VCL_SAFE_BOOL_DEFINE;
public:
  vnl_fft_prime_factors();

  //: constructor takes the size of the signal.
  vnl_fft_prime_factors(int N) { construct(N); }

  ~vnl_fft_prime_factors () { destruct(); }

  //: array of twiddle factors.
  T const *trigs () const { return trigs_; }

  //: number which was factorized
  int number () const { return number_; }

  //: exponents P, Q, R.
  int const *pqr () const { return pqr_; }

  operator safe_bool () const
    { return (trigs_ && info_ >= 0)? VCL_SAFE_BOOL_TRUE : 0; }
  bool operator!() const
    { return (trigs_ && info_ >= 0)? false : true; }

  void resize(int N) {
    destruct();
    construct(N);
  }

 private:
  T *trigs_;
  int number_;   // the number that is being split into prime-facs
  int pqr_[3];   // store P, Q and R
  int info_;

  void construct(int N);
  void destruct();

  // disallow copying
  vnl_fft_prime_factors (vnl_fft_prime_factors<T> const &) { }
  void operator= (vnl_fft_prime_factors<T> const &) { }
};

#endif // vnl_fft_prime_factors_h_
