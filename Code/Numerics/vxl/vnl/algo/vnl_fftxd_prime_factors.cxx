// helper class:
// given an integer n of the form
// N = (2**P)(3**Q)(5**R)
// split n into its primefactors (2,3,5)
// used for Temperton fft routines
//============================================================

#include "vnl_fftxd_prime_factors.h"
#include <vnl/algo/vnl_netlib.h> // dsetgpfa_(), setgpfa_()

// use C++ overloading to find the correct FORTRAN routine

inline void cxx_setgpfa(double *triggs, const int &size, const int *iPvnl_qr, int *info)
{ // call double version
  dsetgpfa_(triggs,size,iPvnl_qr,info);
}

inline void cxx_setgpfa(float *triggs, const int &size, const int *iPvnl_qr, int *info)
{ // call float version
  setgpfa_(triggs,size,iPvnl_qr,info);
}

//--------------------------------------------------------------------------------

// std constructor: give number 'num', splits it into prime-factors
template<class T>
vnl_fftxd_prime_factors<T>::vnl_fftxd_prime_factors (int num)
  : number(num)
{
  factors = new T[2*num];
  cxx_setgpfa (factors,num,iPvnl_qr,&info); // info == -1 if cannot split into primes
}

template<class T>
vnl_fftxd_prime_factors<T>::~vnl_fftxd_prime_factors () {
  delete [] factors;
}

//--------------------------------------------------------------------------------

template<class T>
const T *vnl_fftxd_prime_factors<T>::getFactors () const {
  return factors;
}

template<class T>
int vnl_fftxd_prime_factors<T>::getNumber () const  {
  return number;
}

template<class T>
const int *vnl_fftxd_prime_factors<T>::getPvnl_qr () const {
  return iPvnl_qr;
}

//--------------------------------------------------------------------------------

template class vnl_fftxd_prime_factors<double>;
template class vnl_fftxd_prime_factors<float>;
