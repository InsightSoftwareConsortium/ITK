#ifndef vnl_fftxd_prime_factors_h_
#define vnl_fftxd_prime_factors_h_

//:
//  \file
//  \brief Helper class used by FFTxD
//  \author ?
//
//  Modifications
//  dac (Manchester) 28/03/2001: tidied up documentation
//

template<class T>

//: Helper class used by FFTxD: 
//  given an integer n of the form
//  N = (2**P)(3**Q)(5**R)
//  split n into its primefactors (2,3,5)
//
//  possible improvements: elements 'number' and 'iPvnl_qr' should
//  really be of type 'unsigned int', but since the f2c'd
//  fortran code does not know about these things....

class vnl_fftxd_prime_factors {
public:
  vnl_fftxd_prime_factors (int);
  ~vnl_fftxd_prime_factors ();
  
  const T *getFactors () const;     // array of factors
  int getNumber () const;           // number which was factorized
  const int *getPvnl_qr () const;   // number of factors

  operator bool () const { return info >= 0; }

private:
  T *factors;
  int number; // the number that is being split into prime-facs
  int iPvnl_qr[3]; // store PQ and R (number = (2^P)+(3^Q)+(5^R)
  int info;
  
  // disallow
  vnl_fftxd_prime_factors (const vnl_fftxd_prime_factors<T> &);
  vnl_fftxd_prime_factors<T>& operator= (const vnl_fftxd_prime_factors<T>&);
};
  
#endif // vnl_fftxd_prime_factors_h_
