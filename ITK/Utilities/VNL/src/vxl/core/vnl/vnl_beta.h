// This is core/vnl/vnl_beta.h
#ifndef vnl_beta_h_
#define vnl_beta_h_
//:
//  \file
//  \brief implementation of the beta function, also called the Euler integral of the first kind
//  \author Gamze Tunali

#include "vnl_gamma.h"

#if 1 // implementation via vnl_log_gamma
//: Computation of beta function in terms of gamma function.
//  Actually, this implementation refers to vnl_log_gamma,
//  since this involves just a single call to std::exp instead of three.
template <class T>
inline double vnl_beta(T x, T y) {return vcl_exp(vnl_log_gamma(x)+vnl_log_gamma(y)-vnl_log_gamma(x+y)); }
#else // implementation via vnl_gamma; less efficient since it needs 3x vcl_exp
//: Computation of beta function in terms of gamma function.
template <class T>
inline double vnl_beta(T x, T y) {return (vnl_gamma(x)*vnl_gamma(y))/vnl_gamma(x+y); }
#endif

//: Computation of the log beta function in terms of the log gamma function.
//  vnl_log_beta is just the std::log (natural logarithm) of the beta function.
template <class T>
inline double vnl_log_beta(T x, T y) {return vnl_log_gamma(x)+vnl_log_gamma(y)-vnl_log_gamma(x+y); }

#endif
