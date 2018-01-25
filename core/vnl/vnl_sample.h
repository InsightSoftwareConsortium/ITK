// This is core/vnl/vnl_sample.h
#ifndef vnl_sample_h_
#define vnl_sample_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif

#include "vnl/vnl_export.h"
//:
//  \file
//  \brief easy ways to sample from various probability distributions
// \author fsm
// \verbatim
//  Modifications
//   2005-01-01 Peter Vanroose - use simple (but robust) rng when no DRAND48
//   2007-03-26 Peter Vanroose - avoid returning log(0.0) by switching params
//   2010-09-12 Peter Vanroose - added implementation for binomial sampling
//   2010-09-12 Peter Vanroose - added Bernoulli (unfair coin toss) sampling
// \endverbatim

//: re-seed the random number generator.
VNL_EXPORT void vnl_sample_reseed();

//: re-seed the random number generator given a seed.
VNL_EXPORT void vnl_sample_reseed(int seed);

//: return a random number uniformly drawn on [a, b)
VNL_EXPORT double vnl_sample_uniform(double a, double b);

//: two independent samples from a standard normal distribution.
VNL_EXPORT void vnl_sample_normal_2(double *x, double *y);

//: Normal distribution with given mean and standard deviation
VNL_EXPORT double vnl_sample_normal(double mean, double sigma);

//: Return random k, where P(X = k) = [kth term in binomial expansion of (q + (1-q))^n].
// The returned value will lie between 0 and n (but will be -1 when input is nonsense).
VNL_EXPORT int vnl_sample_binomial(int n, double q);

//: Bernoulli distribution ("coin toss").
// The returned value will be 0 (with probability q) or 1 (with probability 1-q).
// For a "fair" coin toss, use q=0.5.
// When q does not lie between 0 and 1, the value -1 is returned.
VNL_EXPORT int vnl_sample_bernoulli(double q);

// ----------------------------------------

//: handy function to fill a range of values.
template <class I>
inline void vnl_sample_uniform(I begin, I end, double a, double b)
{
  for (I p=begin; p!=end; ++p)
    (*p) = vnl_sample_uniform(a, b);
}

//: handy function to fill a range of values.
template <class I>
inline void vnl_sample_normal(I begin, I end, double mean, double sigma)
{
  for (I p=begin; p!=end; ++p)
    (*p) = vnl_sample_normal(mean, sigma);
}

//: handy function to fill a range of values.
template <class I>
inline void vnl_sample_binomial(I begin, I end, int n, double q)
{
  for (I p=begin; p!=end; ++p)
    (*p) = vnl_sample_binomial(n, q);
}

//: handy function to fill a range of values.
template <class I>
inline void vnl_sample_bernoulli(I begin, I end, double q)
{
  for (I p=begin; p!=end; ++p)
    (*p) = vnl_sample_bernoulli(q);
}

//: handy function to fill a range of values.
template <class I, class T>
inline void vnl_sample_uniform(I begin, I end, double a, double b, T /*dummy*/)
{
  for (I p=begin; p!=end; ++p)
    (*p) = T(vnl_sample_uniform(a, b));
}

//: handy function to fill a range of values.
template <class I, class T>
inline void vnl_sample_normal(I begin, I end, double mean, double sigma, T /*dummy*/)
{
  for (I p=begin; p!=end; ++p)
    (*p) = T(vnl_sample_normal(mean, sigma));
}

#endif // vnl_sample_h_
