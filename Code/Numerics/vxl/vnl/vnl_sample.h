#ifndef vnl_sample_h_
#define vnl_sample_h_
#ifdef __GNUC__
#pragma interface
#endif

// This is vxl/vnl/vnl_sample.h


//:
//  \file
//  \brief easy ways to sample from various probability distributions
//  \author unknown




//: re-seed the random number generator.
void vnl_sample_reseed();

//: uniform on [a, b)
double vnl_sample_uniform(double a, double b);

//: two independent samples from a standard normal distribution.
void vnl_sample_normal_2(double *x, double *y);

//: Normal distribution with given mean and standard deviation
double vnl_sample_normal(double mean, double sigma);

// P(X = k) = [kth term in binomial expansion of (p + (1-p))^n]
//int vnl_sample_binomial(int n, int k, double p);

#endif // vnl_sample_h_
