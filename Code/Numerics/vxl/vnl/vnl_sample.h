#ifndef vnl_sample_h_
#define vnl_sample_h_
#ifdef __GNUC__
#pragma interface
#endif

// .NAME vnl_sample - easy ways to sample from various probability distributions
// .HEADER vxl package
// .LIBRARY vnl
// .INCLUDE vnl/vnl_sample.h
// .FILE vnl_sample.cxx

// uniform on [a, b)
double vnl_sample_uniform(double a, double b); 

// normal distribution with given mean and standard deviation
double vnl_sample_normal(double mean, double sigma);

// P(X = k) = [kth term in binomial expansion of (p + (1-p))^n]
//int vnl_sample_binomial(int n, int k, double p); 

#endif // vnl_sample_h_
