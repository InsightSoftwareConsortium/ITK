// This is core/vnl/algo/vnl_convolve.h
#ifndef vnl_convolve_h_
#define vnl_convolve_h_
//:
// \file
// \brief Templated 1D and 2D convolution
// \author Peter Vanroose
// \date 22 August 2001
//
// This file contains function declarations for 1D and 2D convolutions,
// both cyclic and non-cyclic, of vnl_vectors and vnl_matrices.
// One can choose between straightforward `time-domain' implementations
// or using FFT to do a `frequency-domain' calculation.

#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_algo_export.h>

//: Convolve two vnl_vector<T>'s, possibly with different base types T.
//  $res[k] := \displaystyle\sum_{i=-\infty}^{+\infty} v1[k-i] \cdot v2[i]$.
//
//  The returned vnl_vector has base type U (the third argument).
//  All calculations are done with type U, so take care!
//  To specify the third argument, pass e.g. a null pointer, casted to U*.
//  Thus: vnl_convolve(v1, v2, (double*)0)
//  would convolve v1 and v2, and return a vnl_vector<double>.
//
//  This convolution is non-cyclic, and the length of the result is
//  one less than the sum of the lengths of the two input vectors.
//  But if one of the arguments has length 0, the result has length 0.
//
//  When specifying a non-zero 4th argument, FFTs are used to compute
//  the result (see below), which should be identical.
//  The speed of execution may of course differ.
//  In this case both vectors are padded with a sufficient number of zeros,
//  making the length at least that 4th argument,
//  then vnl_convolve_cyclic() is applied.
//
template <class T1, class T2, class U>
VNL_TEMPLATE_EXPORT
vnl_vector<U>
vnl_convolve(vnl_vector<T1> const& v1, vnl_vector<T2> const& v2,
             U*, int use_fft = 0);


//: Convolve two vnl_vector<T>'s, with the same base type T.
//
//  The returned vnl_vector has the same base type T, and is identical to
//  the return value of the previous function when T1 = T2 = U.
//
//  \relatesalso vnl_vector
template <class T>
VNL_TEMPLATE_EXPORT
vnl_vector<T>
vnl_convolve(vnl_vector<T> const& v1, vnl_vector<T> const& v2,
             int use_fft = 0);


//: Cyclically convolve two vnl_vector<T>'s of the same length.
//  $res[k] := \displaystyle\sum_{i=0}^{n-1} v1[k-i] \cdot v2[i]$.
//
//  A cyclic convolution requires that the lengths of the input vectors
//  are identical.  If this is not the case, an assert failure occurs.
//  The length of the returned vector equals the length of the inputs.
//
//  Since the convolution theorem states that a cyclic convolution followed by
//  an FFT is the same as an FFT followed by a multiplication, a cyclic
//  convolution can also be implemented using 3 FFTs on n points and n complex
//  multiplications.
//  By default, vnl_convolve_cyclic does not use FFTs.  By specifying "true" as
//  the fourth argument, calculation of the convolution is done using FFTs.
//  This will generally be faster for large n, especially if the vectors are
//  not sparse, and/or if n is a power of 2.
//
//  \relatesalso vnl_vector

template <class T1, class T2, class U>
VNL_TEMPLATE_EXPORT
vnl_vector<U>
vnl_convolve_cyclic(vnl_vector<T1> const& v1, vnl_vector<T2> const& v2,
                    U*, bool use_fft = false);

// Not yet implemented
template <class T1, class T2, class U>
vnl_matrix<U>
vnl_convolve(vnl_matrix<T1> const& v1, vnl_matrix<T2> const& v2,
             U*, int use_fft = 0);

// Not yet implemented
template <class T>
vnl_matrix<T>
vnl_convolve(vnl_matrix<T> const& v1, vnl_matrix<T> const& v2,
             int use_fft = 0);

// Not yet implemented
template <class T1, class T2, class U>
vnl_matrix<U>
vnl_convolve_cyclic(vnl_matrix<T1> const& v1, vnl_matrix<T2> const& v2,
                    U*, bool use_fft = false);

// Not yet implemented
template <class T1, class T2, class U>
vnl_matrix<U>
vnl_convolve(vnl_matrix<T1> const& v1, vnl_vector<T2> const& v2,
             U*, int use_fft = 0);

// Not yet implemented
template <class T>
vnl_matrix<T>
vnl_convolve(vnl_matrix<T> const& v1, vnl_vector<T> const& v2,
             int use_fft = 0);

#define VNL_CONVOLVE_INSTANTIATE(T) \
extern "please include vnl/algo/vnl_convolve.hxx first"

#endif // vnl_convolve_h_
