// This is core/vnl/algo/Templates/vnl_qr+std::complex+float--.cxx
#include <complex>
#include <iostream>
#include <vcl_compiler.h>
#include <vnl/algo/vnl_qr.hxx>
//:
// \file

#if 1
VNL_QR_INSTANTIATE(std::complex<float>);

#else
// the netlib qrsl routine seems to have a bug
// for single precision complex scalars, so let's
// try to use the double precision version instead.
//
// hmm... that still doesn't work.

#include <vcl_cassert.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_copy.h>

template <>
vnl_vector<std::complex<float>> vnl_qr<std::complex<float>>::solve(const vnl_vector<std::complex<float>>& b) const
{
  assert(!"this does not work");
  int n = qrdc_out_.columns();
  int p = qrdc_out_.rows();

  vnl_matrix<std::complex<double>> DOUBLE_qrdc_out_(qrdc_out_.rows(), qrdc_out_.cols());
  vnl_copy(qrdc_out_, DOUBLE_qrdc_out_);

  vnl_vector<std::complex<double>> DOUBLE_qraux_(qraux_.size());
  vnl_copy(qraux_, DOUBLE_qraux_);

  vnl_vector<std::complex<double>> DOUBLE_b(b.size());
  vnl_copy(b, DOUBLE_b);

  const std::complex<double> * DOUBLE_b_data = DOUBLE_b.data_block();
  vnl_vector<std::complex<double>> DOUBLE_QtB(n);
  vnl_vector<std::complex<double>> DOUBLE_x(p);

  // see comment above
  int JOB = 100;

  int info = 0;
  vnl_linpack_qrsl(DOUBLE_qrdc_out_.data_block(),
                   n, n, p,
                   DOUBLE_qraux_.data_block(),
                   DOUBLE_b_data, 0, DOUBLE_QtB.data_block(),
                   DOUBLE_x.data_block(),
                   0/*residual*/,
                   0/*Ax*/,
                   JOB,
                   &info);

  if (info > 0)
    std::cerr << "vnl_qr<T>::solve() : A is rank-deficient by " << info << '\n';

  vnl_vector<std::complex<float>> x(p);
  vnl_copy(DOUBLE_x, x);

  return x;
}

//: Return residual vector d of M x = b -> d = Q'b
template <>
vnl_vector<std::complex<float>> vnl_qr<std::complex<float>>::QtB(const vnl_vector<std::complex<float>>& b) const
{
  assert(!"this does not work");
  int n = qrdc_out_.columns();
  int p = qrdc_out_.rows();

  vnl_matrix<std::complex<double>> DOUBLE_qrdc_out_(qrdc_out_.rows(), qrdc_out_.cols());
  vnl_copy(qrdc_out_, DOUBLE_qrdc_out_);

  vnl_vector<std::complex<double>> DOUBLE_qraux_(qraux_.size());
  vnl_copy(qraux_, DOUBLE_qraux_);

  vnl_vector<std::complex<double>> DOUBLE_b(b.size());
  vnl_copy(b, DOUBLE_b);

  const std::complex<double> * DOUBLE_b_data = DOUBLE_b.data_block();
  vnl_vector<std::complex<double>> DOUBLE_QtB(n);

  // see comment above
  int JOB = 1000;

  int info = 0;
  vnl_linpack_qrsl(DOUBLE_qrdc_out_.data_block(),
                   n, n, p,
                   DOUBLE_qraux_.data_block(),
                   DOUBLE_b_data,
                   0,                       // A: Qb
                   DOUBLE_QtB.data_block(), // B: Q'b
                   0,                       // C: x
                   0,                       // D: residual
                   0,                       // E: Ax
                   JOB,
                   &info);

  if (info > 0) {
    std::cerr << "vnl_qr<T>::QtB() -- A is rank-def by " << info << '\n';
  }

  vnl_vector<std::complex<float>> QtB(n);
  vnl_copy(DOUBLE_QtB, QtB);

  return QtB;
}

#endif
