// This is vxl/vnl/vnl_fastops.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   08 Dec 96
//
//-----------------------------------------------------------------------------

#include "vnl_fastops.h"

#include <vcl_cstdlib.h> // abort()
#include <vcl_cstring.h> // memset()
#include <vcl_iostream.h>

//: Compute $A^\top A$.
void vnl_fastops::AtA(const vnl_matrix<double>& A, vnl_matrix<double>* AtA)
{
  unsigned n = A.columns();
  // Verify output is the right size
  if (AtA->rows() != n || AtA->columns() != n) {
    vcl_cerr << "vnl_fastops::AtA: " << AtA->rows() << "x" << AtA->columns() << " is not " << n << "x" << n << '\n';
    vcl_abort();
  }

  int m = A.rows();

  double const* const* a = A.data_array();
  double** ata = AtA->data_array();

  if (0) {
    for (unsigned i = 0; i < n; ++i)
      for (unsigned j = i; j < n; ++j) {
        double accum = 0;
        for (int k = 0; k < m; ++k)
          accum += a[k][i] * a[k][j];
        ata[i][j] = ata[j][i] = accum;
      }
  } else {
    // 5 times faster on 600 Mhz Pentium III for m = 10000, n = 50
    vcl_memset(ata[0], 0, n * n * sizeof ata[0][0]);
    for (int k = 0; k < m; ++k)
      for (unsigned i = 0; i < n; ++i) {
        double aki = a[k][i];
        double const* arow = a[k] + i;
        double* atarow = ata[i] + i;
        double const* arowend = a[k] + n;
        while (arow != arowend)
          *atarow++ += aki * *arow++;
      }
      for (unsigned i = 0; i < n; ++i)
        for (unsigned j = i+1; j < n; ++j)
          ata[j][i] = ata[i][j];
  }
}

//: Compute AxB.
void vnl_fastops::AB(const vnl_matrix<double>& A, const vnl_matrix<double>& B, vnl_matrix<double>* out_ptr)
{
  unsigned ma = A.rows();
  unsigned na = A.columns();
  unsigned mb = B.rows();
  unsigned nb = B.columns();

  // Verify matrices compatible
  if (na != mb) {
    vcl_cerr << "vnl_fastops::AB: " << na << " != " << mb << '\n';
    vcl_abort();
  }

  // Verify output is the right size
  if (out_ptr->rows() != ma || out_ptr->columns() != nb) {
    vcl_cerr << "vnl_fastops::AB: " << out_ptr->rows() << "x" << out_ptr->columns() << " is not " << ma << "x" << nb << '\n';
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** out = out_ptr->data_array();

  for (unsigned i = 0; i < ma; ++i)
    for (unsigned j = 0; j < nb; ++j) {
      double accum = 0;
      for (unsigned k = 0; k < na; ++k)
        accum += a[i][k] * b[k][j];
      out[i][j] = accum;
    }
}

//:  Compute $A^\top B$.
void vnl_fastops::AtB(const vnl_matrix<double>& A, const vnl_matrix<double>& B, vnl_matrix<double>* out_ptr)
{
  unsigned ma = A.rows();
  unsigned na = A.columns();
  unsigned mb = B.rows();
  unsigned nb = B.columns();

  // Verify matrices compatible
  if (ma != mb) {
    vcl_cerr << "vnl_fastops::AtA: " << ma << " != " << mb << '\n';
    vcl_abort();
  }

  // Verify output is the right size
  if (out_ptr->rows() != na || out_ptr->columns() != nb) {
    vcl_cerr << "vnl_fastops::AtA: " << out_ptr->rows() << "x" << out_ptr->columns() << " is not " << na << "x" << nb << '\n';
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** out = out_ptr->data_array();

  for (unsigned i = 0; i < na; ++i)
    for (unsigned j = 0; j < nb; ++j) {
      double accum = 0;
      for (unsigned k = 0; k < ma; ++k)
        accum += a[k][i] * b[k][j];
      out[i][j] = accum;
    }
}

//: Compute $A^\top b$ for vector b. out_ptr may not be b.
void vnl_fastops::AtB(const vnl_matrix<double>& A, const vnl_vector<double>& B, vnl_vector<double>* out_ptr)
{
  unsigned m = A.rows();
  unsigned n = A.columns();
  unsigned l = B.size();

  // Verify matrices compatible
  if (m != l) {
    vcl_cerr << "vnl_fastops::AtB: " << m << " != " << l << '\n';
    vcl_abort();
  }

  // Verify output is the right size
  if (out_ptr->size() != n) {
    vcl_cerr << "vnl_fastops::AtA: Output vector wrong size.  Is " << out_ptr->size() << ", should be " << n << '\n';
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* b = B.data_block();
  double* out = out_ptr->data_block();

  for (unsigned i = 0; i < n; ++i) {
    double accum = 0;
    for (unsigned k = 0; k < l; ++k)
      accum += a[k][i] * b[k];
   out[i] = accum;
  }
}

//: Compute $A B^\top$.
void vnl_fastops::ABt(const vnl_matrix<double>& A, const vnl_matrix<double>& B, vnl_matrix<double>* out_ptr)
{
  unsigned ma = A.rows();
  unsigned na = A.columns();
  unsigned mb = B.rows();
  unsigned nb = B.columns();

  // Verify matrices compatible
  if (na != nb) {
    vcl_cerr << "vnl_fastops::ABt: " << na << " != " << nb << '\n';
    vcl_abort();
  }

  // Verify output is the right size
  if (out_ptr->rows() != ma || out_ptr->columns() != mb) {
    vcl_cerr << "vnl_fastops::ABt: " << out_ptr->rows() << "x" << out_ptr->columns() << " is not " << ma << "x" << mb << '\n';
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** out = out_ptr->data_array();

  for (unsigned i = 0; i < ma; ++i)
    for (unsigned j = 0; j < mb; ++j) {
      double accum = 0;
      for (unsigned k = 0; k < na; ++k)
        accum += a[i][k] * b[j][k];
      out[i][j] = accum;
    }
}

//: Compute $ X += A^\top A$
void vnl_fastops::inc_X_by_AtA(vnl_matrix<double>& X, const vnl_matrix<double>& A)
{
  unsigned m = X.rows();
  unsigned n = X.columns();
  unsigned l = A.rows();

  if (m != n || m != A.columns()) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtA: size error\n";
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double** x = X.data_array();

  if (l == 2) {
    for (unsigned i = 0; i < n; ++i) {
      x[i][i] += (a[0][i] * a[0][i] + a[1][i] * a[1][i]);
      for (unsigned j = i+1; j < n; ++j) {
        double accum = (a[0][i] * a[0][j] + a[1][i] * a[1][j]);
        x[i][j] += accum;
        x[j][i] += accum;
      }
    }
  } else {
    for (unsigned i = 0; i < n; ++i)
      for (unsigned j = i; j < n; ++j) {
        double accum = 0;
        for (unsigned k = 0; k < l; ++k)
          accum += a[k][i] * a[k][j];
        x[i][j] += accum;
        if (i != j)
          x[j][i] += accum;
      }
  }
}

// Compute $X += A^\top B$
void vnl_fastops::inc_X_by_AtB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  unsigned mx = X.rows();
  unsigned nx = X.columns();
  unsigned ma = A.rows();
  unsigned na = A.columns();
  unsigned mb = B.rows();
  unsigned nb = B.columns();

  // Verify matrices compatible
  if (ma != mb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: A " << ma << " != B " << mb << '\n';
    vcl_abort();
  }

  // Verify output is the right size
  if (mx != na || nx != nb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: X " << mx << "x" << nx << " is not A " << na << "x" << nb << '\n';
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** x = X.data_array();

  for (unsigned i = 0; i < na; ++i)
    for (unsigned j = 0; j < nb; ++j) {
      double accum = 0;
      for (unsigned k = 0; k < ma; ++k)
        accum += a[k][i] * b[k][j];
      x[i][j] += accum;
    }
}

//: Compute $X += A^\top b$
void vnl_fastops::inc_X_by_AtB(vnl_vector<double>& X, const vnl_matrix<double>& A, const vnl_vector<double>& B)
{
  unsigned mx = X.size();
  unsigned ma = A.rows();
  unsigned na = A.columns();
  unsigned mb = B.size();

  // Verify matrices compatible
  if (ma != mb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: " << ma << " != " << mb << '\n';
    vcl_abort();
  }

  // Verify output is the right size
  if (mx != na) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: " << mx << "x" << 1 << " is not " << na << "x" << 1 << '\n';
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* b = B.data_block();
  double* x = X.data_block();

  for (unsigned i = 0; i < na; ++i) {
    double accum = 0;
    for (unsigned k = 0; k < ma; ++k)
      accum += a[k][i] * b[k];
    x[i] += accum;
  }
}

//: Compute $X -= A^\top B$
void vnl_fastops::dec_X_by_AtB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  unsigned mx = X.rows();
  unsigned nx = X.columns();
  unsigned ma = A.rows();
  unsigned na = A.columns();
  unsigned mb = B.rows();
  unsigned nb = B.columns();

  // Verify matrices compatible
  if (ma != mb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: " << ma << " != " << mb << '\n';
    vcl_abort();
  }

  // Verify output is the right size
  if (mx != na || nx != nb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: " << mx << "x" << nx << " is not " << na << "x" << nb << '\n';
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** x = X.data_array();

  for (unsigned i = 0; i < na; ++i)
    for (unsigned j = 0; j < nb; ++j) {
      double accum = 0;
      for (unsigned k = 0; k < ma; ++k)
        accum += a[k][i] * b[k][j];
      x[i][j] -= accum;
    }
}

//: Compute dot product of a and b
double vnl_fastops::dot(const double* a, const double* b, int n)
{
  double accum = 0;
  // Method 2 is fastest on the u170 -- weird.
#define method 3
#if method == 1
  const double* aend = a + n;
  while (a != aend)
    accum += *a++ * *b++;
#endif
#if method == 2
  for (int k = 0; k < n; ++k)
    accum += a[k] * b[k];
#endif
#if method == 3
  while (n--)
    accum += a[n] * b[n];
#endif
  return accum;
}

//: Compute $X -= A B^\top$
void vnl_fastops::dec_X_by_ABt(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  unsigned mx = X.rows();
  unsigned nx = X.columns();
  unsigned ma = A.rows();
  unsigned na = A.columns();
  unsigned mb = B.rows();
  unsigned nb = B.columns();

  // Verify matrices compatible
  if (na != nb) {
    vcl_cerr << "vnl_fastops::dec_X_by_ABt: A.columns " << na << " != B.columns " << nb << '\n';
    vcl_abort();
  }

  // Verify output is the right size
  if (mx != ma || nx != mb) {
    vcl_cerr << "vnl_fastops::dec_X_by_ABt: X size " << mx << "x" << nx << " is not AB^T " << ma << "x" << mb << '\n';
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** x = X.data_array();

  if (na == 3) {
    for (unsigned i = 0; i < mb; ++i)
      for (unsigned j = 0; j < ma; ++j)
        x[j][i] -= (a[j][0] * b[i][0] +
                    a[j][1] * b[i][1] +
                    a[j][2] * b[i][2]);
  } else if (na == 2) {
    for (unsigned i = 0; i < mb; ++i)
      for (unsigned j = 0; j < ma; ++j)
        x[j][i] -= (a[j][0] * b[i][0] +
                    a[j][1] * b[i][1]);
  } else {
    for (unsigned i = 0; i < mb; ++i)
      for (unsigned j = 0; j < ma; ++j)
        x[j][i] -= dot(a[j], b[i], na);
  }
}
