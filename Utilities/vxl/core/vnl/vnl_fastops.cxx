// This is core/vnl/vnl_fastops.cxx
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
void vnl_fastops::AtA(vnl_matrix<double>& out, const vnl_matrix<double>& A)
{
  const unsigned int n = A.columns();
  // Verify output is the right size
  if (out.rows() != n || out.columns() != n)
    out.set_size(n,n);

  const unsigned int m = A.rows();

  double const* const* a = A.data_array();
  double** ata = out.data_array();

#if 0
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = i; j < n; ++j) {
        double accum = 0;
        for (unsigned int k = 0; k < m; ++k)
          accum += a[k][i] * a[k][j];
        ata[i][j] = ata[j][i] = accum;
      }
#else // 5 times faster on 600 Mhz Pentium III for m = 10000, n = 50
    vcl_memset(ata[0], 0, n * n * sizeof ata[0][0]);
    for (unsigned int k = 0; k < m; ++k)
      for (unsigned int i = 0; i < n; ++i) {
        double aki = a[k][i];
        double const* arow = a[k] + i;
        double* atarow = ata[i] + i;
        double const* arowend = a[k] + n;
        while (arow != arowend)
          *atarow++ += aki * *arow++;
      }
      for (unsigned int i = 0; i < n; ++i)
        for (unsigned int j = i+1; j < n; ++j)
          ata[j][i] = ata[i][j];
#endif // 0
}

//: Compute AxB.
void vnl_fastops::AB(vnl_matrix<double>& out, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  const unsigned int na = A.columns();
  const unsigned int mb = B.rows();

  // Verify matrices compatible
  if (na != mb) {
    vcl_cerr << "vnl_fastops::AB: argument sizes do not match: " << na << " != " << mb << '\n';
    vcl_abort();
  }

  const unsigned int ma = A.rows();
  const unsigned int nb = B.columns();

  // Verify output is the right size
  if (out.rows() != ma || out.columns() != nb)
    out.set_size(ma,nb);

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** outdata = out.data_array();

  for (unsigned int i = 0; i < ma; ++i)
    for (unsigned int j = 0; j < nb; ++j) {
      double accum = 0;
      for (unsigned int k = 0; k < na; ++k)
        accum += a[i][k] * b[k][j];
      outdata[i][j] = accum;
    }
}

//: Compute $A^\top B$.
void vnl_fastops::AtB(vnl_matrix<double>& out, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  const unsigned int ma = A.rows();
  const unsigned int mb = B.rows();

  // Verify matrices compatible
  if (ma != mb) {
    vcl_cerr << "vnl_fastops::AtB: argument sizes do not match: " << ma << " != " << mb << '\n';
    vcl_abort();
  }

  const unsigned int na = A.columns();
  const unsigned int nb = B.columns();

  // Verify output is the right size
  if (out.rows() != na || out.columns() != nb)
    out.set_size(na,nb);

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** outdata = out.data_array();

  for (unsigned int i = 0; i < na; ++i)
    for (unsigned int j = 0; j < nb; ++j) {
      double accum = 0;
      for (unsigned int k = 0; k < ma; ++k)
        accum += a[k][i] * b[k][j];
      outdata[i][j] = accum;
    }
}

//: Compute $A^\top b$ for vector b. out may not be b.
void vnl_fastops::AtB(vnl_vector<double>& out, const vnl_matrix<double>& A, const vnl_vector<double>& B)
{
  const unsigned int m = A.rows();
  const unsigned int l = B.size();

  // Verify matrices compatible
  if (m != l) {
    vcl_cerr << "vnl_fastops::AtB: argument sizes do not match: " << m << " != " << l << '\n';
    vcl_abort();
  }

  const unsigned int n = A.columns();

  // Verify output is the right size
  if (out.size() != n)
    out.set_size(n);

  double const* const* a = A.data_array();
  double const* b = B.data_block();
  double* outdata = out.data_block();

  for (unsigned int i = 0; i < n; ++i) {
    double accum = 0;
    for (unsigned int k = 0; k < l; ++k)
      accum += a[k][i] * b[k];
   outdata[i] = accum;
  }
}

//: Compute $A b$ for vector b. out may not be b.
void vnl_fastops::Ab(vnl_vector<double>& out, const vnl_matrix<double>& A, const vnl_vector<double>& b)
{
  const unsigned int m = A.cols();
  const unsigned int l = b.size();

  // Verify matrices compatible
  if (m != l) {
    vcl_cerr << "vnl_fastops::Ab: argument sizes do not match: " << m << " != " << l << '\n';
    vcl_abort();
  }

  const unsigned int n = A.rows();

  // Verify output is the right size
  if (out.size() != n)
    out.set_size(n);

  double const* const* a = A.data_array();
  double const* bb = b.data_block();
  double* outdata = out.data_block();

  for (unsigned int i = 0; i < n; ++i) {
    double accum = 0;
    for (unsigned int k = 0; k < l; ++k)
      accum += a[i][k] * bb[k];
   outdata[i] = accum;
  }
}

//: Compute $A B^\top$.
void vnl_fastops::ABt(vnl_matrix<double>& out, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  const unsigned int na = A.columns();
  const unsigned int nb = B.columns();

  // Verify matrices compatible
  if (na != nb) {
    vcl_cerr << "vnl_fastops::ABt: argument sizes do not match: " << na << " != " << nb << '\n';
    vcl_abort();
  }

  const unsigned int ma = A.rows();
  const unsigned int mb = B.rows();

  // Verify output is the right size
  if (out.rows() != ma || out.columns() != mb)
    out.set_size(ma,mb);

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** outdata = out.data_array();

  for (unsigned int i = 0; i < ma; ++i)
    for (unsigned int j = 0; j < mb; ++j) {
      double accum = 0;
      for (unsigned int k = 0; k < na; ++k)
        accum += a[i][k] * b[j][k];
      outdata[i][j] = accum;
    }
}

//: Compute $ X += A^\top A$
void vnl_fastops::inc_X_by_AtA(vnl_matrix<double>& X, const vnl_matrix<double>& A)
{
  const unsigned int m = X.rows();
  const unsigned int n = X.columns();

  // Verify output is the right size
  if (m != n || m != A.columns()) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtA: argument sizes do not match\n";
    vcl_abort();
  }

  const unsigned int l = A.rows();

  double const* const* a = A.data_array();
  double** x = X.data_array();

  if (l == 2) {
    for (unsigned int i = 0; i < n; ++i) {
      x[i][i] += (a[0][i] * a[0][i] + a[1][i] * a[1][i]);
      for (unsigned int j = i+1; j < n; ++j) {
        double accum = (a[0][i] * a[0][j] + a[1][i] * a[1][j]);
        x[i][j] += accum;
        x[j][i] += accum;
      }
    }
  } else {
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = i; j < n; ++j) {
        double accum = 0;
        for (unsigned int k = 0; k < l; ++k)
          accum += a[k][i] * a[k][j];
        x[i][j] += accum;
        if (i != j)
          x[j][i] += accum;
      }
  }
}

//: Compute $X += A B$
void vnl_fastops::inc_X_by_AB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  const unsigned int na = A.columns();
  const unsigned int mb = B.rows();

  // Verify matrices compatible
  if (na != mb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AB: argument sizes do not match: " << na << " != " << mb << '\n';
    vcl_abort();
  }

  const unsigned int ma = A.rows();
  const unsigned int nb = B.columns();
  const unsigned int mx = X.rows();
  const unsigned int nx = X.columns();

  // Verify output is the right size
  if (mx != ma || nx != nb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AB: argument sizes do not match\n";
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** x = X.data_array();

  for (unsigned int i = 0; i < ma; ++i)
    for (unsigned int j = 0; j < nb; ++j)
      for (unsigned int k = 0; k < na; ++k)
        x[i][j] += a[i][k] * b[k][j];
}

//: Compute $X -= A B$
void vnl_fastops::dec_X_by_AB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  const unsigned int na = A.columns();
  const unsigned int mb = B.rows();

  // Verify matrices compatible
  if (na != mb) {
    vcl_cerr << "vnl_fastops::dec_X_by_AB: argument sizes do not match: " << na << " != " << mb << '\n';
    vcl_abort();
  }

  const unsigned int ma = A.rows();
  const unsigned int nb = B.columns();
  const unsigned int mx = X.rows();
  const unsigned int nx = X.columns();

  // Verify output is the right size
  if (mx != ma || nx != nb) {
    vcl_cerr << "vnl_fastops::dec_X_by_AB: argument sizes do not match\n";
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** x = X.data_array();

  for (unsigned int i = 0; i < ma; ++i)
    for (unsigned int j = 0; j < nb; ++j)
      for (unsigned int k = 0; k < na; ++k)
        x[i][j] -= a[i][k] * b[k][j];
}

//: Compute $X += A^\top B$
void vnl_fastops::inc_X_by_AtB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  const unsigned int ma = A.rows();
  const unsigned int mb = B.rows();

  // Verify matrices compatible
  if (ma != mb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: argument sizes do not match: " << ma << " != " << mb << '\n';
    vcl_abort();
  }

  const unsigned int na = A.columns();
  const unsigned int nb = B.columns();
  const unsigned int mx = X.rows();
  const unsigned int nx = X.columns();

  // Verify output is the right size
  if (mx != na || nx != nb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: argument sizes do not match\n";
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** x = X.data_array();

  for (unsigned int i = 0; i < na; ++i)
    for (unsigned int j = 0; j < nb; ++j) {
      double accum = 0;
      for (unsigned int k = 0; k < ma; ++k)
        accum += a[k][i] * b[k][j];
      x[i][j] += accum;
    }
}

//: Compute $X -= A^\top B$
void vnl_fastops::dec_X_by_AtB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  const unsigned int ma = A.rows();
  const unsigned int mb = B.rows();

  // Verify matrices compatible
  if (ma != mb) {
    vcl_cerr << "vnl_fastops::dec_X_by_AtB: argument sizes do not match: " << ma << " != " << mb << '\n';
    vcl_abort();
  }

  const unsigned int na = A.columns();
  const unsigned int nb = B.columns();
  const unsigned int mx = X.rows();
  const unsigned int nx = X.columns();

  // Verify output is the right size
  if (mx != na || nx != nb) {
    vcl_cerr << "vnl_fastops::dec_X_by_AtB: argument sizes do not match\n";
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** x = X.data_array();

  for (unsigned int i = 0; i < na; ++i)
    for (unsigned int j = 0; j < nb; ++j) {
      double accum = 0;
      for (unsigned int k = 0; k < ma; ++k)
        accum += a[k][i] * b[k][j];
      x[i][j] -= accum;
    }
}

//: Compute $X += A^\top b$
void vnl_fastops::inc_X_by_AtB(vnl_vector<double>& X, const vnl_matrix<double>& A, const vnl_vector<double>& B)
{
  const unsigned int ma = A.rows();
  const unsigned int mb = B.size();

  // Verify matrices compatible
  if (ma != mb) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: argument sizes do not match: " << ma << " != " << mb << '\n';
    vcl_abort();
  }

  const unsigned int mx = X.size();
  const unsigned int na = A.columns();

  // Verify output is the right size
  if (mx != na) {
    vcl_cerr << "vnl_fastops::inc_X_by_AtB: argument sizes do not match\n";
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* b = B.data_block();
  double* x = X.data_block();

  for (unsigned int i = 0; i < na; ++i) {
    double accum = 0;
    for (unsigned int k = 0; k < ma; ++k)
      accum += a[k][i] * b[k];
    x[i] += accum;
  }
}

//: Compute $X -= A^\top b$
void vnl_fastops::dec_X_by_AtB(vnl_vector<double>& X, const vnl_matrix<double>& A, const vnl_vector<double>& B)
{
  const unsigned int ma = A.rows();
  const unsigned int mb = B.size();

  // Verify matrices compatible
  if (ma != mb) {
    vcl_cerr << "vnl_fastops::dec_X_by_AtB: argument sizes do not match: " << ma << " != " << mb << '\n';
    vcl_abort();
  }

  const unsigned int mx = X.size();
  const unsigned int na = A.columns();

  // Verify output is the right size
  if (mx != na) {
    vcl_cerr << "vnl_fastops::dec_X_by_AtB: argument sizes do not match\n";
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* b = B.data_block();
  double* x = X.data_block();

  for (unsigned int i = 0; i < na; ++i) {
    double accum = 0;
    for (unsigned int k = 0; k < ma; ++k)
      accum += a[k][i] * b[k];
    x[i] -= accum;
  }
}

//: Compute $ X -= A^\top A$
void vnl_fastops::dec_X_by_AtA(vnl_matrix<double>& X, const vnl_matrix<double>& A)
{
  const unsigned int m = X.rows();
  const unsigned int n = X.columns();

  // Verify output is the right size
  if (m != n || m != A.columns()) {
    vcl_cerr << "vnl_fastops::dec_X_by_AtA: argument sizes do not match\n";
    vcl_abort();
  }

  const unsigned int l = A.rows();

  double const* const* a = A.data_array();
  double** x = X.data_array();

  if (l == 2) {
    for (unsigned int i = 0; i < n; ++i) {
      x[i][i] -= (a[0][i] * a[0][i] + a[1][i] * a[1][i]);
      for (unsigned int j = i+1; j < n; ++j) {
        double accum = (a[0][i] * a[0][j] + a[1][i] * a[1][j]);
        x[i][j] -= accum;
        x[j][i] -= accum;
      }
    }
  } else {
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = i; j < n; ++j) {
        double accum = 0;
        for (unsigned int k = 0; k < l; ++k)
          accum += a[k][i] * a[k][j];
        x[i][j] -= accum;
        if (i != j)
          x[j][i] -= accum;
      }
  }
}

//: Compute dot product of a and b
double vnl_fastops::dot(const double* a, const double* b, unsigned int n)
{
#define METHOD 3  // Method 2 is fastest on the u170 -- weird.
  double accum = 0;
#if METHOD == 1
  const double* aend = a + n;
  while (a != aend)
    accum += *a++ * *b++;
#endif
#if METHOD == 2
  for (unsigned int k = 0; k < n; ++k)
    accum += a[k] * b[k];
#endif
#if METHOD == 3
  while (n--)
    accum += a[n] * b[n];
#endif
#if METHOD == 4
  unsigned int k = n;
  while (k > 0)
    --k, accum += a[k] * b[k];
#endif
  return accum;
#undef METHOD
}

//: Compute $X += A B^\top$
void vnl_fastops::inc_X_by_ABt(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  const unsigned int na = A.columns();
  const unsigned int nb = B.columns();

  // Verify matrices compatible
  if (na != nb) {
    vcl_cerr << "vnl_fastops::inc_X_by_ABt: argument sizes do not match: " << na << " != " << nb << '\n';
    vcl_abort();
  }

  const unsigned int mx = X.rows();
  const unsigned int nx = X.columns();
  const unsigned int ma = A.rows();
  const unsigned int mb = B.rows();

  // Verify output is the right size
  if (mx != ma || nx != mb) {
    vcl_cerr << "vnl_fastops::inc_X_by_ABt: argument sizes do not match\n";
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** x = X.data_array();

  if (na == 3) {
    for (unsigned int i = 0; i < mb; ++i)
      for (unsigned int j = 0; j < ma; ++j)
        x[j][i] += (a[j][0] * b[i][0] +
                    a[j][1] * b[i][1] +
                    a[j][2] * b[i][2]);
  } else if (na == 2) {
    for (unsigned int i = 0; i < mb; ++i)
      for (unsigned int j = 0; j < ma; ++j)
        x[j][i] += (a[j][0] * b[i][0] +
                    a[j][1] * b[i][1]);
  } else if (na == 1) {
    for (unsigned int i = 0; i < mb; ++i)
      for (unsigned int j = 0; j < ma; ++j)
        x[j][i] += a[j][0] * b[i][0];
  } else {
    for (unsigned int i = 0; i < mb; ++i)
      for (unsigned int j = 0; j < ma; ++j)
        x[j][i] += dot(a[j], b[i], na);
  }
}

//: Compute $X -= A B^\top$
void vnl_fastops::dec_X_by_ABt(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B)
{
  const unsigned int na = A.columns();
  const unsigned int nb = B.columns();

  // Verify matrices compatible
  if (na != nb) {
    vcl_cerr << "vnl_fastops::dec_X_by_ABt: argument sizes do not match: " << na << " != " << nb << '\n';
    vcl_abort();
  }

  const unsigned int mx = X.rows();
  const unsigned int nx = X.columns();
  const unsigned int ma = A.rows();
  const unsigned int mb = B.rows();

  // Verify output is the right size
  if (mx != ma || nx != mb) {
    vcl_cerr << "vnl_fastops::dec_X_by_ABt: argument sizes do not match\n";
    vcl_abort();
  }

  double const* const* a = A.data_array();
  double const* const* b = B.data_array();
  double** x = X.data_array();

  if (na == 3) {
    for (unsigned int i = 0; i < mb; ++i)
      for (unsigned int j = 0; j < ma; ++j)
        x[j][i] -= (a[j][0] * b[i][0] +
                    a[j][1] * b[i][1] +
                    a[j][2] * b[i][2]);
  } else if (na == 2) {
    for (unsigned int i = 0; i < mb; ++i)
      for (unsigned int j = 0; j < ma; ++j)
        x[j][i] -= (a[j][0] * b[i][0] +
                    a[j][1] * b[i][1]);
  } else if (na == 1) {
    for (unsigned int i = 0; i < mb; ++i)
      for (unsigned int j = 0; j < ma; ++j)
        x[j][i] -= a[j][0] * b[i][0];
  } else {
    for (unsigned int i = 0; i < mb; ++i)
      for (unsigned int j = 0; j < ma; ++j)
        x[j][i] -= dot(a[j], b[i], na);
  }
}
