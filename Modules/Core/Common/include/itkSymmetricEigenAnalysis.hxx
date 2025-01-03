/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSymmetricEigenAnalysis_hxx
#define itkSymmetricEigenAnalysis_hxx

#include "itkMath.h"
#include "itkMakeUniqueForOverwrite.h"

namespace itk
{
template <typename TMatrix, typename TVector, typename TEigenMatrix>
unsigned int
SymmetricEigenAnalysis<TMatrix, TVector, TEigenMatrix>::ComputeEigenValues(const TMatrix & A, TVector & D) const
{
  if (m_UseEigenLibrary && m_OrderEigenValues != EigenValueOrderEnum::DoNotOrder)
  {
    return ComputeEigenValuesWithEigenLibrary(A, D);
  }

  return ComputeEigenValuesLegacy(A, D);
}

template <typename TMatrix, typename TVector, typename TEigenMatrix>
unsigned int
SymmetricEigenAnalysis<TMatrix, TVector, TEigenMatrix>::ComputeEigenValuesAndVectors(const TMatrix & A,
                                                                                     TVector &       EigenValues,
                                                                                     TEigenMatrix &  EigenVectors) const
{
  if (m_UseEigenLibrary)
  {
    return ComputeEigenValuesAndVectorsWithEigenLibrary(A, EigenValues, EigenVectors);
  }

  return ComputeEigenValuesAndVectorsLegacy(A, EigenValues, EigenVectors);
}

template <typename TMatrix, typename TVector, typename TEigenMatrix>
unsigned int
SymmetricEigenAnalysis<TMatrix, TVector, TEigenMatrix>::ComputeEigenValuesLegacy(const TMatrix & A, TVector & D) const
{
  const auto workArea1 = std::make_unique<double[]>(m_Dimension);

  // Copy the input matrix
  const auto inputMatrix = make_unique_for_overwrite<double[]>(m_Dimension * m_Dimension);
  const auto dVector = make_unique_for_overwrite<double[]>(m_Dimension);

  unsigned int k = 0;

  for (unsigned int row = 0; row < m_Dimension; ++row)
  {
    dVector[row] = D[row];

    for (unsigned int col = 0; col < m_Dimension; ++col)
    {
      inputMatrix[k++] = A(row, col);
    }
  }

  this->ReduceToTridiagonalMatrix(inputMatrix.get(), dVector.get(), workArea1.get(), workArea1.get());
  const unsigned int eigenErrIndex = this->ComputeEigenValuesUsingQL(dVector.get(), workArea1.get());

  for (unsigned int i = 0; i < m_Dimension; ++i)
  {
    D[i] = dVector[i];
  }

  return eigenErrIndex; // index of eigenvalue that could not be computed
}

template <typename TMatrix, typename TVector, typename TEigenMatrix>
unsigned int
SymmetricEigenAnalysis<TMatrix, TVector, TEigenMatrix>::ComputeEigenValuesAndVectorsLegacy(
  const TMatrix & A,
  TVector &       EigenValues,
  TEigenMatrix &  EigenVectors) const
{
  const auto workArea1 = std::make_unique<double[]>(m_Dimension);
  const auto workArea2 = std::make_unique<double[]>(m_Dimension * m_Dimension);

  // Copy the input matrix
  const auto inputMatrix = make_unique_for_overwrite<double[]>(m_Dimension * m_Dimension);
  const auto dVector = make_unique_for_overwrite<double[]>(m_Dimension);

  unsigned int k = 0;

  for (unsigned int row = 0; row < m_Dimension; ++row)
  {
    dVector[row] = EigenValues[row];

    for (unsigned int col = 0; col < m_Dimension; ++col)
    {
      inputMatrix[k++] = A(row, col);
    }
  }

  this->ReduceToTridiagonalMatrixAndGetTransformation(
    inputMatrix.get(), dVector.get(), workArea1.get(), workArea2.get());

  const unsigned int eigenErrIndex =
    this->ComputeEigenValuesAndVectorsUsingQL(dVector.get(), workArea1.get(), workArea2.get());

  // Copy eigenvectors
  k = 0;
  for (unsigned int row = 0; row < m_Dimension; ++row)
  {
    EigenValues[row] = dVector[row];
    for (unsigned int col = 0; col < m_Dimension; ++col)
    {
      EigenVectors[row][col] = workArea2[k++];
    }
  }

  return eigenErrIndex; // index of eigenvalue that could not be computed
}

template <typename TMatrix, typename TVector, typename TEigenMatrix>
void
SymmetricEigenAnalysis<TMatrix, TVector, TEigenMatrix>::ReduceToTridiagonalMatrix(double * a,
                                                                                  double * d,
                                                                                  double * e,
                                                                                  double * e2) const
{
  // Local variables
  for (int i = 0; i < static_cast<int>(m_Order); ++i)
  {
    d[i] = a[m_Order - 1 + i * m_Dimension];
    a[m_Order - 1 + i * m_Dimension] = a[i + i * m_Dimension];
  }

  for (int i = m_Order - 1; i >= 0; --i)
  {
    const int l = i - 1;
    double    h = 0.;
    double    scale = 0.;

    // Scale row (algol tol then not needed)
    for (int k = 0; k <= l; ++k)
    {
      scale += itk::Math::abs(d[k]);
    }

    if (scale == 0.)
    {
      for (int j = 0; j <= l; ++j)
      {
        d[j] = a[l + j * m_Dimension];
        a[l + j * m_Dimension] = a[i + j * m_Dimension];
        a[i + j * m_Dimension] = 0.;
      }
      e[i] = 0.;
      e2[i] = 0.;
      continue;
    }
    for (int k = 0; k <= l; ++k)
    {
      d[k] /= scale;
      h += d[k] * d[k];
    }

    e2[i] = scale * scale * h;
    double       f = d[l];
    const double d__1 = std::sqrt(h);
    double       g = (-1.0) * itk::Math::sgn0(f) * itk::Math::abs(d__1);
    e[i] = scale * g;
    h -= f * g;
    d[l] = f - g;
    if (l != 0)
    {
      // Form a*u
      for (int j = 0; j <= l; ++j)
      {
        e[j] = 0.;
      }

      for (int j = 0; j <= l; ++j)
      {
        f = d[j];
        g = e[j] + a[j + j * m_Dimension] * f;

        for (int k = j + 1; k <= l; ++k)
        {
          g += a[k + j * m_Dimension] * d[k];
          e[k] += a[k + j * m_Dimension] * f;
        }
        e[j] = g;
      }

      // Form p
      f = 0.;

      for (int j = 0; j <= l; ++j)
      {
        e[j] /= h;
        f += e[j] * d[j];
      }

      h = f / (h + h);
      // Form q
      for (int j = 0; j <= l; ++j)
      {
        e[j] -= h * d[j];
      }

      // Form reduced a
      for (int j = 0; j <= l; ++j)
      {
        f = d[j];
        g = e[j];

        for (int k = j; k <= l; ++k)
        {
          a[k + j * m_Dimension] = a[k + j * m_Dimension] - f * e[k] - g * d[k];
        }
      }
    }

    for (int j = 0; j <= l; ++j)
    {
      f = d[j];
      d[j] = a[l + j * m_Dimension];
      a[l + j * m_Dimension] = a[i + j * m_Dimension];
      a[i + j * m_Dimension] = f * scale;
    }
  }
}

template <typename TMatrix, typename TVector, typename TEigenMatrix>
void
SymmetricEigenAnalysis<TMatrix, TVector, TEigenMatrix>::ReduceToTridiagonalMatrixAndGetTransformation(const double * a,
                                                                                                      double *       d,
                                                                                                      double *       e,
                                                                                                      double * z) const
{
  // Local variables
  for (unsigned int i = 0; i < m_Order; ++i)
  {
    for (unsigned int j = i; j < m_Order; ++j)
    {
      z[j + i * m_Dimension] = a[j + i * m_Dimension];
    }
    d[i] = a[m_Order - 1 + i * m_Dimension];
  }

  for (unsigned int i = m_Order - 1; i > 0; --i)
  {
    const unsigned int l = i - 1;
    double             h = 0.0;
    double             scale = 0.0;

    // Scale row (algol tol then not needed)
    for (unsigned int k = 0; k <= l; ++k)
    {
      scale += itk::Math::abs(d[k]);
    }

    if (scale == 0.0)
    {
      e[i] = d[l];

      for (unsigned int j = 0; j <= l; ++j)
      {
        d[j] = z[l + j * m_Dimension];
        z[i + j * m_Dimension] = 0.0;
        z[j + i * m_Dimension] = 0.0;
      }
    }
    else
    {
      for (unsigned int k = 0; k <= l; ++k)
      {
        d[k] /= scale;
        h += d[k] * d[k];
      }

      double       f = d[l];
      const double d__1 = std::sqrt(h);
      double       g = (-1.0) * itk::Math::sgn0(f) * itk::Math::abs(d__1);
      e[i] = scale * g;
      h -= f * g;
      d[l] = f - g;

      // Form a*u
      for (unsigned int j = 0; j <= l; ++j)
      {
        e[j] = 0.0;
      }

      for (unsigned int j = 0; j <= l; ++j)
      {
        f = d[j];
        z[j + i * m_Dimension] = f;
        g = e[j] + z[j + j * m_Dimension] * f;

        for (unsigned int k = j + 1; k <= l; ++k)
        {
          g += z[k + j * m_Dimension] * d[k];
          e[k] += z[k + j * m_Dimension] * f;
        }

        e[j] = g;
      }

      // Form p
      f = 0.0;

      for (unsigned int j = 0; j <= l; ++j)
      {
        e[j] /= h;
        f += e[j] * d[j];
      }

      const double hh = f / (h + h);

      // Form q
      for (unsigned int j = 0; j <= l; ++j)
      {
        e[j] -= hh * d[j];
      }

      // Form reduced a
      for (unsigned int j = 0; j <= l; ++j)
      {
        f = d[j];
        g = e[j];

        for (unsigned int k = j; k <= l; ++k)
        {
          z[k + j * m_Dimension] = z[k + j * m_Dimension] - f * e[k] - g * d[k];
        }

        d[j] = z[l + j * m_Dimension];
        z[i + j * m_Dimension] = 0.0;
      }
    }

    d[i] = h;
  }

  // Accumulation of transformation matrices
  for (unsigned int i = 1; i < m_Order; ++i)
  {
    const unsigned int l = i - 1;
    z[m_Order - 1 + l * m_Dimension] = z[l + l * m_Dimension];
    z[l + l * m_Dimension] = 1.0;
    const double h = d[i];
    if (h != 0.0)
    {
      for (unsigned int k = 0; k <= l; ++k)
      {
        d[k] = z[k + i * m_Dimension] / h;
      }

      for (unsigned int j = 0; j <= l; ++j)
      {
        double g = 0.0;

        for (unsigned int k = 0; k <= l; ++k)
        {
          g += z[k + i * m_Dimension] * z[k + j * m_Dimension];
        }

        for (unsigned int k = 0; k <= l; ++k)
        {
          z[k + j * m_Dimension] -= g * d[k];
        }
      }
    }

    for (unsigned int k = 0; k <= l; ++k)
    {
      z[k + i * m_Dimension] = 0.0;
    }
  }

  for (unsigned int i = 0; i < m_Order; ++i)
  {
    d[i] = z[m_Order - 1 + i * m_Dimension];
    z[m_Order - 1 + i * m_Dimension] = 0.0;
  }

  z[m_Order - 1 + (m_Order - 1) * m_Dimension] = 1.0;
  e[0] = 0.0;
}

template <typename TMatrix, typename TVector, typename TEigenMatrix>
unsigned int
SymmetricEigenAnalysis<TMatrix, TVector, TEigenMatrix>::ComputeEigenValuesUsingQL(double * d, double * e) const
{
  constexpr double c_b10 = 1.0;

  // Local variables
  unsigned int ierr = 0;

  if (m_Order == 1)
  {
    return 1;
  }
  unsigned int m;
  for (unsigned int i = 1; i < m_Order; ++i)
  {
    e[i - 1] = e[i];
  }

  double f = 0.;
  double tst1 = 0.;
  e[m_Order - 1] = 0.;

  for (unsigned int l = 0; l < m_Order; ++l)
  {
    unsigned int j = 0;
    double       h = itk::Math::abs(d[l]) + itk::Math::abs(e[l]);
    if (tst1 < h)
    {
      tst1 = h;
    }
    // Look for small sub-diagonal element
    for (m = l; m < m_Order - 1; ++m)
    {
      const double tst2 = tst1 + itk::Math::abs(e[m]);
      if (tst2 == tst1)
      {
        break;
      }
      // e(n) is always zero, so there is no exit through the bottom of the loop
    }

    if (m != l)
    {
      double tst2;
      do
      {
        if (j == 30)
        {
          // Set error: no convergence to an eigenvalue after 30 iterations
          ierr = l + 1;
          return ierr;
        }
        ++j;
        // Form shift
        double g = d[l];
        double p = (d[l + 1] - g) / (e[l] * 2.);
        double r = itk::Math::hypot(p, c_b10);
        d[l] = e[l] / (p + itk::Math::sgn0(p) * itk::Math::abs(r));
        d[l + 1] = e[l] * (p + itk::Math::sgn0(p) * itk::Math::abs(r));
        const double dl1 = d[l + 1];
        h = g - d[l];

        for (unsigned int i = l + 2; i < m_Order; ++i)
        {
          d[i] -= h;
        }

        f += h;
        // ql transformation
        p = d[m];
        double       c = 1.;
        double       c2 = c;
        const double el1 = e[l + 1];
        double       s = 0.;
        double       s2 = 0;
        double       c3 = c2;
        for (unsigned int i = m - 1; i >= l; --i)
        {
          c3 = c2;
          c2 = c;
          s2 = s;
          g = c * e[i];
          h = c * p;
          r = itk::Math::hypot(p, e[i]);
          e[i + 1] = s * r;
          s = e[i] / r;
          c = p / r;
          p = c * d[i] - s * g;
          d[i + 1] = h + s * (c * g + s * d[i]);
          if (i == l)
          {
            break;
          }
        }

        p = -s * s2 * c3 * el1 * e[l] / dl1;
        e[l] = s * p;
        d[l] = c * p;
        tst2 = tst1 + itk::Math::abs(e[l]);
      } while (tst2 > tst1);
    }

    const double p = d[l] + f;

    if (m_OrderEigenValues == EigenValueOrderEnum::OrderByValue)
    {
      // Order by value
      unsigned int i = l;
      for (; i > 0; --i)
      {
        if (p >= d[i - 1])
        {
          break;
        }
        d[i] = d[i - 1];
      }
      d[i] = p;
    }
    else if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
    {
      // Order by magnitude. Make eigenvalues positive
      unsigned int i = l;
      for (; i > 0; --i)
      {
        if (itk::Math::abs(p) >= itk::Math::abs(d[i - 1]))
        {
          break;
        }
        d[i] = d[i - 1];
      }
      d[i] = p;
    }
    else
    {
      d[l] = p;
    }
  }

  return ierr; // ierr'th eigenvalue that couldn't be computed
}

template <typename TMatrix, typename TVector, typename TEigenMatrix>
unsigned int
SymmetricEigenAnalysis<TMatrix, TVector, TEigenMatrix>::ComputeEigenValuesAndVectorsUsingQL(double * d,
                                                                                            double * e,
                                                                                            double * z) const
{
  constexpr double c_b10 = 1.0;
  if (m_Order == 1)
  {
    return 1;
  }

  unsigned int ierr = 0;
  double       c3 = 0.0;
  double       s2 = 0.0;
  for (unsigned int i = 1; i < m_Order; ++i)
  {
    e[i - 1] = e[i];
  }


  double f = 0.0;
  double tst1 = 0.;
  e[m_Order - 1] = 0.;

  for (unsigned int l = 0; l < m_Order; ++l)
  {
    unsigned int j = 0;
    double       h = itk::Math::abs(d[l]) + itk::Math::abs(e[l]);
    if (tst1 < h)
    {
      tst1 = h;
    }

    // Look for small sub-diagonal element
    unsigned int m;
    double       tst2;
    for (m = l; m < m_Order - 1; ++m)
    {
      tst2 = tst1 + itk::Math::abs(e[m]);
      if (tst2 == tst1)
      {
        break;
      }

      // e(n) is always zero, so there is no exit through the bottom of the loop
    }

    if (m != l)
    {
      do
      {
        if (j == 30)
        {
          // Set error: no convergence to an eigenvalue after 30 iterations
          ierr = l + 1;
          return ierr;
        }
        ++j;
        // Form shift
        double g = d[l];
        double p = (d[l + 1] - g) / (e[l] * 2.);
        double r = itk::Math::hypot(p, c_b10);
        d[l] = e[l] / (p + itk::Math::sgn0(p) * itk::Math::abs(r));
        d[l + 1] = e[l] * (p + itk::Math::sgn0(p) * itk::Math::abs(r));
        const double dl1 = d[l + 1];
        h = g - d[l];

        for (unsigned int i = l + 2; i < m_Order; ++i)
        {
          d[i] -= h;
        }

        f += h;
        // ql transformation
        p = d[m];
        double       c = 1.0;
        double       c2 = c;
        const double el1 = e[l + 1];
        double       s = 0.;

        for (unsigned int i = m - 1; i >= l; --i)
        {
          c3 = c2;
          c2 = c;
          s2 = s;
          g = c * e[i];
          h = c * p;
          r = itk::Math::hypot(p, e[i]);
          e[i + 1] = s * r;
          s = e[i] / r;
          c = p / r;
          p = c * d[i] - s * g;
          d[i + 1] = h + s * (c * g + s * d[i]);

          // Form vector
          for (unsigned int k = 0; k < m_Order; ++k)
          {
            h = z[k + (i + 1) * m_Dimension];
            z[k + (i + 1) * m_Dimension] = s * z[k + i * m_Dimension] + c * h;
            z[k + i * m_Dimension] = c * z[k + i * m_Dimension] - s * h;
          }
          if (i == l)
          {
            break;
          }
        }

        p = -s * s2 * c3 * el1 * e[l] / dl1;
        e[l] = s * p;
        d[l] = c * p;
        tst2 = tst1 + itk::Math::abs(e[l]);
      } while (tst2 > tst1);
    }

    d[l] += f;
  }

  // Order eigenvalues and eigenvectors
  if (m_OrderEigenValues == EigenValueOrderEnum::OrderByValue)
  {
    // Order by value
    for (unsigned int i = 0; i < m_Order - 1; ++i)
    {
      unsigned int k = i;
      double       p = d[i];

      for (unsigned int j = i + 1; j < m_Order; ++j)
      {
        if (d[j] >= p)
        {
          continue;
        }
        k = j;
        p = d[j];
      }

      if (k == i)
      {
        continue;
      }
      d[k] = d[i];
      d[i] = p;

      for (unsigned int j = 0; j < m_Order; ++j)
      {
        p = z[j + i * m_Dimension];
        z[j + i * m_Dimension] = z[j + k * m_Dimension];
        z[j + k * m_Dimension] = p;
      }
    }
  }
  else if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
  {
    // Order by magnitude
    for (unsigned int i = 0; i < m_Order - 1; ++i)
    {
      unsigned int k = i;
      double       p = d[i];

      for (unsigned int j = i + 1; j < m_Order; ++j)
      {
        if (itk::Math::abs(d[j]) >= itk::Math::abs(p))
        {
          continue;
        }
        k = j;
        p = d[j];
      }

      if (k == i)
      {
        continue;
      }

      d[k] = d[i];
      d[i] = p;

      for (unsigned int j = 0; j < m_Order; ++j)
      {
        p = z[j + i * m_Dimension];
        z[j + i * m_Dimension] = z[j + k * m_Dimension];
        z[j + k * m_Dimension] = p;
      }
    }
  }

  return ierr;
}
} // end namespace itk

#endif
