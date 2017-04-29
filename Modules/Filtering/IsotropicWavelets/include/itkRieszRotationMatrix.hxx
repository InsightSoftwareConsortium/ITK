/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkRieszRotationMatrix_hxx
#define itkRieszRotationMatrix_hxx

#include "itkRieszRotationMatrix.h"
#include "itkNumericTraits.h"

namespace itk
{
template <typename T, unsigned int VImageDimension>
RieszRotationMatrix<T, VImageDimension>::RieszRotationMatrix()
  : Superclass()
  , m_SpatialRotationMatrix()
  , m_Order(0)
  , m_Components(0)
  , m_MaxAbsoluteDifferenceCloseToZero(1 * itk::NumericTraits<ValueType>::epsilon())
  , m_Debug(false)
{}

template <typename T, unsigned int VImageDimension>
RieszRotationMatrix<T, VImageDimension>::RieszRotationMatrix(const Self & rieszMatrix)
  : Superclass(rieszMatrix)
  , m_SpatialRotationMatrix(rieszMatrix.GetSpatialRotationMatrix())
  , m_Order(rieszMatrix.GetOrder())
  , m_Components(rieszMatrix.GetComponents())
  , m_MaxAbsoluteDifferenceCloseToZero(1 * itk::NumericTraits<ValueType>::epsilon())
  , m_Debug(false)
{}

template <typename T, unsigned int VImageDimension>
RieszRotationMatrix<T, VImageDimension>::RieszRotationMatrix(const SpatialRotationMatrixType & spatialRotationMatrix,
                                                             const unsigned int &              order)
  : Superclass()
  , m_SpatialRotationMatrix(spatialRotationMatrix)
  , m_MaxAbsoluteDifferenceCloseToZero(1 * itk::NumericTraits<ValueType>::epsilon())
  , m_Debug(false)
{
  this->SetOrder(order);
  this->ComputeSteerableMatrix();
}

/**
 *  Product by a std::vector
 */
template <typename T, unsigned int VImageDimension>
std::vector<T>
RieszRotationMatrix<T, VImageDimension>::operator*(const std::vector<T> & vect) const
{
  unsigned int rows = this->Rows();
  unsigned int cols = this->Cols();

  if (vect.size() != cols)
  {
    itkGenericExceptionMacro(<< "Matrix with " << this->Cols() << " columns cannot be "
                             << "multiplied with vector of length: " << vect.size());
  }

  std::vector<T> result(rows);
  for (unsigned int r = 0; r < rows; r++)
  {
    T sum = NumericTraits<T>::ZeroValue();
    for (unsigned int c = 0; c < cols; c++)
    {
      sum += this->m_Matrix(r, c) * vect[c];
    }
    result[r] = sum;
  }
  return result;
}

template <typename T, unsigned int VImageDimension>
const typename RieszRotationMatrix<T, VImageDimension>::InternalMatrixType &
RieszRotationMatrix<T, VImageDimension>::ComputeSteerableMatrix()
{
  // precondition
  if (this->m_Order == 0)
  {
    itkGenericExceptionMacro(<< "RieszRotationMatrix has order zero, use SetOrder(n),"
                                " and SetSpatialRotationMatrix(R) before computing the steerable matrix");
  }
  InternalMatrixType & S = this->GetVnlMatrix();
  if (this->m_Order == 1)
  {
    S = this->GetSpatialRotationMatrix().GetVnlMatrix();
    // return this->GetVnlMatrix();
  }

  // Create map between i,j matrix indices and n,m multiindex.
  typedef std::vector<unsigned int>     IndicesArrayType;
  typedef std::vector<IndicesArrayType> IndicesVector;
  typedef std::vector<IndicesVector>    IndicesMatrixRow;
  typedef std::vector<IndicesMatrixRow> IndicesMatrix;
  IndicesMatrix                         allIndicesPairs(
    this->m_Components,                                              // number of rows
    IndicesMatrixRow(this->m_Components,                             // number of columns
                     IndicesVector(2,                                // pair of indices
                                   IndicesArrayType(VImageDimension) // dimension of the indices.
                                   )));

  typedef std::set<IndicesArrayType, std::greater<IndicesArrayType>> SetType;
  SetType allIndices = itk::utils::ComputeAllPossibleIndices<IndicesArrayType, VImageDimension>(this->m_Order);
  std::vector<SetType> allIndicesOrder(this->m_Order + 1);
  for (unsigned int ord = 1; ord < this->m_Order + 1; ++ord)
  {
    allIndicesOrder[ord] = itk::utils::ComputeAllPossibleIndices<IndicesArrayType, VImageDimension>(ord);
  }

  // Populate IndicesMatrix.
  {
    unsigned int ind_i = 0, ind_j = 0;
    for (typename SetType::const_iterator itN = allIndices.begin(); itN != allIndices.end(); ++itN)
    {
      ind_j = 0;
      for (typename SetType::const_iterator itM = allIndices.begin(); itM != allIndices.end(); ++itM)
      {
        allIndicesPairs[ind_i][ind_j][0] = *itN;
        allIndicesPairs[ind_i][ind_j][1] = *itM;
        ++ind_j;
      }
      ++ind_i;
    }
  }

  for (unsigned int i = 0; i < this->m_Components; ++i)
  {
    for (unsigned int j = 0; j < this->m_Components; ++j)
    {
      const IndicesArrayType & n = allIndicesPairs[i][j][0];
      const IndicesArrayType & m = allIndicesPairs[i][j][1];
      // Set initial valid indices based on n and m.
      std::vector<SetType> kValidInitialIndices(VImageDimension);
      for (unsigned int dim = 0; dim < VImageDimension; ++dim)
      {
        const unsigned int & kOrder = n[dim];
        if (kOrder == 0)
        {
          kValidInitialIndices[dim].insert(IndicesArrayType(VImageDimension));
          continue;
        }
        for (typename SetType::const_iterator itN = allIndicesOrder[kOrder].begin();
             itN != allIndicesOrder[kOrder].end();
             ++itN)
        {
          if (itk::utils::LessOrEqualIndiceComparisson<IndicesArrayType, VImageDimension>(*itN, m))
          {
            kValidInitialIndices[dim].insert(*itN);
          }
        }
      }
      // Select those set indices with \sum_i k_i = m among the the combination
      // of initial valid indices.
      // First combine indices in dimensions pairs. Example,
      // k1 combine k2 = k'
      // and then k' with k3 if exist, etc...

      // Initialize kValidIndices with k0:
      typedef std::vector<IndicesVector> ValidIndicesType;
      ValidIndicesType                   kValidIndices;
      for (typename SetType::const_iterator itValid0 = kValidInitialIndices[0].begin();
           itValid0 != kValidInitialIndices[0].end();
           ++itValid0)
      {
        // IndicesVector tmp;
        // tmp.push_back(*itValid0);
        // kValidIndices.push_back( tmp );
        kValidIndices.push_back(IndicesVector(1, *itValid0));
      }

      unsigned int combineIterations = VImageDimension - 1;
      for (unsigned int combineIndex = 0; combineIndex < combineIterations; ++combineIndex)
      {
        ValidIndicesType tmpValidIndices; // store new valid indices.
        for (unsigned int validKIndex = 0; validKIndex < kValidIndices.size(); ++validKIndex)
        {
          for (typename SetType::const_iterator itKIni = kValidInitialIndices[combineIndex + 1].begin();
               itKIni != kValidInitialIndices[combineIndex + 1].end();
               ++itKIni)
          {
            // ---verbose sum---
            IndicesArrayType sumKIndices = *itKIni;
            for (unsigned int kIndice = 0; kIndice < kValidIndices[validKIndex].size(); ++kIndice)
            {
              for (unsigned int dim = 0; dim < VImageDimension; ++dim)
              {
                sumKIndices[dim] += kValidIndices[validKIndex][kIndice][dim];
              }
            }
            // --- end verbose sum:---
            // if sum is valid: append indice to tmpValidIndices.
            if (itk::utils::LessOrEqualIndiceComparisson<IndicesArrayType, VImageDimension>(sumKIndices, m))
            {
              IndicesVector tmp = kValidIndices[validKIndex];
              tmp.push_back(*itKIni);
              tmpValidIndices.push_back(tmp);
            }
          } // end new k indices
        } // end partial valid index
        kValidIndices = tmpValidIndices;
      } // end combinations

      // kValidIndices at current i,j matrix positions
      // The vector kValidIndices holds a vector of valid sets of k vectors (k1,k2,k3),(k'1,k'2,k'3) ,...
      // where k_i is a multiindex: k1 = (k11, k12, ..., k1d)
      //
      // we calculate now r11^k11*...*r1d^k1d
      // matrix R = [r1,...,rd], where r_i are columns of the rotation matrix.
      // we sum and normalize them.
      S[i][j] = 0;
      long double result = 0;
      long        nFactorial = 1;
      long        mFactorial = 1;
      for (unsigned int dim = 0; dim < VImageDimension; ++dim)
      {
        nFactorial *= itk::utils::Factorial(n[dim]);
        mFactorial *= itk::utils::Factorial(m[dim]);
      }
      double nFactorialReal = static_cast<double>(nFactorial);
      for (unsigned int validKIndex = 0; validKIndex < kValidIndices.size(); ++validKIndex)
      {
        long double rotationFactor = 1;
        long        kFactorialMultiplication = 1;
        // There are always VImageDimension indices. (k1,k2,...,kd)
        for (unsigned int kIndice = 0; kIndice < VImageDimension; ++kIndice)
        {
          for (unsigned int dim = 0; dim < VImageDimension; ++dim)
          {
            const unsigned int & k = kValidIndices[validKIndex][kIndice][dim];
            // k1! = k11!*k12!*...*k1d!
            kFactorialMultiplication *= itk::utils::Factorial(k);
            // r11^k11*...*r1d^k1d
            // switch to avoid unnecesary calls to std::pow.
            if (k == 0)
            {
              continue;
            }
            else if (k == 1)
            {
              rotationFactor *= this->m_SpatialRotationMatrix[kIndice][dim];
            }
            else
            {
              rotationFactor *= std::pow(this->m_SpatialRotationMatrix[kIndice][dim], k);
            }
          }
        }
        result += nFactorialReal / kFactorialMultiplication * rotationFactor;
      }

      // normalize by sqrt(m!/n!):
      result *= sqrt(mFactorial / nFactorialReal);
      S[i][j] = static_cast<ValueType>(result);
      // Try to fix close to zero float errors
      if (itk::Math::FloatAlmostEqual(S[i][j],
                                      static_cast<ValueType>(0),
                                      4, // default maxULPS from Math::AlmostFloatEqual
                                      this->m_MaxAbsoluteDifferenceCloseToZero))
      {
        S[i][j] = 0;
      }

      if (this->GetDebug())
      {
        std::cout << "\n" << std::endl;
        std::cout << "i, j: " << i << ", " << j << std::endl;
        std::cout << "n, m: ";
        for (unsigned int p = 0; p < 2; ++p)
        {
          std::cout << "( ";
          for (unsigned int dim = 0; dim < VImageDimension; ++dim)
          {
            std::cout << allIndicesPairs[i][j][p][dim] << ", ";
          }
          std::cout << ") , ";
        }
        std::cout << std::endl;

        std::cout << "kValidIndices.size(): " << kValidIndices.size() << std::endl;
        for (unsigned int c = 0; c < kValidIndices.size(); ++c)
        {
          for (unsigned int v = 0; v < kValidIndices[c].size(); ++v)
          {
            std::cout << "( ";
            for (unsigned int dim = 0; dim < VImageDimension; ++dim)
            {
              std::cout << kValidIndices[c][v][dim] << ", ";
            }
            std::cout << ") , ";
          }
          std::cout << "    ";
        }

        std::cout << "\n InitialValidIndices" << std::endl;
        ;
        for (unsigned int dim = 0; dim < VImageDimension; ++dim)
        {
          std::cout << "dim:" << dim << std::endl;
          for (typename SetType::const_iterator it = kValidInitialIndices[dim].begin();
               it != kValidInitialIndices[dim].end();
               ++it)
          {
            std::cout << "( ";
            for (unsigned int innerDim = 0; innerDim < VImageDimension; ++innerDim)
            {
              std::cout << (*it)[innerDim] << ", ";
            }
            std::cout << ")";
          }
          std::cout << std::endl;
          ;
        }
      } // end Debug
    } // end j
  } // end i

  // ----- PRINT ---- //
  // Print allIndicesPairs
  if (this->GetDebug())
  {
    // for(typename SetType::const_iterator it = allIndices.begin(); it != allIndices.end(); ++it)
    //   {
    //   std::cout <<"( ";
    //   for (unsigned int i = 0; i<VImageDimension; ++i)
    //     {
    //     std::cout << (*it)[i] << ", ";
    //     }
    //   std::cout <<")";
    //   }
    //   std::cout << std::endl;;
    std::cout << std::endl;
    std::cout << "All Indices:" << std::endl;
    for (unsigned int i = 0; i < this->m_Components; ++i)
    {
      for (unsigned int j = 0; j < this->m_Components; ++j)
      {
        for (unsigned int p = 0; p < 2; ++p)
        {
          std::cout << "( ";
          for (unsigned int dim = 0; dim < VImageDimension; ++dim)
          {
            std::cout << allIndicesPairs[i][j][p][dim] << ", ";
          }
          std::cout << ") , ";
        }
        std::cout << "    ";
      }
      std::cout << "\n";
    }
    // for (unsigned int c = 0; c < kValidIndices.size(); ++c)
    //   {
    //   for (unsigned int v = 0; v < kValidIndices[c].size(); ++v)
    //     {
    //     std::cout <<"( ";
    //     for (unsigned int dim = 0; dim<VImageDimension; ++dim)
    //       {
    //       std::cout << kValidIndices[c][v][dim] << ", ";
    //       }
    //     std::cout <<") , ";
    //     }
    //   std::cout <<"    ";
    //   }
  } // end Debug

  return this->GetVnlMatrix(); // return S;
}

} // end namespace itk

#endif
