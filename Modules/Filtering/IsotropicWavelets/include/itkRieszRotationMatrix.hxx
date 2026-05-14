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
#ifndef itkRieszRotationMatrix_hxx
#define itkRieszRotationMatrix_hxx

#include "itkNumericTraits.h"
#include "itkMultiplyImageFilter.h"
#include "itkAddImageFilter.h"

namespace itk
{
template <unsigned int VImageDimension>
RieszRotationMatrix<VImageDimension>::RieszRotationMatrix()
  : Superclass()
  , m_SpatialRotationMatrix()
  ,

  m_MaxAbsoluteDifferenceCloseToZero(1 * itk::NumericTraits<RealType>::epsilon())

{}

template <unsigned int VImageDimension>
RieszRotationMatrix<VImageDimension>::RieszRotationMatrix(const Self & rieszMatrix)
  : Superclass(rieszMatrix)
  , m_SpatialRotationMatrix(rieszMatrix.GetSpatialRotationMatrix())
  , m_Order(rieszMatrix.GetOrder())
  , m_Components(rieszMatrix.GetComponents())
  , m_MaxAbsoluteDifferenceCloseToZero(1 * itk::NumericTraits<RealType>::epsilon())

{}

template <unsigned int VImageDimension>
RieszRotationMatrix<VImageDimension>::RieszRotationMatrix(const SpatialRotationMatrixType & spatialRotationMatrix,
                                                          const unsigned int &              order)
  : Superclass()
  , m_SpatialRotationMatrix(spatialRotationMatrix)
  , m_MaxAbsoluteDifferenceCloseToZero(1 * itk::NumericTraits<RealType>::epsilon())

{
  this->SetOrder(order);
  this->ComputeSteerableMatrix();
}

template <unsigned int VImageDimension>
template <typename TInputValue>
std::vector<TInputValue>
RieszRotationMatrix<VImageDimension>::MultiplyWithVector(const std::vector<TInputValue> & vect) const
{
  unsigned int rows = this->Rows();
  unsigned int cols = this->Cols();
  auto         resultVector = std::vector<TInputValue>(rows, NumericTraits<TInputValue>::ZeroValue());

  for (unsigned int r = 0; r < rows; r++)
  {
    for (unsigned int c = 0; c < cols; c++)
    {
      resultVector[r] += this->GetVnlMatrix()(r, c) * vect[c];
    }
  }
  return resultVector;
}

template <unsigned int VImageDimension>
template <typename TInputValue>
itk::VariableSizeMatrix<TInputValue>
RieszRotationMatrix<VImageDimension>::MultiplyWithColumnMatrix(
  const itk::VariableSizeMatrix<TInputValue> & inputColumn) const
{
  unsigned int rows = this->Rows();
  unsigned int cols = this->Cols();
  using ColumnMatrix = VariableSizeMatrix<TInputValue>;
  ColumnMatrix columnMatrix(rows, 1);
  columnMatrix.Fill(NumericTraits<TInputValue>::ZeroValue());

  for (unsigned int r = 0; r < rows; r++)
  {
    TInputValue sum = NumericTraits<TInputValue>::ZeroValue();
    for (unsigned int c = 0; c < cols; c++)
    {
      sum += this->GetVnlMatrix()(r, c) * inputColumn.GetVnlMatrix()(c, 0);
    }
    columnMatrix.GetVnlMatrix()(r, 0) = sum;
  }
  return columnMatrix;
}

template <unsigned int VImageDimension>
template <typename TImage>
std::vector<typename TImage::Pointer>
RieszRotationMatrix<VImageDimension>::MultiplyWithVectorOfImages(
  const std::vector<typename TImage::Pointer> & vect) const
{
  unsigned int rows = this->Rows();
  unsigned int cols = this->Cols();

  if (vect.size() != cols)
  {
    itkGenericExceptionMacro(<< "Matrix with " << this->Cols() << " columns cannot be "
                             << "multiplied with vector of images of length: " << vect.size());
  }

  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  std::vector<ImagePointer> result(rows);
  for (unsigned int r = 0; r < rows; r++)
  {
    // Init result image to zero.
    ImagePointer sum = ImageType::New();
    sum->SetRegions(vect[r]->GetLargestPossibleRegion());
    sum->Allocate();
    sum->FillBuffer(NumericTraits<typename ImageType::PixelType>::ZeroValue());

    for (unsigned int c = 0; c < cols; c++)
    {
      // sum += this->m_Matrix(r, c) * vect[r];
      using MultiplyImageFilterType = itk::MultiplyImageFilter<ImageType>;
      typename MultiplyImageFilterType::Pointer multiplyImageFilter = MultiplyImageFilterType::New();
      multiplyImageFilter->SetInput(vect[c]);
      multiplyImageFilter->SetConstant(this->GetVnlMatrix()(r, c));

      using AddImageFilterType = itk::AddImageFilter<ImageType>;
      typename AddImageFilterType::Pointer addImageFilter = AddImageFilterType::New();
      // Note that inPlace uses input1
      addImageFilter->SetInput1(sum);
      addImageFilter->SetInput2(multiplyImageFilter->GetOutput());
      addImageFilter->InPlaceOn();
      addImageFilter->Update();
      sum = addImageFilter->GetOutput();
    }
    result[r] = sum;
  }
  return result;
}

template <unsigned int VImageDimension>
typename RieszRotationMatrix<VImageDimension>::IndicesMatrix
RieszRotationMatrix<VImageDimension>::GenerateIndicesMatrix()
{
  using LocalIndicesArrayType = std::vector<unsigned int>;
  using LocalIndicesVector = std::vector<LocalIndicesArrayType>;
  using LocalLocalIndicesMatrixRow = std::vector<LocalIndicesVector>;
  using LocalIndicesMatrix = std::vector<LocalLocalIndicesMatrixRow>;
  LocalIndicesMatrix allIndicesPairs(
    this->m_Components,                                                                  // number of rows
    LocalLocalIndicesMatrixRow(this->m_Components,                                       // number of columns
                               LocalIndicesVector(2,                                     // pair of indices
                                                  LocalIndicesArrayType(VImageDimension) // dimension of the indices.
                                                  )));

  using SetType = std::set<LocalIndicesArrayType, std::greater<LocalIndicesArrayType>>;
  SetType allIndices = itk::utils::ComputeAllPossibleIndices<LocalIndicesArrayType, VImageDimension>(this->m_Order);

  // Populate LocalIndicesMatrix.
  {
    unsigned int ind_i = 0;
    for (auto itN = allIndices.begin(); itN != allIndices.end(); ++itN)
    {
      unsigned int ind_j = 0;
      for (const auto & oneIndex : allIndices)
      {
        allIndicesPairs[ind_i][ind_j][0] = *itN;
        allIndicesPairs[ind_i][ind_j][1] = oneIndex;
        ++ind_j;
      }
      ++ind_i;
    }
  }

  return allIndicesPairs;
}

template <unsigned int VImageDimension>
const typename RieszRotationMatrix<VImageDimension>::InternalMatrixType &
RieszRotationMatrix<VImageDimension>::ComputeSteerableMatrix()
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
    for (unsigned int i = 0; i < this->m_Components; ++i)
    {
      for (unsigned int j = 0; j < this->m_Components; ++j)
      {
        S.put(i, j, static_cast<ValueType>(m_SpatialRotationMatrix.GetVnlMatrix().get(i, j)));
      }
    }
    return this->GetVnlMatrix();
  }

  using SetType = std::set<IndicesArrayType, std::greater<IndicesArrayType>>;

  // Create map between i,j matrix indices and n,m multiindex.
  IndicesMatrix allIndicesPairs(this->GenerateIndicesMatrix());

  std::vector<SetType> allIndicesOrder(this->m_Order + 1);
  for (unsigned int ord = 1; ord < this->m_Order + 1; ++ord)
  {
    allIndicesOrder[ord] = itk::utils::ComputeAllPossibleIndices<IndicesArrayType, VImageDimension>(ord);
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
        for (auto itN = allIndicesOrder[kOrder].begin(); itN != allIndicesOrder[kOrder].end(); ++itN)
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
      using ValidIndicesType = std::vector<IndicesVector>;
      ValidIndicesType kValidIndices;
      for (const auto & itValid0 : kValidInitialIndices[0])
      {
        // IndicesVector tmp;
        // tmp.push_back(*itValid0);
        // kValidIndices.push_back( tmp );
        kValidIndices.push_back(IndicesVector(1, itValid0));
      }

      unsigned int combineIterations = VImageDimension - 1;
      for (unsigned int combineIndex = 0; combineIndex < combineIterations; ++combineIndex)
      {
        ValidIndicesType tmpValidIndices; // store new valid indices.
        for (auto & kValidIndex : kValidIndices)
        {
          for (const auto & itKIni : kValidInitialIndices[combineIndex + 1])
          {
            // ---verbose sum---
            IndicesArrayType sumKIndices = itKIni;
            for (auto & kIndex : kValidIndex)
            {
              for (unsigned int dim = 0; dim < VImageDimension; ++dim)
              {
                sumKIndices[dim] += kIndex[dim];
              }
            }
            // --- end verbose sum:---
            // if sum is valid: append index to tmpValidIndices.
            if (itk::utils::LessOrEqualIndiceComparisson<IndicesArrayType, VImageDimension>(sumKIndices, m))
            {
              IndicesVector tmp = kValidIndex;
              tmp.push_back(itKIni);
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
      ResultValueType result = 0;
      long            nFactorial = 1;
      long            mFactorial = 1;
      for (unsigned int dim = 0; dim < VImageDimension; ++dim)
      {
        nFactorial *= itk::utils::Factorial(n[dim]);
        mFactorial *= itk::utils::Factorial(m[dim]);
      }
      auto nFactorialReal = static_cast<RealType>(nFactorial);
      for (auto & kValidIndex : kValidIndices)
      {
        ValueType rotationFactor = 1;
        long      kFactorialMultiplication = 1;
        // There are always VImageDimension indices. (k1,k2,...,kd)
        for (unsigned int kIndex = 0; kIndex < VImageDimension; ++kIndex)
        {
          for (unsigned int dim = 0; dim < VImageDimension; ++dim)
          {
            const unsigned int & k = kValidIndex[kIndex][dim];
            // k1! = k11!*k12!*...*k1d!
            kFactorialMultiplication *= itk::utils::Factorial(k);
            // r11^k11*...*r1d^k1d
            // switch to avoid unnecessary calls to std::pow.
            if (k == 0)
            {
              continue;
            }
            else if (k == 1)
            {
              rotationFactor *= this->m_SpatialRotationMatrix[kIndex][dim];
            }
            else
            {
              rotationFactor *= std::pow(this->m_SpatialRotationMatrix[kIndex][dim], static_cast<int>(k));
            }
          }
        }
        result += nFactorialReal / kFactorialMultiplication * rotationFactor;
      }

      // normalize by sqrt(m!/n!):
      result *= sqrt(mFactorial / nFactorialReal);
      S[i][j] = static_cast<ValueType>(result);
      // Try to fix close to zero float errors
      if (itk::Math::FloatAlmostEqual(S[i][j].real(),
                                      static_cast<typename ValueType::value_type>(0),
                                      4, // default maxULPS from Math::AlmostFloatEqual
                                      this->m_MaxAbsoluteDifferenceCloseToZero))
      {
        S[i][j].real(0);
      }
      if (itk::Math::FloatAlmostEqual(S[i][j].imag(),
                                      static_cast<typename ValueType::value_type>(0),
                                      4, // default maxULPS from Math::AlmostFloatEqual
                                      this->m_MaxAbsoluteDifferenceCloseToZero))
      {
        S[i][j].imag(0);
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
        for (auto & kValidIndex : kValidIndices)
        {
          for (auto & v : kValidIndex)
          {
            std::cout << "( ";
            for (unsigned int dim = 0; dim < VImageDimension; ++dim)
            {
              std::cout << v[dim] << ", ";
            }
            std::cout << ") , ";
          }
          std::cout << "    ";
        }

        std::cout << "\n InitialValidIndices" << std::endl;
        for (unsigned int dim = 0; dim < VImageDimension; ++dim)
        {
          std::cout << "dim:" << dim << std::endl;
          for (const auto & it : kValidInitialIndices[dim])
          {
            std::cout << "( ";
            for (unsigned int innerDim = 0; innerDim < VImageDimension; ++innerDim)
            {
              std::cout << it[innerDim] << ", ";
            }
            std::cout << ")";
          }
          std::cout << std::endl;
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
