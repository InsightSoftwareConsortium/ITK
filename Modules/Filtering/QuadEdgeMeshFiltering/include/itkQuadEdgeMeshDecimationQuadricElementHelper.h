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
#ifndef itkQuadEdgeMeshDecimationQuadricElementHelper_h
#define itkQuadEdgeMeshDecimationQuadricElementHelper_h

#include "itkPoint.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_matrix_inverse.h"

#include "itkTriangleHelper.h"

namespace itk
{
///TODO explicit specification for VDimension=3!!!
template< typename TPoint >
class QuadEdgeMeshDecimationQuadricElementHelper
{
public:

  typedef QuadEdgeMeshDecimationQuadricElementHelper Self;
  typedef TPoint                                     PointType;
  typedef typename PointType::CoordRepType           CoordType;

  itkStaticConstMacro(PointDimension, unsigned int, PointType::PointDimension);
  itkStaticConstMacro(NumberOfCoefficients, unsigned int,
                      PointDimension * ( PointDimension + 1 ) / 2 + PointDimension + 1);

  typedef typename PointType::VectorType VectorType;
  typedef vnl_matrix< CoordType >        VNLMatrixType;
  typedef vnl_vector_fixed< CoordType,
                            itkGetStaticConstMacro(PointDimension) >            VNLVectorType;
  typedef vnl_vector_fixed< CoordType,
                            itkGetStaticConstMacro(NumberOfCoefficients) >      CoefficientVectorType;
  typedef TriangleHelper< PointType > TriangleType;

  // *****************************************************************
  QuadEdgeMeshDecimationQuadricElementHelper():
    m_Coefficients(itk::NumericTraits< CoordType >::ZeroValue()),
    m_A(PointDimension, PointDimension, itk::NumericTraits< CoordType >::ZeroValue()),
    m_B(itk::NumericTraits< CoordType >::ZeroValue()),
    m_SVDAbsoluteThreshold( static_cast< CoordType >( 1e-6 ) ),
    m_SVDRelativeThreshold( static_cast< CoordType >( 1e-3 ) )
  {
    this->m_Rank = PointDimension;
  }

  QuadEdgeMeshDecimationQuadricElementHelper(const CoefficientVectorType & iCoefficients):
    m_Coefficients(iCoefficients),
    m_A(PointDimension, PointDimension, itk::NumericTraits< CoordType >::ZeroValue()),
    m_B(itk::NumericTraits< CoordType >::ZeroValue()),
    m_SVDAbsoluteThreshold( static_cast< CoordType >( 1e-3 ) ),
    m_SVDRelativeThreshold( static_cast< CoordType >( 1e-3 ) )
  {
    this->m_Rank = PointDimension;
    this->ComputeAMatrixAndBVector();
  }

  ~QuadEdgeMeshDecimationQuadricElementHelper()
  {}

  CoefficientVectorType GetCoefficients() const
  {
    return this->m_Coefficients;
  }

  VNLMatrixType GetAMatrix()
  {
    this->ComputeAMatrixAndBVector();
    return m_A;
  }

  VNLVectorType GetBVector()
  {
    ComputeAMatrixAndBVector();
    return m_B;
  }

  unsigned int GetRank() const
  {
    return m_Rank;
  }

  ///TODO this method should be really optimized!!!
  inline CoordType ComputeError(const PointType & iP) const
  {
    //     ComputeAMatrixAndBVector();
    vnl_svd< CoordType > svd(m_A, m_SVDAbsoluteThreshold);
    svd.zero_out_relative(m_SVDRelativeThreshold);
    CoordType oError = inner_product( iP.GetVnlVector(), svd.recompose() * iP.GetVnlVector() );

    return this->m_Coefficients[this->m_Coefficients.size() - 1] - oError;
    /*
    CoordType oError( 0. );

    std::vector< CoordType > pt( PointDimension + 1, 1. );

    unsigned int dim1( 0 ), dim2, k( 0 );

    while( dim1 < PointDimension )
      {
      pt[dim1] = iP[dim1];
      ++dim1;
      }

    for( dim1 = 0; dim1 < PointDimension + 1; ++dim1 )
      {
      oError += this->m_Coefficients[k++] * pt[dim1] * pt[dim1];

      for( dim2 = dim1 + 1; dim2 < PointDimension + 1; ++dim2 )
        {
        oError += 2. * this->m_Coefficients[k++] * pt[dim1] * pt[dim2];
        }
      }
    oError += this->m_Coefficients[k++];

    return oError;*/
  }

  ///TODO this method should be really optimized!!!
  inline CoordType ComputeErrorAtOptimalLocation(const PointType & iP)
  {
    PointType optimal_location = ComputeOptimalLocation(iP);

    return ComputeError(optimal_location);
  }

  PointType ComputeOptimalLocation(const PointType & iP)
  {
    ComputeAMatrixAndBVector();

    vnl_svd< CoordType > svd(m_A, m_SVDAbsoluteThreshold);
    svd.zero_out_relative(m_SVDRelativeThreshold);

    m_Rank = svd.rank();

    VNLVectorType y = m_B.as_vector() - m_A *iP.GetVnlVector();

    VNLVectorType displacement = svd.solve(y);
    PointType     oP;

    for ( unsigned int dim = 0; dim < PointDimension; dim++ )
      {
      oP[dim] = iP[dim] + displacement[dim];
      }

    return oP;
  }

  ///TODO to be implemented!!!
  PointType ComputeOptimalLocation(
    const unsigned int & )
  {}

  void AddTriangle( const PointType & iP1,
                    const PointType & iP2,
                    const PointType & iP3,
                    const CoordType & iWeight = static_cast< CoordType >( 1. ) )
  {
    VectorType N = TriangleType::ComputeNormal(iP1, iP2, iP3);

    AddPoint(iP1, N, iWeight);
  }

  void AddPoint( const PointType & iP,
                 const VectorType & iN,
                 const CoordType & iWeight = static_cast< CoordType >( 1. ) )
  {
    unsigned int k(0), dim1, dim2;

    CoordType d = -iN *iP.GetVectorFromOrigin();

    for ( dim1 = 0; dim1 < PointDimension; ++dim1 )
      {
      for ( dim2 = dim1; dim2 < PointDimension; ++dim2 )
        {
        this->m_Coefficients[k++] += iWeight * iN[dim1] * iN[dim2];
        }
      this->m_Coefficients[k++] += iWeight * iN[dim1] * d;
      }

    this->m_Coefficients[k++] += iWeight * d * d;
  }

  // ***********************************************************************
  // operators
  Self & operator=(const Self & iRight)
  {
    if(this != &iRight)
      {
      this->m_Coefficients = iRight.m_Coefficients;
      }
    return *this;
  }

  Self operator+(const Self & iRight) const
  {
    return Self(this->m_Coefficients + iRight.m_Coefficients);
  }

  Self & operator+=(const Self & iRight)
  {
    this->m_Coefficients += iRight.m_Coefficients;
    return *this;
  }

  Self operator-(const Self & iRight) const
  {
    return Self(this->m_Coefficients - iRight.m_Coefficients);
  }

  Self & operator-=(const Self & iRight)
  {
    this->m_Coefficients -= iRight.m_Coefficients;
    return *this;
  }

  Self operator*(const CoordType & iV) const
  {
    Self oElement = Self(this->m_Coefficients * iV);

    return oElement;
  }

  Self & operator*=(const CoordType & iV)
  {
    this->m_Coefficients *= iV;
    return *this;
  }

protected:

  CoefficientVectorType m_Coefficients;
  VNLMatrixType         m_A;
  VNLVectorType         m_B;
  unsigned int          m_Rank;
  CoordType             m_SVDAbsoluteThreshold;
  CoordType             m_SVDRelativeThreshold;
  //bool                        m_MatrixFilled;

  void ComputeAMatrixAndBVector()
  {
    unsigned int k(0), dim1, dim2;

    for ( dim1 = 0; dim1 < PointDimension; ++dim1 )
      {
      for ( dim2 = dim1; dim2 < PointDimension; ++dim2 )
        {
        m_A[dim1][dim2] = m_A[dim2][dim1] = m_Coefficients[k++];
        }
      m_B[dim1] = -m_Coefficients[k++];
      }
    //m_MatrixFilled = true;
  }
};
}
#endif
