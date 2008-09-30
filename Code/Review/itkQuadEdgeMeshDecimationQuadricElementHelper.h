#ifndef __itkQuadEdgeMeshDecimationQuadricElementHelper_h
#define __itkQuadEdgeMeshDecimationQuadricElementHelper_h

#include <itkPoint.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_matrix_inverse.h>

#include "itkTriangleHelper.h"

namespace itk
{
  ///TODO explicit specification for VDimension=3!!!
  template< class TPoint >
  class QuadEdgeMeshDecimationQuadricElementHelper
  {
  public:
    typedef QuadEdgeMeshDecimationQuadricElementHelper Self;
    typedef TPoint PointType;
    typedef typename PointType::CoordRepType CoordType;

    itkStaticConstMacro(PointDimension, unsigned int, PointType::PointDimension);
    itkStaticConstMacro(NumberOfCoefficients, unsigned int,
      PointDimension * ( PointDimension + 1 ) / 2 + PointDimension + 1);

//    static const unsigned int PointDimension = PointType::PointDimension;
//    static const unsigned int NumberOfCoefficients = 
//      PointDimension * ( PointDimension + 1 ) / 2 + PointDimension + 1;
    
    typedef typename PointType::VectorType VectorType;
    typedef vnl_matrix< CoordType > VNLMatrixType;
    typedef vnl_vector_fixed< CoordType,
      itkGetStaticConstMacro(PointDimension) >  VNLVectorType;
    typedef vnl_vector_fixed< CoordType,
      itkGetStaticConstMacro(NumberOfCoefficients) > CoefficientVectorType;
    typedef TriangleHelper< PointType > TriangleType;
    
    // *****************************************************************
    QuadEdgeMeshDecimationQuadricElementHelper() : m_Coefficients( 0. ),
      m_A( PointDimension, PointDimension, 0. ),
      m_b( 0. ),
      m_Rank( PointDimension ) {}
      
    QuadEdgeMeshDecimationQuadricElementHelper(
      const CoefficientVectorType& iCoefficients ) : 
      m_Coefficients( iCoefficients ), 
      m_A( PointDimension, PointDimension, 0. ),
      m_b( 0. ),
      m_Rank( PointDimension )
    {
      ComputeAMatrixAndBVector();
    }
    ~QuadEdgeMeshDecimationQuadricElementHelper()
    {}
    
    CoefficientVectorType GetCoefficients( ) const
    {
      return m_Coefficients;
    }
    
    VNLMatrixType GetAMatrix()
    {
      ComputeAMatrixAndBVector();
      return m_A;
    }
    
    VNLVectorType GetBVector()
    {
      ComputeAMatrixAndBVector();
      return m_b;
    }
    
    unsigned int GetRank() const
    {
      return m_Rank;
    }
    
    ///TODO this method should be really optimized!!!
    inline CoordType ComputeError( const PointType& iP )
    {
      CoordType oError( 0. );
      
      std::vector< CoordType > pt( PointDimension + 1, 1. );
      unsigned int dim1( 0 ), dim2, k( 0 );
      
      for(; dim1 < PointDimension; dim1++ )
        pt[dim1] = iP[dim1];
      
//       typename std::vector< CoordType >::iterator p_it1 = pt.begin();
//       typename std::vector< CoordType >::iterator p_it2 = p_it1;
      
      for( dim1 = 0; dim1 < PointDimension + 1; dim1++ )
      {
        oError += m_Coefficients[k++] * pt[dim1] * pt[dim1];
        
        for( dim2 = dim1 + 1; dim2 < PointDimension + 1; dim2++ )
        {
          oError += 2. * m_Coefficients[k++] * pt[dim1] * pt[dim2];
        }
      }
      return oError;
    }
    
    ///TODO this method should be really optimized!!!
    inline CoordType ComputeErrorAtOptimalLocation()
    {
      return ComputeError( ComputeOptimalLocation() );
    }
    
    PointType ComputeOptimalLocation()
    {
      ComputeAMatrixAndBVector();
      vnl_svd< CoordType > svd( m_A );
      svd.zero_out_relative( 1e-10 );
      m_Rank = svd.rank();
      
      VNLVectorType location = svd.solve( m_b.as_vector() );
      PointType oP;
      
      for( unsigned int dim = 0; dim < PointDimension; dim++ )
        oP[dim] = location[dim];
      
      return oP;
    }
    
    ///TODO to be implemented!!!
    PointType ComputeOptimalLocation( 
      const unsigned int& iNumberOfEigenValues )
    {
    }

    PointType ComputeOptimalLocation( 
      const CoordType& iValue )
    {
      ComputeAMatrixAndBVector();
      vnl_svd< CoordType > svd( m_A );
      svd.zero.zero_out_relative( iValue );
      m_Rank = svd.rank();
      
      VNLVectorType location = svd.solve( m_b );
      PointType oP;
      
      for( unsigned int dim = 0; dim < PointDimension; dim++ )
        oP[dim] = location[dim];
      
      return oP;
    }

    void AddTriangle( const PointType& iP1, 
                      const PointType& iP2, 
                      const PointType& iP3,
                      const CoordType& iWeight = static_cast< CoordType >( 1. ) ) 
    {
      AddPoint( iP1, TriangleType::ComputeNormal( iP1, iP2, iP3 ), iWeight );
    }
    
    void AddPoint( const PointType& iP, 
                   const VectorType& iN, 
                   const CoordType& iWeight = static_cast< CoordType >( 1. ) )
    {
      unsigned int dim1( 0 ), dim2, k( 0 );
      
      CoordType d = -iN * iP.GetVectorFromOrigin();
      
      for( ; dim1 < PointDimension; dim1++ )
      {
        for( dim2 = dim1; dim2 < PointDimension; dim2++ )
        {
          m_Coefficients[k++] += iWeight * iN[dim1] * iN[dim2];
        }
        m_Coefficients[k++] += iWeight * iN[dim1] * d;
      }
      
      m_Coefficients[k++] += iWeight * d * d;
    }
    
    
    
    
    // ***********************************************************************
    // operators
    Self& operator=( const Self& iRight )
    {
      m_Coefficients = iRight.m_Coefficients;
      return *this;
    }
    
    Self operator+( const Self & iRight ) const
    {
      return Self( m_Coefficients + iRight.m_Coefficients );
    }
    Self& operator+=( const Self& iRight )
    {
      m_Coefficients += iRight.m_Coefficients;
      return *this;
    }
    Self operator-( const Self& iRight ) const
    {
      return Self( m_Coefficients - iRight.m_Coefficients );
    }
    Self& operator-=( const Self& iRight )
    {
      m_Coefficients -= iRight.m_Coefficients;
      return *this;
    }
    Self operator*( const CoordType& iV ) const
    {
      Self oElement = Self( m_Coefficients * iV );
      return oElement;
    }
    Self& operator*=( const CoordType& iV )
    {
      m_Coefficients *= iV;
      return *this;
    }
    
  protected:
    CoefficientVectorType m_Coefficients;
    VNLMatrixType m_A;
    VNLVectorType m_b;
    unsigned int m_Rank;
    
    void ComputeAMatrixAndBVector( )
    {
      unsigned int dim1( 0 ), dim2, k( 0 );
      
      for( ; dim1 < PointDimension; dim1++ )
      {
        for( dim2 = dim1; dim2 < PointDimension; dim2++ )
        {
          m_A[dim1][dim2] = m_A[dim2][dim1] = m_Coefficients[k++];
        }
        m_b[dim1] = - m_Coefficients[k++];
      }
    }
    
  };
  
}
#endif
