/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleHelper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTriangleHelper_h
#define __itkTriangleHelper_h

#include "itkCrossHelper.h"

namespace itk
{
  /** \class Triangle
   * \brief Convenient class for various triangles elements computation in
   * 2D or 3D
   * \author Arnaud GELAS
   */
template< typename TPoint >
class TriangleHelper
{
public:
  typedef TriangleHelper Self;
  typedef TPoint PointType;
  typedef typename PointType::CoordRepType CoordRepType;
  typedef typename PointType::VectorType VectorType;
  typedef CrossHelper< VectorType > CrossVectorType;

  itkStaticConstMacro ( PointDimension, unsigned int, PointType::PointDimension );

  static bool IsObtuse( const PointType& iA,
    const PointType& iB,
    const PointType& iC )
  {
    VectorType v01 = iB - iA;
    VectorType v02 = iC - iA;
    VectorType v12 = iC - iB;

    if( v01 * v02 < 0. )
      return true;
    else
      {
      if( v02 * v12 < 0. )
        return true;
      else
        {
        if( v01 * -v12 < 0. )
          return true;
        else
          return false;
        }
      }
  }
  
  static VectorType ComputeNormal ( const PointType& iA,
    const PointType& iB,
    const PointType& iC )
  {
    CrossVectorType cross;
    VectorType w = cross ( iB - iA, iC - iA );
    CoordRepType l2 = w.GetSquaredNorm();

    if( l2 != 0. )
      w /= vcl_sqrt( l2 );

    return w;
  }

  static CoordRepType Cotangent ( const PointType& iA,
                                  const PointType& iB,
                                  const PointType& iC )
  {
    VectorType v21 = iA - iB;
    v21.Normalize();
    
    VectorType v23 = iC - iB;
    v23.Normalize();
    
    CoordRepType bound( 0.999999 );

    CoordRepType cos_theta = vnl_math_max( -bound, 
      vnl_math_min( bound, v21 * v23 ) );
            
    return 1. / vcl_tan( vcl_acos( cos_theta ) );
  }

  static PointType ComputeBarycenter (
      const CoordRepType& iA1, const PointType& iP1,
      const CoordRepType& iA2, const PointType& iP2,
      const CoordRepType& iA3, const PointType& iP3 )
  {
    PointType oPt;

    for ( unsigned int dim = 0; dim < PointDimension; dim++ )
      oPt[dim] = iA1 * iP1[dim] + iA2 * iP2[dim] + iA3 * iP3[dim];

    return oPt;
  }

  static CoordRepType ComputeAngle( const PointType& iP1, const PointType& iP2,
      const PointType& iP3 )
  {
    VectorType v21 = iP1 - iP2;
    VectorType v23 = iP3 - iP2;

    CoordRepType v21_l2 = v21.GetSquaredNorm();
    CoordRepType v23_l2 = v23.GetSquaredNorm();

    if( v21_l2 != 0. )
      v21 /= v21_l2;
    if( v23_l2 != 0. )
      v23 /= v23_l2;

    CoordRepType bound( 0.999999 );

    CoordRepType cos_theta = vnl_math_max( -bound, 
      vnl_math_min( bound, v21 * v23 ) );
 
    return vcl_acos( cos_theta );
  }

  static PointType ComputeGravityCenter (
      const PointType& iP1,
      const PointType& iP2,
      const PointType& iP3 )
  {
    PointType oPt;
    CoordRepType inv_3 = 1. / 3.;

    for ( unsigned int dim = 0; dim < PointDimension; dim++ )
      oPt[dim] = ( iP1[dim] + iP2[dim] + iP3[dim] ) * inv_3;

    return oPt;
  }

  static PointType ComputeCircumCenter (
      const PointType& iP1,
      const PointType& iP2,
      const PointType& iP3 )
  {
    PointType oPt;
    oPt.Fill ( 0. );

    CoordRepType a = iP2.SquaredEuclideanDistanceTo ( iP3 );
    CoordRepType b = iP1.SquaredEuclideanDistanceTo ( iP3 );
    CoordRepType c = iP2.SquaredEuclideanDistanceTo ( iP1 );

    CoordRepType Weight[3];
    Weight[0] = a * ( b + c - a );
    Weight[1] = b * ( c + a - b );
    Weight[2] = c * ( a + b - c );

    CoordRepType SumWeight = Weight[0] + Weight[1] + Weight[2];

    if ( SumWeight != 0. )
      {
      SumWeight = 1. / SumWeight;

      for ( unsigned int dim = 0; dim < PointDimension; dim++ )
        oPt[dim] = ( Weight[0] * iP1[dim] +
            Weight[1] * iP2[dim] + Weight[2] * iP3[dim] ) * SumWeight;
      }


    return oPt;
  }

  static PointType ComputeConstrainedCircumCenter ( const PointType& iP1,
      const PointType& iP2, const PointType& iP3 )
  {
    PointType oPt;
    CoordRepType a = iP2.SquaredEuclideanDistanceTo ( iP3 );
    CoordRepType b = iP1.SquaredEuclideanDistanceTo ( iP3 );
    CoordRepType c = iP2.SquaredEuclideanDistanceTo ( iP1 );

    CoordRepType Weight[3];
    Weight[0] = a * ( b + c - a );
    Weight[1] = b * ( c + a - b );
    Weight[2] = c * ( a + b - c );

    for ( unsigned int i = 0; i < 3; i++ )
      {
      if ( Weight[i] < 0. )
        Weight[i] = 0.;
      }

    CoordRepType SumWeight = Weight[0] + Weight[1] + Weight[2];

    if ( SumWeight != 0. )
      {
      SumWeight = 1. / SumWeight;

      for ( unsigned int dim = 0; dim < PointDimension; dim++ )
        oPt[dim] = ( Weight[0] * iP1[dim] +
          Weight[1] * iP2[dim] + Weight[2] * iP3[dim] ) * SumWeight;
      }

    return oPt;
  }

  static CoordRepType ComputeArea ( const PointType& iP1,
      const PointType& iP2,
      const PointType& iP3 )
  {
    CoordRepType a = iP2.EuclideanDistanceTo ( iP3 );
    CoordRepType b = iP1.EuclideanDistanceTo ( iP3 );
    CoordRepType c = iP2.EuclideanDistanceTo ( iP1 );

    CoordRepType s = 0.5 * ( a + b + c );
    return static_cast< CoordRepType > (
        vcl_sqrt ( s * ( s - a ) * ( s - b ) * ( s - c ) ) );
  }

protected:
  TriangleHelper( );
  virtual ~TriangleHelper( );

  void PrintSelf ( std::ostream& os, Indent indent ) const;


private:
  TriangleHelper( const Self& );
  void operator = ( const Self& );
};
}

#endif
