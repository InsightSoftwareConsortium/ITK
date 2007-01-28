/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeSpatialObjectPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkTubeSpatialObjectPoint_h
#define __itkTubeSpatialObjectPoint_h

#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk 
{

/** \class TubeSpatialObjectPoint
* \brief Point used for a tube definition
*
* This class contains all the functions necessary to define a point
* that can be used to build tubes.
*
* \sa TubeSpatialObject 
*/ 
template < unsigned int TPointDimension = 3 >
class TubeSpatialObjectPoint 
  : public SpatialObjectPoint<TPointDimension>
{

public:

  typedef TubeSpatialObjectPoint                    Self;
  typedef SpatialObjectPoint<TPointDimension>       Superclass;
  typedef Point< double, TPointDimension >          PointType;
  typedef Vector<double, TPointDimension >          VectorType;
  typedef CovariantVector<double, TPointDimension > CovariantVectorType;
 
  /** Constructor. This one defines the # of dimensions in the 
   * TubeSpatialObjectPoint */
  TubeSpatialObjectPoint( void );

  /** Default destructor. */
  virtual ~TubeSpatialObjectPoint( void );

  /** Get the tangent */
  const VectorType & GetTangent( void ) const;

  /** Set T. Couldn't use macros for these methods */
  void SetTangent(const VectorType & newT);
  void SetTangent(const double t0, const double t1);
  void SetTangent(const double t0, const double t1, const double t2);

  /** Get V1 */
  const CovariantVectorType & GetNormal1( void ) const;

  /** Set V1 */
  void SetNormal1(const CovariantVectorType & newV1);
  void SetNormal1(const double v10, const double v11);
  void SetNormal1(const double v10, const double v11, const double v12);

  /** Get V2 */
  const CovariantVectorType & GetNormal2( void ) const;

  /** Set V2 */
  void SetNormal2(const CovariantVectorType & newV2);
  void SetNormal2(const double v20, const double v21);
  void SetNormal2(const double v20, const double v21, const double v22);

  /** Get R */
  float GetRadius( void ) const;

  /** Set R */
  void SetRadius(const float newR);

  /** Get # of dimensions */
  unsigned short int GetNumDimensions( void ) const;

  /** Copy one TubeSpatialObjectPoint to another */
  Self & operator=(const TubeSpatialObjectPoint & rhs);

protected:

  VectorType          m_T;
  CovariantVectorType m_Normal1;
  CovariantVectorType m_Normal2;

  /** The radius of the tube point */
  float m_R;

  /** # of dimensions */
  unsigned short int m_NumDimensions;

  /** Print the object */
  void PrintSelf( std::ostream & os, Indent indent) const;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTubeSpatialObjectPoint.txx"
#endif

#endif // __itkTubeSpatialObjectPoint_h
