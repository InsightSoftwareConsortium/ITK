/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeSpatialObjectPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkTubeSpatialObjectPoint_h
#define __itkTubeSpatialObjectPoint_h

#include "itkSpatialObjectPoint.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk 
{

/** \class TubeSpatialObjectPoint
* \brief Point used for a tube definition
*
* This class contains all the functions necessary to define a point
* that can be used to build tubes.
*
* \also TubeSpatialObject TubeNetworkSpatialObject
*/ 
template < unsigned int TPointDimension = 3 >
class TubeSpatialObjectPoint 
: public SpatialObjectPoint<TPointDimension>
{

public:

  typedef TubeSpatialObjectPoint              Self;
  typedef SpatialObjectPoint<TPointDimension> Superclass;
  typedef Point< double, TPointDimension >    PointType;
  typedef Vector<double, TPointDimension >    VectorType;
 
  /** Constructor. This one defines the # of dimensions in the TubeSpatialObjectPoint */
  TubeSpatialObjectPoint( void );

  /** Default destructor. */
  ~TubeSpatialObjectPoint( void );

  /** Get the tangent */
  const VectorType & GetTangent( void ) const;

  /** Set T. Couldn't use macros for these methods */
  void SetTangent(const VectorType & newT);
  void SetTangent(const double t0, const double t1);
  void SetTangent(const double t0, const double t1, const double t2);

  /** Get V1 */
  const VectorType & GetV1( void ) const;

  /** Set V1 */
  void SetV1(const VectorType & newV1);
  void SetV1(const double v10, const double v11);
  void SetV1(const double v10, const double v11, const double v12);

  /** Get V2 */
   const VectorType & GetV2( void ) const;

  /** Set V2 */
  void SetV2(const VectorType & newV2);
  void SetV2(const double v20, const double v21);
  void SetV2(const double v20, const double v21, const double v22);

  /** Get R */
  float GetRadius( void ) const;

  /** Set R */
  void SetRadius(const float newR);

  /** Get Medialness */
  float GetMedialness( void ) const;

  /** Set Medialness */
  void SetMedialness(const float newMedialness);

  /** Get Ridgeness */
  float GetRidgeness( void ) const;

  /** Set Ridgeness */
  void SetRidgeness(const float newRidgeness);

  /** Get Branchness */
  float GetBranchness( void ) const;

  /** Set Branchness */
  void SetBranchness(const float newBranchness);

  /** Get Mark */
  bool GetMark( void ) const;

  /** Set Mark */
  void SetMark(const bool newMark);

  /** Get Alpha1 */
  float GetAlpha1( void ) const;

  /** Set Alpha1 */
  void SetAlpha1(const float newAlpha);

  /** Get Alpha2 */
  float GetAlpha2( void ) const;

  /** Set Alpha2 */
  void SetAlpha2(const float newAlpha);

  /** Get Alpha3 */
  float GetAlpha3( void ) const;

  /** Set Alpha3 */
  void SetAlpha3(const float newAlpha);

  /** Get # of dimensions */
  unsigned short int GetNumDimensions( void ) const;

  /** Copy one TubeSpatialObjectPoint to another */
  Self & operator=(const TubeSpatialObjectPoint & rhs);

protected:

  /** A unique ID assigned to this TubeSpatialObjectPoint */
  unsigned int m_ID;

  VectorType m_T;
  VectorType m_V1;
  VectorType m_V2;

  /** First of 3 alpha values */
  float m_Alpha1;

  /** Second of 3 alpha values */
  float m_Alpha2;

  /** Third of 3 alpha values */
  float m_Alpha3;

  /** The radius of the tube point */
  float m_R;

  /** The medialness of the tube point */
  float m_Medialness;

  /** The ridgeness of the tube point */
  float m_Ridgeness;

  /** The branchness of the tube point */
  float m_Branchness;

  /** Is the tube point marked (selected) ? */
  bool m_Mark;

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
