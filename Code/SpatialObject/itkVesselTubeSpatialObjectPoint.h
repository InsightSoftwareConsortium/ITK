/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVesselTubeSpatialObjectPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVesselTubeSpatialObjectPoint_h
#define __itkVesselTubeSpatialObjectPoint_h

#include "itkTubeSpatialObjectPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk 
{

/** \class VesselTubeSpatialObjectPoint
* \brief Point used for a tube definition
*
* This class contains all the functions necessary to define a point
* that can be used to build tubes.
*
* \sa VesselTubeSpatialObject 
*/ 
template < unsigned int TPointDimension = 3 >
class VesselTubeSpatialObjectPoint 
  : public TubeSpatialObjectPoint<TPointDimension>
{

public:

  typedef VesselTubeSpatialObjectPoint                 Self;
  typedef TubeSpatialObjectPoint<TPointDimension>      Superclass;
  typedef Point< double, TPointDimension >             PointType;
  typedef Vector<double, TPointDimension >             VectorType;
  typedef CovariantVector<double, TPointDimension >    CovariantVectorType;
 
  /** Constructor. This one defines the # of dimensions in the 
   * VesselTubeSpatialObjectPoint */
  VesselTubeSpatialObjectPoint( void );

  /** Default destructor. */
  virtual ~VesselTubeSpatialObjectPoint( void );

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

  /** Copy one VesselTubeSpatialObjectPoint to another */
  Self & operator=(const VesselTubeSpatialObjectPoint & rhs);

protected:

  /** First of 3 alpha values */
  float m_Alpha1;

  /** Second of 3 alpha values */
  float m_Alpha2;

  /** Third of 3 alpha values */
  float m_Alpha3;

  /** The medialness of the tube point */
  float m_Medialness;

  /** The ridgeness of the tube point */
  float m_Ridgeness;

  /** The branchness of the tube point */
  float m_Branchness;

  /** Is the tube point marked (selected) ? */
  bool m_Mark;

  /** Print the object */
  void PrintSelf( std::ostream & os, Indent indent) const;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVesselTubeSpatialObjectPoint.txx"
#endif

#endif // __itkVesselTubeSpatialObjectPoint_h
