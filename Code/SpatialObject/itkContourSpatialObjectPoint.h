/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContourSpatialObjectPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkContourSpatialObjectPoint_h
#define __itkContourSpatialObjectPoint_h

#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"

namespace itk 
{

/** \class ContourSpatialObjectPoint
* \brief Point used for a Contour definition
*
* This class contains all the functions necessary to define a point
* that can be used to build surfaces.
* A surface point has a position and only one normal
*
* \sa SpatialObjectPoint 
*/ 
template < unsigned int TPointDimension = 3 >
class ContourSpatialObjectPoint 
  : public SpatialObjectPoint<TPointDimension>
{

public:

  typedef ContourSpatialObjectPoint                Self;
  typedef SpatialObjectPoint<TPointDimension>      Superclass;
  typedef Point< double, TPointDimension >         PointType;
  typedef CovariantVector<double,TPointDimension>  VectorType;

  /** Constructor. This one defines the # of dimensions in the ContourSpatialObjectPoint */
  ContourSpatialObjectPoint( void );

  /** Default destructor. */
  virtual ~ContourSpatialObjectPoint( void );

  /** Get Picked Point */
  const PointType & GetPickedPoint( void ) const;

  /** Set Picked Point */
  void SetPickedPoint(const PointType & point);
  void SetPickedPoint(const double pointx, const double pointy);
  void SetPickedPoint(const double pointx, const double pointy, const double pointz);

  /** Get Normal */
  const VectorType & GetNormal( void ) const;

  /** Set Normal */
  void SetNormal(const VectorType & normal);
  void SetNormal(const double normalx, const double normaly);
  void SetNormal(const double normalx, const double normaly, const double normalz);

  /** Copy one ContourSpatialObjectPoint to another */
  Self & operator=(const ContourSpatialObjectPoint & rhs);

protected:

  VectorType m_Normal;
  PointType  m_PickedPoint;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkContourSpatialObjectPoint.txx"
#endif

#endif // __itkContourSpatialObjectPoint_h
