/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSurfaceSpatialObjectPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSurfaceSpatialObjectPoint_h
#define __itkSurfaceSpatialObjectPoint_h

#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"

namespace itk 
{

/** \class SurfaceSpatialObjectPoint
* \brief Point used for a Surface definition
*
* This class contains all the functions necessary to define a point
* that can be used to build surfaces.
* A surface point has a position and only one normal
*
* \also SpatialObjectPoint 
*/ 
template < unsigned int TPointDimension = 3 >
class SurfaceSpatialObjectPoint 
: public SpatialObjectPoint<TPointDimension>
{

public:

  typedef SurfaceSpatialObjectPoint                Self;
  typedef SpatialObjectPoint<TPointDimension>      Superclass;
  typedef Point< double, TPointDimension >         PointType;
  typedef CovariantVector<double,TPointDimension>  VectorType;

  /** Constructor. This one defines the # of dimensions in the SurfaceSpatialObjectPoint */
  SurfaceSpatialObjectPoint( void );

  /** Default destructor. */
  virtual ~SurfaceSpatialObjectPoint( void );

  /** Get Normal */
  const VectorType & GetNormal( void ) const;

  /** Set Normal */
  void SetNormal(const VectorType & normal);
  void SetNormal(const double normalx, const double normaly);
  void SetNormal(const double normalx, const double normaly, const double normalz);

  /** Copy one SurfaceSpatialObjectPoint to another */
  Self & operator=(const SurfaceSpatialObjectPoint & rhs);

protected:

  VectorType m_Normal;
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkSurfaceSpatialObjectPoint.txx"
#endif

#endif // __itkSurfaceSpatialObjectPoint_h
