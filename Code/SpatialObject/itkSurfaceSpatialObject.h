/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:  itkSurfaceSpatialObject.h
  Language:  C++
  Date:    $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

   This software is distributed WITHOUT ANY WARRANTY; without even 
   the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
   PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSurfaceSpatialObject_h
#define __itkSurfaceSpatialObject_h

#include <list>

#include "itkSpatialObject.h"
#include "itkSurfaceSpatialObjectPoint.h"

namespace itk 
{

/** 
* \class SurfaceSpatialObject
* \brief Representation of a Surface based on the spatial object classes.
*
* The Surface is basically defined by a set of points. 
*
* \also SurfaceSpatialObjectPoint
*/

template < unsigned int TDimension = 3 >
class SurfaceSpatialObject 
:public SpatialObject<  TDimension >
{

public:

  typedef SurfaceSpatialObject                          Self;
  typedef SpatialObject< TDimension >                   Superclass;
  typedef SmartPointer < Self >                         Pointer;
  typedef SmartPointer < const Self >                   ConstPointer;
  typedef double                                        ScalarType;
  typedef SurfaceSpatialObjectPoint< TDimension >       SurfacePointType;
  typedef std::list < SurfacePointType >                PointListType;
  typedef typename Superclass::PointType                PointType;
  typedef typename Superclass::TransformType            TransformType;
  typedef VectorContainer<unsigned long,PointType>      PointContainerType;
  typedef SmartPointer<PointContainerType>              PointContainerPointer;

  /** Method for creation through the object factory.*/
  itkNewMacro( Self );

  /** Method for creation through the object factory. */
  itkTypeMacro( Self, Superclass );
  
  /** Returns a reference to the list of the Surface points. */
  PointListType & GetPoints( void );

  /** Set the list of Surface points. */
  void SetPoints( PointListType & newPoints );

  /** Returns true if the Surface is evaluable at the requested point, 
   * false otherwise. */
  bool IsEvaluableAt( const PointType & point,
                      unsigned int depth=0, char * name=NULL) const;

  /** Returns the value of the Surface at that point.
   *  Currently this function returns a binary value,
   *  but it might want to return a degree of membership
   *  in case of fuzzy Surfaces. */
  bool ValueAt( const PointType & point, double & value,
                unsigned int depth=0, char * name=NULL) const;

  /** Returns true if the point is inside the Surface, false otherwise. */
  bool IsInside( const PointType & point,
                 unsigned int depth=0, char * name=NULL) const;

  /** Compute the boundaries of the Surface. */
  bool ComputeBoundingBox( unsigned int depth=0, char * name=NULL);

protected:

  PointListType  m_Points;

  SurfaceSpatialObject();
  virtual ~SurfaceSpatialObject();

  /** Method to print the object.*/
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkSurfaceSpatialObject.txx" 
#endif 

#endif // __itkSurfaceSpatialObject_h
