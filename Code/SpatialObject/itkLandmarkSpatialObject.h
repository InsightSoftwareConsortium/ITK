/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLandmarkSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLandmarkSpatialObject_h
#define __itkLandmarkSpatialObject_h

#include <list>

#include "itkSpatialObject.h"
#include "itkSpatialObjectPoint.h"

namespace itk 
{

/** 
* \class LandmarkSpatialObject
* \brief Representation of a Landmark based on the spatial object classes.
*
* The Landmark is basically defined by a set of points which are inside this blob
*
* \also SpatialObjectPoint
*/

template < unsigned int TDimension = 3 >
class LandmarkSpatialObject 
:public SpatialObject<  TDimension >
{

public:

  typedef LandmarkSpatialObject                       Self;
  typedef SpatialObject< TDimension>                  Superclass;
  typedef SmartPointer < Self >                       Pointer;
  typedef SmartPointer < const Self >                 ConstPointer;
  typedef double                                      ScalarType;
  typedef SpatialObjectPoint< TDimension >            LandmarkPointType; 
  typedef std::list < LandmarkPointType >             PointListType;
  typedef typename Superclass::PointType              PointType;
  typedef VectorContainer<unsigned long,PointType>    PointContainerType;
  typedef SmartPointer<PointContainerType>            PointContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Method for creation through the object factory. */
  itkTypeMacro( Self, Superclass );
  
  /** Returns a reference to the list of the Landmark points. */
  PointListType & GetPoints( void );

 /** Returns a reference to the list of the Landmark points. */
  const PointListType & GetPoints( void ) const;

  /** Set the list of Landmark points.*/
  void SetPoints( PointListType & newPoints );

  /** Returns true if the Landmark is evaluable at the requested point, 
   *  false otherwise. */
  bool IsEvaluableAt( const PointType & point, 
                      unsigned int depth=0, char *name = NULL ) const;

  /** Returns the value of the Landmark at that point.
   *  Currently this function returns a binary value,
   *  but it might want to return a degree of membership
   *  in case of fuzzy Landmarks. */
  bool ValueAt( const PointType & point, double & value,
                unsigned int depth=0, char *name = NULL ) const;

  /** Returns true if the point is inside the Landmark, false otherwise. */
  bool IsInside( const PointType & point,
                 unsigned int depth=0, char *name = NULL ) const;

  /** Compute the boundaries of the Landmark. */
  bool ComputeBoundingBox( unsigned int depth=0, char *name = NULL );

protected:

  PointListType   m_Points;

  LandmarkSpatialObject();
  virtual ~LandmarkSpatialObject();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkLandmarkSpatialObject.txx" 
#endif 

#endif // __itkLandmarkSpatialObject_h
