/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLineSpatialObject_h
#define __itkLineSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"
#include "itkLineSpatialObjectPoint.h"

namespace itk 
{

/** 
* \class LineSpatialObject
* \brief Representation of a Line based on the spatial object classes.
*
* The Line is basically defined by a set of points. 
*
* \sa LineSpatialObjectPoint 
*/

template < unsigned int TDimension = 3 >
class LineSpatialObject 
  :public PointBasedSpatialObject<  TDimension >
{

public:

  typedef LineSpatialObject                           Self;
  typedef PointBasedSpatialObject< TDimension >       Superclass;
  typedef SmartPointer < Self >                       Pointer;
  typedef SmartPointer < const Self >                 ConstPointer;
  typedef double                                      ScalarType;
  typedef LineSpatialObjectPoint< TDimension >        LinePointType;
  typedef std::vector< LinePointType >                PointListType;
  typedef typename Superclass::SpatialObjectPointType SpatialObjectPointType;
  typedef typename Superclass::PointType              PointType;
  typedef typename Superclass::TransformType          TransformType;
  typedef typename Superclass::BoundingBoxType        BoundingBoxType;
  typedef VectorContainer<unsigned long,PointType>    PointContainerType;
  typedef SmartPointer<PointContainerType>            PointContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Method for creation through the object factory. */
  itkTypeMacro( LineSpatialObject, PointBasedSpatialObject );
  
  /** Returns a reference to the list of the Line points.*/
  PointListType & GetPoints( void );

  /** Set the list of line points. */
  void SetPoints( PointListType & newPoints );

  /** Return a point in the list given the index */
  const SpatialObjectPointType* GetPoint(unsigned long id) const {return &(m_Points[id]);}

  /** Return a point in the list given the index */
  SpatialObjectPointType* GetPoint(unsigned long id) {return &(m_Points[id]);}
  
  /** Return the number of points in the list */
  unsigned long GetNumberOfPoints(void) const {return m_Points.size();}

  /** Returns true if the line is evaluable at the requested point, 
   *  false otherwise. */
  bool IsEvaluableAt( const PointType & point, 
                      unsigned int depth=0, char * name=NULL ) const;

  /** Returns the value of the line at that point.
   * Currently this function returns a binary value,
   * but it might want to return a degree of membership
   * in case of fuzzy Lines. */
  bool ValueAt( const PointType & point, double & value,
                unsigned int depth=0, char * name=NULL ) const;

  /** Returns true if the point is inside the line, false otherwise. */
  bool IsInside( const PointType & point, 
                 unsigned int depth, char * name) const;

  /** Test whether a point is inside or outside the object 
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */ 
  virtual bool IsInside( const PointType & point) const;


  /** Compute the boundaries of the line.*/
  bool ComputeLocalBoundingBox() const;

protected:
  LineSpatialObject(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
   
  PointListType   m_Points;

  LineSpatialObject();
  virtual ~LineSpatialObject();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
#include "itkLineSpatialObject.txx" 
#endif 

#endif // __itkLineSpatialObject_h
