/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaneSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkPlaneSpatialObject_h
#define __itkPlaneSpatialObject_h

#include "itkSpatialObject.h"
#include "itkAffineTransform.h"


namespace itk
{

/** \class PlaneSpatialObject
 * A plane spatial object is defined by two points
 * \brief 
 */

template < unsigned int NDimensions = 3 , unsigned int SpaceDimension = 3  >
class PlaneSpatialObject 
: public SpatialObject< NDimensions, SpaceDimension >
{

public:

  typedef PlaneSpatialObject Self;
  typedef double ScalarType;
  typedef SmartPointer < Self > Pointer;
  typedef SmartPointer < const Self > ConstPointer;
  typedef SpatialObject< NDimensions, SpaceDimension > Superclass;
  typedef SmartPointer<Superclass> SuperclassPointer;
  typedef typename Superclass::PointType              PointType;
  typedef VectorContainer<unsigned long,PointType>    PointContainerType;
  typedef SmartPointer<PointContainerType>            PointContainerPointer;


  itkStaticConstMacro(NumberOfDimension, unsigned int,
                      NDimensions);

  itkNewMacro( Self );
  itkTypeMacro( Self, Superclass );

  /** Returns a degree of membership to the object. 
   *  That's useful for fuzzy objects. */ 
  virtual bool ValueAt( const PointType & point, double & value,
                        unsigned int depth=0, char * name=NULL) const;
     
  /** return ture if the object provides a method to evaluate the value 
   * at the specified point, else otherwise.*/
  virtual bool IsEvaluableAt( const PointType & point,
                              unsigned int depth=0, char * name=NULL) const;

  /** Test whether a point is inside or outside the object */ 
  virtual bool IsInside( const PointType & point,
                         unsigned int depth=0, char * name=NULL) const;

 /** provide a method to get the boundaries of 
  *  a specific object. Basically, this function need to be called
  *  every time one of the object component is changed. */ 
  virtual bool ComputeBoundingBox( unsigned int depth=0, char * name=NULL);

  itkSetMacro(LowerPoint,PointType);
  itkSetMacro(UpperPoint,PointType);
  itkGetMacro(LowerPoint,PointType);
  itkGetMacro(UpperPoint,PointType);

protected:

  PlaneSpatialObject( void );
  ~PlaneSpatialObject( void );

  PointType m_LowerPoint;
  PointType m_UpperPoint;

  /** Print the object informations in a stream. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const; 

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkPlaneSpatialObject.txx"
#endif

#endif // __itkPlaneSpatialObject_h
