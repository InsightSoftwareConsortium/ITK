/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipseSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkEllipseSpatialObject_h
#define __itkEllipseSpatialObject_h

#include "itkAffineTransform.h"
#include "itkSpatialObject.h"
#include "itkFixedArray.h"

namespace itk
{

/** \class EllipseSpatialObject
 * 
 * \brief 
 */

template < unsigned int NDimensions = 3 , unsigned int SpaceDimension = 3  >
class EllipseSpatialObject 
: public SpatialObject< NDimensions, SpaceDimension >
{

public:

  typedef EllipseSpatialObject Self;
  typedef double ScalarType;
  typedef SmartPointer < Self > Pointer;
  typedef SmartPointer < const Self > ConstPointer;
  typedef SpatialObject< NDimensions, SpaceDimension > Superclass;
  typedef SmartPointer<Superclass> SuperclassPointer;
  typedef typename Superclass::PointType              PointType;
  typedef VectorContainer<unsigned long,PointType>    PointContainerType;
  typedef SmartPointer<PointContainerType>            PointContainerPointer;

  typedef FixedArray<double,NDimensions> ArrayType;
  itkStaticConstMacro(NumberOfDimension, unsigned int,
                      NDimensions);

  itkNewMacro( Self );
  itkTypeMacro( Self, Superclass );

  /** Set all radii to the same radius value */
  void SetRadius(double radius);

  /** Set raddi via an array of radius */
  itkSetMacro(Radius,ArrayType);
  /** Get raddi via an array of radius */
  itkGetMacro(Radius,ArrayType);

  /** Returns a degree of membership to the object. 
   *  That's useful for fuzzy objects. */ 
  virtual void ValueAt( const PointType & point, double & value, 
                        unsigned int depth=MaximumDepth,
                        char * name=NULL);
     
  /** return ture if the object provides a method to evaluate the value 
   * at the specified point, else otherwise.*/
  virtual bool IsEvaluableAt( const PointType & point, 
                              unsigned int depth=MaximumDepth,
                              char * name=NULL);

  /** Test whether a point is inside or outside the object */ 
  virtual bool IsInside( const PointType & point,
                         unsigned int depth=MaximumDepth,
                         char * name=NULL) const;

 /** provide a method to get the boundaries of 
  *  a specific object. Basically, this function need to be called
  *  every time one of the object component is changed. */ 
  virtual bool ComputeBoundingBox( unsigned int depth=MaximumDepth,
                                   char * name=NULL);

protected:

  EllipseSpatialObject( void );
  ~EllipseSpatialObject( void );

  ArrayType m_Radius;

  /** Print the object informations in a stream. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const; 

  bool IsInsideProjection(double x, double y, unsigned int i) const;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkEllipseSpatialObject.txx"
#endif

#endif // __itkEllipseSpatialObject_h
