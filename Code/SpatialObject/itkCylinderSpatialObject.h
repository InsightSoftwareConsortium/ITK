/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCylinderSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCylinderSpatialObject_h
#define __itkCylinderSpatialObject_h

#include "itkSpatialObject.h"
#include "itkAffineTransform.h"
#include "itkFixedArray.h"

namespace itk
{

/** \class CylinderSpatialObject
 * 
 * \brief This class describe a cylinder in 3D only.
 */
class CylinderSpatialObject 
  : public SpatialObject< 3 >
{

public:

  typedef CylinderSpatialObject Self;
  typedef double ScalarType;
  typedef SmartPointer < Self > Pointer;
  typedef SmartPointer < const Self > ConstPointer;
  typedef SpatialObject< 3 > Superclass;
  typedef SmartPointer<Superclass> SuperclassPointer;
  typedef Superclass::PointType              PointType;
  typedef Superclass::TransformType          TransformType;
  typedef Superclass::BoundingBoxType        BoundingBoxType;
  typedef VectorContainer<unsigned long,PointType>    PointContainerType;
  typedef SmartPointer<PointContainerType>            PointContainerPointer;

  itkStaticConstMacro(NumberOfDimension, unsigned int,
                      3);

  itkNewMacro( Self );
  itkTypeMacro( CylinderSpatialObject, SpatialObject );


  /** Set/Get the radius */
  itkSetMacro(Radius,double);
  itkGetConstReferenceMacro(Radius,double);

  /** Set/Get the height */
  itkSetMacro(Height,double);
  itkGetConstReferenceMacro(Height,double);

  /** Returns a degree of membership to the object. 
   *  That's useful for fuzzy objects. */ 
  virtual bool ValueAt( const PointType & point, double & value, 
                        unsigned int depth=0,
                        char * name=NULL) const;
     
  /** Return true if the object provides a method to evaluate the value 
   * at the specified point, false otherwise.*/
  virtual bool IsEvaluableAt( const PointType & point, 
                              unsigned int depth=0,
                              char * name=NULL) const;

  /** Test whether a point is inside or outside the object */ 
  virtual bool IsInside( const PointType & point,
                         unsigned int depth,
                         char *) const;

  /** Test whether a point is inside or outside the object 
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */ 
  virtual bool IsInside( const PointType & point) const;

  /** Get the boundaries of a specific object.  This function needs to
   *  be called every time one of the object's components is
   *  changed. */ 
  virtual bool ComputeLocalBoundingBox() const;

protected:
  CylinderSpatialObject(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  CylinderSpatialObject( void );
  ~CylinderSpatialObject( void );

  double m_Radius;
  double m_Height;

  /** Print the object informations in a stream. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const; 

};

} // end namespace itk

#endif // __itkCylinderSpatialObject_h
