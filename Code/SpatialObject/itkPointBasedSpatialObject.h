/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointBasedSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkPointBasedSpatialObject_h
#define __itkPointBasedSpatialObject_h

#include "itkSpatialObject.h"
#include "itkSpatialObjectPoint.h"

namespace itk 
{

/** 
* \class PointBasedSpatialObject
* \brief This class serves as the base class for point-based spatial objects
*
* A PointBasedSpatialObject is an abstract class to support PointBasedSpatialObject
* filters and algorithms.
*
*/

template < unsigned int TDimension = 3 >
class PointBasedSpatialObject 
  :public SpatialObject< TDimension >
{

public:

  typedef PointBasedSpatialObject                      Self;
  typedef SpatialObject< TDimension >                  Superclass;
  typedef SmartPointer < Self >                        Pointer;
  typedef SmartPointer < const Self >                  ConstPointer;
  typedef double                                       ScalarType;
  typedef SpatialObjectPoint< TDimension >             SpatialObjectPointType;
  typedef typename Superclass::PointType               PointType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::VectorType              VectorType;
  typedef typename Superclass::CovariantVectorType     CovariantVectorType;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Method for creation through the object factory. */
  itkTypeMacro( PointBasedSpatialObject, SpatialObject );

  /** Return a SpatialObjectPoint given its position in the list */
  virtual const SpatialObjectPointType* 
    GetPoint(unsigned long itkNotUsed(id)) const
    {
    itkWarningMacro(<<"PointBasedSpatialObject::GetPoint() is not implemented"
                    <<" in the base class" << std::endl);
    return 0;
    }

  virtual SpatialObjectPointType* 
    GetPoint(unsigned long itkNotUsed(id)) 
    {
    itkWarningMacro(<<"PointBasedSpatialObject::GetPoint() is not implemented"
                    <<" in the base class" << std::endl);
    return 0;
    }

  /** Return the number of points in the list */
  virtual unsigned long GetNumberOfPoints(void) const
    {
    itkWarningMacro(<<"PointBasedSpatialObject::GetNumberOfPoints() is not"
                    <<" implemented in the base class" << std::endl);
    return 0;
    }

  /**  */
  bool ComputeLocalBoundingBox() const 
    {
    itkWarningMacro(<<"PointBasedSpatialObject::ComputeLocalBoundingBox() is"
                    <<" not implemented in the base class" << std::endl);
    return false;
    }

protected:
  PointBasedSpatialObject(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
   
  PointBasedSpatialObject();
  virtual ~PointBasedSpatialObject();

  /** Method to print the object.*/
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
#include "itkPointBasedSpatialObject.txx" 
#endif 

#endif // __itkPointBasedSpatialObject_h
