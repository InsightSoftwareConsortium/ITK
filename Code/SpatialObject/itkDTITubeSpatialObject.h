/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDTITubeSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDTITubeSpatialObject_h
#define __itkDTITubeSpatialObject_h

#include <list>

#include "itkTubeSpatialObject.h"
#include "itkDTITubeSpatialObjectPoint.h"

namespace itk 
{

/** 
* \class DTITubeSpatialObject
* \brief Representation of a tube based on the spatial object classes.
*
* The tube is basically defined by a set of points. Each tube can
* be connected to a tube network, by using the AddSpatialObject() methods
* of a DTITubeSpatialObject Object. 
* A tube is also identified by an id number when connected to a network.
*
* \sa DTITubeSpatialObjectPoint 
*/

template < unsigned int TDimension = 3 >
class DTITubeSpatialObject : 
  public TubeSpatialObject< TDimension, 
                            DTITubeSpatialObjectPoint< TDimension >  >
{

public:

  typedef DTITubeSpatialObject                         Self;
  typedef TubeSpatialObject< TDimension,
          DTITubeSpatialObjectPoint< TDimension > >    Superclass;
  typedef SmartPointer < Self >                        Pointer;
  typedef SmartPointer < const Self >                  ConstPointer;
  typedef DTITubeSpatialObjectPoint< TDimension >      TubePointType;
  typedef typename Superclass::PointListType           PointListType;
  typedef typename Superclass::PointType               PointType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::SpatialObjectPointType  SpatialObjectPointType;
  typedef VectorContainer<unsigned long,PointType>     PointContainerType;
  typedef SmartPointer<PointContainerType>             PointContainerPointer;
  typedef typename Superclass::VectorType              VectorType;
  typedef typename Superclass::CovariantVectorType     CovariantVectorType;
  typedef typename Superclass::BoundingBoxType         BoundingBoxType;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Method for creation through the object factory. */
  itkTypeMacro( DTITubeSpatialObject, TubeSpatialObject );
    
protected:

  DTITubeSpatialObject();
  virtual ~DTITubeSpatialObject();

  /** Method to print the object.*/
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
#include "itkDTITubeSpatialObject.txx" 
#endif 

#endif // __itkDTITubeSpatialObject_h
