/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVesselTubeSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVesselTubeSpatialObject_h
#define __itkVesselTubeSpatialObject_h

#include <list>

#include "itkTubeSpatialObject.h"
#include "itkVesselTubeSpatialObjectPoint.h"

namespace itk 
{

/** 
* \class VesselTubeSpatialObject
* \brief Representation of a tube based on the spatial object classes.
*
* The tube is basically defined by a set of points. Each tube can
* be connected to a tube network, by using the AddSpatialObject() methods
* of a VesselTubeSpatialObject Object. 
* A tube is also identified by an id number when connected to a network.
*
* \sa VesselTubeSpatialObjectPoint 
*/

template < unsigned int TDimension = 3 >
class VesselTubeSpatialObject 
  :public TubeSpatialObject< TDimension, 
                             VesselTubeSpatialObjectPoint< TDimension >  >
{

public:

  typedef VesselTubeSpatialObject                      Self;
  typedef TubeSpatialObject< TDimension,
          VesselTubeSpatialObjectPoint< TDimension > > Superclass;
  typedef SmartPointer < Self >                        Pointer;
  typedef SmartPointer < const Self >                  ConstPointer;
  typedef VesselTubeSpatialObjectPoint< TDimension >   TubePointType;
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
  itkTypeMacro( VesselTubeSpatialObject, TubeSpatialObject );
    
protected:

  VesselTubeSpatialObject();
  virtual ~VesselTubeSpatialObject();

  /** Method to print the object.*/
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
#include "itkVesselTubeSpatialObject.txx" 
#endif 

#endif // __itkVesselTubeSpatialObject_h
