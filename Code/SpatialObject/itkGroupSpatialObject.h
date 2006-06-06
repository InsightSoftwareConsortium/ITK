/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGroupSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGroupSpatialObject_h
#define __itkGroupSpatialObject_h

#include <list>

#include "itkSpatialObject.h"

namespace itk 
{

/** 
* \class GroupSpatialObject
* \brief Representation of a group based on the spatial object classes.
*
* A GroupSpatialObject represents a group by serving as the parent of
* the elements of the group.  Since any itk::SpatialObject can have
* children (see SpatialObject::GetChildren()), this class needs no
* additional methods.
*/

template < unsigned int TDimension = 3 >
class GroupSpatialObject 
  :public SpatialObject< TDimension >
{

public:

  typedef GroupSpatialObject                           Self;
  typedef SpatialObject< TDimension >                  Superclass;
  typedef SmartPointer < Self >                        Pointer;
  typedef SmartPointer < const Self >                  ConstPointer;
  typedef double                                       ScalarType;
  typedef typename Superclass::TreeNodeType            TreeNodeType;
  typedef typename TreeNodeType::ChildrenListType      TreeNodeChildrenListType;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Method for creation through the object factory. */
  itkTypeMacro( GroupSpatialObject, SpatialObject );

  /**  */
  bool ComputeLocalBoundingBox() const {return false;}
    
protected:
  GroupSpatialObject(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
   
  GroupSpatialObject();
  virtual ~GroupSpatialObject();

  /** Method to print the object.*/
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
#include "itkGroupSpatialObject.txx" 
#endif 

#endif // __itkGroupSpatialObject_h
