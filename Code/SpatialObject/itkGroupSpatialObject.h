/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGroupSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Method for creation through the object factory. */
  itkTypeMacro( Self, Superclass );
    
protected:

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
