/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeNetworkSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkTubeNetworkSpatialObject_h
#define __itkTubeNetworkSpatialObject_h

#include "itkAffineTransform.h"
#include "itkTubeSpatialObject.h"
#include "itkCompositeSpatialObject.h"

namespace itk
{

/** \class TubeNetworkSpatialObject
* \brief Network of tubes as spatial object
*
* This class allow to create a network of tubes. It derives from
* a composite spatial object, and have extra functionnalities specific
* to tubes. Any spatial object can be plug to this network, but the 
* specifics functions will only be runned on the tubes, or tubes network
* objects.
*
* \also TubeSpatialObject TubePoint
*/

class ITK_EXPORT TubeNetworkSpatialObject 
: public CompositeSpatialObject< 3, AffineTransform< double, 3 >, bool >
{

public:

  typedef TubeNetworkSpatialObject Self;
  typedef double ScalarType;
  typedef SmartPointer < Self > Pointer;
  typedef SmartPointer < const Self > ConstPointer;
  typedef CompositeSpatialObject< 3, AffineTransform< double, 3 >, bool > Superclass;
  typedef Superclass::Pointer SuperclassPointer;
  typedef SpatialObject< 3, AffineTransform< double, 3 >, bool > ChildrenType;
  typedef ChildrenType::Pointer ChildrenPointer;
  typedef std::list< ChildrenPointer > ChildrenListType;

  itkNewMacro( Self );
  itkTypeMacro( Self, Superclass );

  void CalcTangent( void );

protected:

  TubeNetworkSpatialObject( void );
  ~TubeNetworkSpatialObject( void );
};

} // end namespace itk

#endif // __itkTubeNetworkSpatialObject_h
