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

namespace itk
{

/** \class TubeNetworkSpatialObject
* \brief Network of tubes as spatial object
*
* This class allow to create a network of tubes. 
* Any spatial object can be plug to this network, but the 
* specifics functions will only be runned on the tubes, or tubes network
* objects.
*
* \also TubeSpatialObject TubeSpatialObjectPoint
*/

template < unsigned int TDimension , unsigned int PipelineDimension = 3 >
class TubeNetworkSpatialObject 
: public SpatialObject<  TDimension, PipelineDimension >
{

public:

  typedef TubeNetworkSpatialObject                      Self;
  typedef double                                        ScalarType;
  typedef SmartPointer < Self >                         Pointer;
  typedef SmartPointer < const Self >                   ConstPointer;
  typedef SpatialObject< TDimension,PipelineDimension>  Superclass;
  typedef SmartPointer<Superclass>                      SuperclassPointer;
  typedef SpatialObject< TDimension,PipelineDimension>  ChildrenType;
  typedef SmartPointer<ChildrenType>                    ChildrenPointer;
  typedef std::list< ChildrenType * >                   ChildrenListType;
  typedef TubeSpatialObject<TDimension>                 TubeType;
  typedef std::list< TubeType * >                       TubeListType;

  itkNewMacro( Self );

  itkTypeMacro( Self, Superclass );

  /** Compute Tangents */
  void CalcTangent( void );

  /** Get Tubes in the tree given a certain depth */
  TubeListType * GetTubes( unsigned int maximumDepth=0 , unsigned int currentDepth=0 ) const;

protected:

  TubeNetworkSpatialObject( void );
  ~TubeNetworkSpatialObject( void );

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkTubeNetworkSpatialObject.txx" 
#endif 

#endif // __itkTubeNetworkSpatialObject_h
