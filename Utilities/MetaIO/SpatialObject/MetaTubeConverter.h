/*=========================================================================

  Program:   itkUNC
  Module:    MetaTubeConverter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  Author:    Julien Jomier (julien@jomier.com)

  Copyright (c) 2002 CADDLab @ UNC. All rights reserved.
  See itkUNCCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MetaTubeConverter__h
#define __MetaTubeConverter__h


#include "metaTube.h"
#include "itkTubeSpatialObject.h"
#include "itkSpatialObject.h"

template <unsigned int NDimensions = 3>
class MetaTubeConverter
{

public:

  MetaTubeConverter();
  ~MetaTubeConverter() {};

  typedef itk::TubeSpatialObject<NDimensions> SpatialObjectType;


  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  //typedef typename itk::NDimensionalSpatialObject NDimSpatialObject;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaTubeToTubeSpatialObject(MetaTube * Tube);
  MetaTube* TubeSpatialObjectToMetaTube(SpatialObjectType * spatialObject);

};


#ifndef ITK_MANUAL_INSTANTIATION
  #include "MetaTubeConverter.txx"
#endif


#endif
