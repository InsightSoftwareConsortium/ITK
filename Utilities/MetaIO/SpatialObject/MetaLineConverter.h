/*=========================================================================

  Program:   itkUNC
  Module:    MetaLineConverter.h
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
#ifndef __MetaLineConverter__h
#define __MetaLineConverter__h


#include "metaLine.h"
#include "itkLineSpatialObject.h"
#include "itkNDimensionalSpatialObject.h"

template <unsigned int NDimensions = 3>
class MetaLineConverter
{

public:

  MetaLineConverter();
  ~MetaLineConverter() {};

  typedef itk::LineSpatialObject<NDimensions> SpatialObjectType;

  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaLineToLineSpatialObject(MetaLine * Line);
  MetaLine* LineSpatialObjectToMetaLine(SpatialObjectType * spatialObject);

};


#ifndef ITK_MANUAL_INSTANTIATION
  #include "MetaLineConverter.txx"
#endif


#endif
