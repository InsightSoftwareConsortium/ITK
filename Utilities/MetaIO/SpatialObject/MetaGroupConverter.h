/*=========================================================================

  Program:   itkUNC
  Module:    MetaGroupConverter.h
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
#ifndef __MetaGroupConverter__h
#define __MetaGroupConverter__h

#include "itkGroupSpatialObject.h"
#include "metaGroup.h"

template <unsigned int NDimensions = 3>
class MetaGroupConverter
{

public:

  MetaGroupConverter();
  ~MetaGroupConverter() {};

  typedef itk::GroupSpatialObject<NDimensions> SpatialObjectType;
  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaGroupToGroupSpatialObject(MetaGroup * group);
  MetaGroup* GroupSpatialObjectToMetaGroup(SpatialObjectType * spatialObject);

};


#ifndef ITK_MANUAL_INSTANTIATION
  #include "MetaGroupConverter.txx"
#endif


#endif
