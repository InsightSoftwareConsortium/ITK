/*=========================================================================

  Program:   itkUNC
  Module:    MetaGroupConverter.txx
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
#ifndef __MetaGroupConverter__txx
#define __MetaGroupConverter__txx

#include "MetaGroupConverter.h"

/** Constructor */ 
template <unsigned int NDimensions>                                          
MetaGroupConverter<NDimensions>
::MetaGroupConverter()
{
  
}


/** Convert a metaGroup into an group SpatialObject  */
template <unsigned int NDimensions>       
typename MetaGroupConverter<NDimensions>::SpatialObjectPointer
MetaGroupConverter<NDimensions>
::MetaGroupToGroupSpatialObject(MetaGroup * group)
{ 
  SpatialObjectPointer spatialObject = SpatialObjectType::New();
  spatialObject->GetProperty()->SetName((char*)group->Name());
  spatialObject->SetParentId(group->ParentID());
  spatialObject->SetId(group->ID());
  return spatialObject;
}

/** Convert an group SpatialObject into a metaGroup */
template <unsigned int NDimensions>       
MetaGroup*
MetaGroupConverter<NDimensions>
::GroupSpatialObjectToMetaGroup(SpatialObjectType * spatialObject)
{ 
  MetaGroup* group = new MetaGroup(NDimensions);

  group->ParentID(spatialObject->GetParentId());
  group->ID(spatialObject->GetId());
  return group;
}


/** Read a meta file give the type */
template <unsigned int NDimensions>       
typename MetaGroupConverter<NDimensions>::SpatialObjectPointer
MetaGroupConverter<NDimensions>
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaGroup* group = new MetaGroup();
  group->Read(name);
  spatialObject = MetaGroupToGroupSpatialObject(group);

  return spatialObject;
}


/** Write a meta group file */
template <unsigned int NDimensions>
bool
MetaGroupConverter<NDimensions>
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaGroup* group = GroupSpatialObjectToMetaGroup(spatialObject);
  group->Write(name);
  return true;
}

#endif
