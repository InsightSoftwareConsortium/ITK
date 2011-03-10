/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkMetaGroupConverter_txx
#define __itkMetaGroupConverter_txx

#include "itkMetaGroupConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaGroupConverter< NDimensions >
::MetaGroupConverter()
{}

/** Convert a metaGroup into an group SpatialObject  */
template< unsigned int NDimensions >
typename MetaGroupConverter< NDimensions >::SpatialObjectPointer
MetaGroupConverter< NDimensions >
::MetaGroupToGroupSpatialObject(MetaGroup *group)
{
  SpatialObjectPointer spatialObject = SpatialObjectType::New();
  double               spacing[NDimensions];
  unsigned int         i;

  for ( i = 0; i < NDimensions; i++ )
    {
    spacing[i] = group->ElementSpacing()[i];
    }
  spatialObject->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  spatialObject->GetProperty()->SetName( group->Name() );
  spatialObject->GetProperty()->SetRed(group->Color()[0]);
  spatialObject->GetProperty()->SetGreen(group->Color()[1]);
  spatialObject->GetProperty()->SetBlue(group->Color()[2]);
  spatialObject->GetProperty()->SetAlpha(group->Color()[3]);
  spatialObject->SetId( group->ID() );
  spatialObject->SetParentId( group->ParentID() );
  return spatialObject;
}

/** Convert an group SpatialObject into a metaGroup */
template< unsigned int NDimensions >
MetaGroup *
MetaGroupConverter< NDimensions >
::GroupSpatialObjectToMetaGroup(SpatialObjectType *spatialObject)
{
  MetaGroup *group = new MetaGroup(NDimensions);

  float color[4];

  for ( unsigned int i = 0; i < 4; i++ )
    {
    color[i] = spatialObject->GetProperty()->GetColor()[i];
    }
  group->Color(color);

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    group->ElementSpacing(i, spatialObject->GetIndexToObjectTransform()
                          ->GetScaleComponent()[i]);
    }

  if ( spatialObject->GetParent() )
    {
    group->ParentID( spatialObject->GetParent()->GetId() );
    }
  group->ID( spatialObject->GetId() );

  return group;
}

/** Read a meta file give the type */
template< unsigned int NDimensions >
typename MetaGroupConverter< NDimensions >::SpatialObjectPointer
MetaGroupConverter< NDimensions >
::ReadMeta(const char *name)
{
  SpatialObjectPointer spatialObject;
  MetaGroup *          group = new MetaGroup();

  group->Read(name);
  spatialObject = this->MetaGroupToGroupSpatialObject(group);

  return spatialObject;
}

/** Write a meta group file */
template< unsigned int NDimensions >
bool
MetaGroupConverter< NDimensions >
::WriteMeta(SpatialObjectType *spatialObject, const char *name)
{
  MetaGroup *group = this->GroupSpatialObjectToMetaGroup(spatialObject);

  group->Write(name);
  return true;
}
} // end namespace itk

#endif
