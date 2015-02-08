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
#ifndef itkMetaGroupConverter_hxx
#define itkMetaGroupConverter_hxx

#include "itkMetaGroupConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaGroupConverter< NDimensions >
::MetaGroupConverter()
{}

template< unsigned int NDimensions >
typename MetaGroupConverter< NDimensions >::MetaObjectType *
MetaGroupConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new GroupMetaObjectType);
}

/** Convert a metaGroup into an group SpatialObject  */
template< unsigned int NDimensions >
typename MetaGroupConverter< NDimensions >::SpatialObjectPointer
MetaGroupConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const GroupMetaObjectType *group = dynamic_cast<const GroupMetaObjectType *>(mo);
  if(group == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaGroup" );
    }

  GroupSpatialObjectPointer groupSO = GroupSpatialObjectType::New();

  double               spacing[NDimensions];
  unsigned int         i;

  for ( i = 0; i < NDimensions; i++ )
    {
    spacing[i] = group->ElementSpacing()[i];
    }
  groupSO->GetModifiableIndexToObjectTransform()->SetScaleComponent(spacing);
  groupSO->GetProperty()->SetName( group->Name() );
  groupSO->GetProperty()->SetRed(group->Color()[0]);
  groupSO->GetProperty()->SetGreen(group->Color()[1]);
  groupSO->GetProperty()->SetBlue(group->Color()[2]);
  groupSO->GetProperty()->SetAlpha(group->Color()[3]);
  groupSO->SetId( group->ID() );
  groupSO->SetParentId( group->ParentID() );
  return groupSO.GetPointer();
}

/** Convert a group SpatialObject into a metaGroup */
template< unsigned int NDimensions >
typename MetaGroupConverter< NDimensions >::MetaObjectType *
MetaGroupConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *so)
{
  GroupSpatialObjectConstPointer groupSO =
    dynamic_cast<const GroupSpatialObjectType *>(so);
  if(groupSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to GroupSpatialObject");
    }

  GroupMetaObjectType *group = new GroupMetaObjectType(NDimensions);

  float color[4];

  for ( unsigned int i = 0; i < 4; i++ )
    {
    color[i] = groupSO->GetProperty()->GetColor()[i];
    }
  group->Color(color);

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    group->ElementSpacing(i, groupSO->GetIndexToObjectTransform()
                          ->GetScaleComponent()[i]);
    }

  if ( groupSO->GetParent() )
    {
    group->ParentID( groupSO->GetParent()->GetId() );
    }
  group->ID( groupSO->GetId() );

  return group;
}

} // end namespace itk

#endif
