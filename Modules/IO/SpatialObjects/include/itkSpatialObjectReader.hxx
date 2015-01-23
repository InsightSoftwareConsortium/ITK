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
#ifndef itkSpatialObjectReader_hxx
#define itkSpatialObjectReader_hxx

#include "itkSpatialObjectReader.h"

namespace itk
{
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
SpatialObjectReader< NDimensions, PixelType, TMeshTraits >
::SpatialObjectReader()
{
  m_FileName = "";
  m_Scene = ITK_NULLPTR;
  m_Group = ITK_NULLPTR;
}

template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
SpatialObjectReader< NDimensions, PixelType, TMeshTraits >
::~SpatialObjectReader()
{}

template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
void
SpatialObjectReader< NDimensions, PixelType, TMeshTraits >
::Update()
{
  m_Scene = m_MetaToSpatialConverter.ReadMeta( m_FileName.c_str() );

  if ( m_Scene->GetNumberOfObjects(0) == 0 )
    {
    itkExceptionMacro("No groups were found in file " << m_FileName);
    }

  if ( m_Scene->GetNumberOfObjects(0) == 1 )
    {
    typename SceneType::ObjectListType * list = m_Scene->GetObjects(0);
    typename SceneType::ObjectListType::iterator it = list->begin();
    if ( !strncmp( ( *it )->GetTypeName(), "Group", 5 ) )
      {
      m_Group = static_cast< GroupType * >( ( *it ).GetPointer() );
      }
    else
      {
      m_Group = GroupType::New();
      m_Group->AddSpatialObject( static_cast< SpatialObjectType * >( ( *it ).GetPointer() ) );
      }
    delete list;
    }
  else
    {
    m_Group = GroupType::New();
    typename SceneType::ObjectListType * list = m_Scene->GetObjects(0);
    typename SceneType::ObjectListType::iterator it = list->begin();
    typename SceneType::ObjectListType::iterator it_end = list->end();
    while ( it != it_end )
      {
      m_Group->AddSpatialObject( static_cast< SpatialObjectType * >( *it ) );
      it++;
      }
    delete list;
    }
}

/** Add a converter for a new MetaObject/SpatialObject type */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
void
SpatialObjectReader< NDimensions, PixelType, TMeshTraits >
::RegisterMetaConverter(const char *metaTypeName,
                      const char *spatialObjectTypeName,
                      MetaConverterBaseType *converter)
{
  this->m_MetaToSpatialConverter.RegisterMetaConverter(metaTypeName,
                                                     spatialObjectTypeName,
                                                     converter);
}

} // namespace itk

#endif
