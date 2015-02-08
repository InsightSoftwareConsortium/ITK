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
#ifndef itkSpatialObjectWriter_hxx
#define itkSpatialObjectWriter_hxx

#include "itkSpatialObjectWriter.h"

namespace itk
{
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
SpatialObjectWriter< NDimensions, PixelType, TMeshTraits >
::SpatialObjectWriter()
{
  m_FileName = "";
  m_SpatialObject = ITK_NULLPTR;
  m_Scene = ITK_NULLPTR;
  m_BinaryPoints = false;
  m_WriteImagesInSeparateFile = false;
}

template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
SpatialObjectWriter< NDimensions, PixelType, TMeshTraits >
::~SpatialObjectWriter()
{}

/** Set the precision at which the transform should be written */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
void
SpatialObjectWriter< NDimensions, PixelType, TMeshTraits >
::SetTransformPrecision(unsigned int precision)
{
  m_MetaToSpatialConverter.SetTransformPrecision(precision);
}

/** Get the precision at which the transform should be written */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
unsigned int
SpatialObjectWriter< NDimensions, PixelType, TMeshTraits >
::GetTransformPrecision()
{
  return m_MetaToSpatialConverter.GetTransformPrecision();
}

template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
void
SpatialObjectWriter< NDimensions, PixelType, TMeshTraits >
::Update()
{
  m_MetaToSpatialConverter.SetBinaryPoints(m_BinaryPoints);
  m_MetaToSpatialConverter.SetWriteImagesInSeparateFile(m_WriteImagesInSeparateFile);

  if ( m_Scene != ITK_NULLPTR )
    {
    m_MetaToSpatialConverter.WriteMeta( m_Scene, m_FileName.c_str() );
    m_Scene = ITK_NULLPTR;
    }
  else
    {
    if ( m_SpatialObject.IsNotNull() )
      {
      typename SceneType::Pointer tScene = SceneType::New();
      tScene->AddSpatialObject(m_SpatialObject);
      // Check if IDs are valid because IDs are used to determine
      //    parent-child hierarchy
      tScene->FixIdValidity();

      m_MetaToSpatialConverter.WriteMeta( tScene,
                                          m_FileName.c_str() );
      m_SpatialObject = ITK_NULLPTR;
      }
    }
}

/** Add a converter for a new MetaObject/SpatialObject type */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
void
SpatialObjectWriter< NDimensions, PixelType, TMeshTraits >
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
