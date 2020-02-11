/*=========================================================================
 *
 *  Copyright NumFOCUS
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
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
SpatialObjectReader<NDimensions, PixelType, TMeshTraits>::SpatialObjectReader()
{
  m_FileName = "";
  m_SpatialObject = nullptr;
  m_Group = nullptr;
  m_MetaToSpatialConverter = MetaSceneConverterType::New();
}

template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
void
SpatialObjectReader<NDimensions, PixelType, TMeshTraits>::Update()
{
  m_SpatialObject = m_MetaToSpatialConverter->ReadMeta(m_FileName.c_str());
  m_Group = nullptr;

  if (m_SpatialObject == nullptr)
  {
    itkExceptionMacro("No objects were found in file " << m_FileName);
  }
}

/** Add a converter for a new MetaObject/SpatialObject type */
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
void
SpatialObjectReader<NDimensions, PixelType, TMeshTraits>::RegisterMetaConverter(const char * metaTypeName,
                                                                                const char * spatialObjectTypeName,
                                                                                MetaConverterBaseType * converter)
{
  this->m_MetaToSpatialConverter->RegisterMetaConverter(metaTypeName, spatialObjectTypeName, converter);
}

} // namespace itk

#endif
