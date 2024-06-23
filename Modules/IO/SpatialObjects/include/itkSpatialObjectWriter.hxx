/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


namespace itk
{
template <unsigned int VDimension, typename PixelType, typename TMeshTraits>
SpatialObjectWriter<VDimension, PixelType, TMeshTraits>::SpatialObjectWriter()
{
  m_FileName = "";
  m_SpatialObject = nullptr;
  m_BinaryPoints = false;
  m_WriteImagesInSeparateFile = false;
  m_MetaToSpatialConverter = MetaSceneConverterType::New();
}

template <unsigned int VDimension, typename PixelType, typename TMeshTraits>
void
SpatialObjectWriter<VDimension, PixelType, TMeshTraits>::SetMetaIOVersion(unsigned int ver)
{
  m_MetaToSpatialConverter->SetMetaIOVersion(ver);
}

template <unsigned int VDimension, typename PixelType, typename TMeshTraits>
unsigned int
SpatialObjectWriter<VDimension, PixelType, TMeshTraits>::GetMetaIOVersion(void) const
{
  return m_MetaToSpatialConverter->GetMetaIOVersion();
}

/** Set the precision at which the transform should be written */
template <unsigned int VDimension, typename PixelType, typename TMeshTraits>
void
SpatialObjectWriter<VDimension, PixelType, TMeshTraits>::SetTransformPrecision(unsigned int precision)
{
  m_MetaToSpatialConverter->SetTransformPrecision(precision);
}

/** Get the precision at which the transform should be written */
template <unsigned int VDimension, typename PixelType, typename TMeshTraits>
unsigned int
SpatialObjectWriter<VDimension, PixelType, TMeshTraits>::GetTransformPrecision()
{
  return m_MetaToSpatialConverter->GetTransformPrecision();
}

template <unsigned int VDimension, typename PixelType, typename TMeshTraits>
void
SpatialObjectWriter<VDimension, PixelType, TMeshTraits>::Update()
{
  m_MetaToSpatialConverter->SetBinaryPoints(m_BinaryPoints);
  m_MetaToSpatialConverter->SetWriteImagesInSeparateFile(m_WriteImagesInSeparateFile);

  if (m_SpatialObject.IsNotNull())
  {
    m_MetaToSpatialConverter->WriteMeta(m_SpatialObject, m_FileName.c_str());
    m_SpatialObject = nullptr;
  }
}

/** Add a converter for a new MetaObject/SpatialObject type */
template <unsigned int VDimension, typename PixelType, typename TMeshTraits>
void
SpatialObjectWriter<VDimension, PixelType, TMeshTraits>::RegisterMetaConverter(const char * metaTypeName,
                                                                               const char * spatialObjectTypeName,
                                                                               MetaConverterBaseType * converter)
{
  this->m_MetaToSpatialConverter->RegisterMetaConverter(metaTypeName, spatialObjectTypeName, converter);
}

} // namespace itk

#endif
