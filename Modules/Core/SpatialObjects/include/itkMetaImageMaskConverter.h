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
#ifndef itkMetaImageMaskConverter_h
#define itkMetaImageMaskConverter_h

//
// to avoid an Internal Compiler Error in Visual Studio 6.0
//
#include "metaImage.h"
#include "itkImageMaskSpatialObject.h"
#include "itkMetaImageConverter.h"

namespace itk
{

/**
 *\class MetaImageMaskConverter
 *  \brief converts between MetaObject<->SpatialObject
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template <unsigned int NDimensions = 3>
class MetaImageMaskConverter
  : public MetaImageConverter<NDimensions, unsigned char, ImageMaskSpatialObject<NDimensions>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MetaImageMaskConverter);

  /** Standard class type aliases */
  using Self = MetaImageMaskConverter;
  using Superclass = MetaImageConverter<NDimensions, unsigned char>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaImageMaskConverter, MetaImageConverter);

protected:
  const char *
  GetMetaObjectSubType() override
  {
    return "Mask";
  }
  MetaImageMaskConverter() = default;
  ~MetaImageMaskConverter() override = default;
};

} // end namespace itk

#endif
