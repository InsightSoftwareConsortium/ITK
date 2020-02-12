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
#ifndef itkMetaBlobConverter_h
#define itkMetaBlobConverter_h

#include "metaBlob.h"
#include "itkBlobSpatialObject.h"
#include "itkMetaConverterBase.h"

namespace itk
{
/**
 *\class MetaBlobConverter
 *  \brief converts between MetaObject<->SpatialObject.
 *
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template <unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT MetaBlobConverter : public MetaConverterBase<NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaBlobConverter);

  /** Standard class type aliases */
  using Self = MetaBlobConverter;
  using Superclass = MetaConverterBase<NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaBlobConverter, MetaConverterBase);

  using SpatialObjectType = typename Superclass::SpatialObjectType;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;
  using MetaObjectType = typename Superclass::MetaObjectType;

  /** Specific class types for conversion */
  using BlobSpatialObjectType = BlobSpatialObject<NDimensions>;
  using BlobSpatialObjectPointer = typename BlobSpatialObjectType::Pointer;
  using BlobSpatialObjectConstPointer = typename BlobSpatialObjectType::ConstPointer;
  using BlobMetaObjectType = MetaBlob;


  /** Convert the MetaObject to Spatial Object */
  SpatialObjectPointer
  MetaObjectToSpatialObject(const MetaObjectType * mo) override;

  /** Convert the SpatialObject to MetaObject */
  MetaObjectType *
  SpatialObjectToMetaObject(const SpatialObjectType * spatialObject) override;

protected:
  /** Create the specific MetaObject for this class */
  MetaObjectType *
  CreateMetaObject() override;

  MetaBlobConverter() = default;
  ~MetaBlobConverter() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMetaBlobConverter.hxx"
#endif

#endif
