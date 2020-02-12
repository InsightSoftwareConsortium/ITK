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
#ifndef itkMetaDTITubeConverter_h
#define itkMetaDTITubeConverter_h

#include "itkMetaConverterBase.h"
#include "metaDTITube.h"
#include "itkDTITubeSpatialObject.h"

namespace itk
{

/**
 *\class MetaDTITubeConverter
 *  \brief converts between MetaObject<->SpatialObject
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template <unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT MetaDTITubeConverter : public MetaConverterBase<NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaDTITubeConverter);

  /** Standard class type aliases */
  using Self = MetaDTITubeConverter;
  using Superclass = MetaConverterBase<NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaDTITubeConverter, MetaConverterBase);

  using SpatialObjectType = typename Superclass::SpatialObjectType;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;
  using MetaObjectType = typename Superclass::MetaObjectType;

  /** Specific class types for conversion */
  using DTITubeSpatialObjectType = DTITubeSpatialObject<NDimensions>;
  using DTITubeSpatialObjectPointer = typename DTITubeSpatialObjectType::Pointer;
  using DTITubeSpatialObjectConstPointer = typename DTITubeSpatialObjectType::ConstPointer;
  using DTITubeMetaObjectType = MetaDTITube;

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

  MetaDTITubeConverter() = default;
  ~MetaDTITubeConverter() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMetaDTITubeConverter.hxx"
#endif

#endif
