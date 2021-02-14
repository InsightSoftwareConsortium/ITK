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
#ifndef itkMetaVesselTubeConverter_h
#define itkMetaVesselTubeConverter_h

#include "metaVesselTube.h"
#include "itkMetaConverterBase.h"
#include "itkTubeSpatialObject.h"

namespace itk
{

/**
 *\class MetaVesselTubeConverter
 *  \brief Converts between MetaObject<->SpatialObject
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template <unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT MetaVesselTubeConverter : public MetaConverterBase<NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MetaVesselTubeConverter);

  /** Standard class type aliases */
  using Self = MetaVesselTubeConverter;
  using Superclass = MetaConverterBase<NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaVesselTubeConverter, MetaConverterBase);

  using SpatialObjectType = typename Superclass::SpatialObjectType;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;
  using MetaObjectType = typename Superclass::MetaObjectType;

  /** Specific class types for conversion */
  using VesselTubeSpatialObjectType = TubeSpatialObject<NDimensions>;
  using VesselTubeSpatialObjectPointer = typename VesselTubeSpatialObjectType::Pointer;
  using VesselTubeSpatialObjectConstPointer = typename VesselTubeSpatialObjectType::ConstPointer;
  using VesselTubeMetaObjectType = MetaVesselTube;

  /** Convert the MetaObject to Spatial Object */
  SpatialObjectPointer
  MetaObjectToSpatialObject(const MetaObjectType * mo) override;

  /** Convert the SpatialObject to MetaObject */
  MetaObjectType *
  SpatialObjectToMetaObject(const SpatialObjectType * so) override;

protected:
  /** Create the specific MetaObject for this class */
  MetaObjectType *
  CreateMetaObject() override;

  MetaVesselTubeConverter() = default;
  ~MetaVesselTubeConverter() override = default;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMetaVesselTubeConverter.hxx"
#endif

#endif
