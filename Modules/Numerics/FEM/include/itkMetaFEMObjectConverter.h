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
#ifndef itkMetaFEMObjectConverter_h
#define itkMetaFEMObjectConverter_h


#include "metaFEMObject.h"
#include "itkFEMObjectSpatialObject.h"
#include "itkMetaConverterBase.h"

namespace itk
{

/** \class MetaFEMObjectConverter
 * \brief Converts from a FEMObject into a FEMSpatialOPbject
 *
 * This class was provides the conversion functionality
 * itk::FEMObject into Meta Object -> FEMObjectSpatialObjectToMetaFEMObject
 * Meta Object into a itk::FEMObject -> MetaFEMObjectToFEMObjectSpatialObject
 *
 * This provides the general infrastructure required for the Meta I/O
 * to read and write the FEMObject as a SpatialObject.
 *
 * \sa FEMObject FEMObjectSpatialObject
 * \ingroup ITKFEM
 */

template <unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT MetaFEMObjectConverter : public MetaConverterBase<NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaFEMObjectConverter);

  /** Standard class type aliases */
  using Self = MetaFEMObjectConverter;
  using Superclass = MetaConverterBase<NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaFEMObjectConverter, MetaConverterBase);

  using SpatialObjectType = typename Superclass::SpatialObjectType;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;
  using MetaObjectType = typename Superclass::MetaObjectType;

  /** Specific class types for conversion */
  using FEMObjectSpatialObjectType = FEMObjectSpatialObject<NDimensions>;
  using FEMObjectSpatialObjectPointer = typename FEMObjectSpatialObjectType::Pointer;
  using FEMObjectSpatialObjectConstPointer = typename FEMObjectSpatialObjectType::ConstPointer;
  using FEMObjectMetaObjectType = MetaFEMObject;

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

  MetaFEMObjectConverter();
  ~MetaFEMObjectConverter() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMetaFEMObjectConverter.hxx"
#endif


#endif // itkMetaFEMObjectConverter_h
