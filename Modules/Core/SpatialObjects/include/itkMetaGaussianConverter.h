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
#ifndef itkMetaGaussianConverter_h
#define itkMetaGaussianConverter_h

#include "itkMetaConverterBase.h"
#include "itkGaussianSpatialObject.h"
#include "metaGaussian.h"

namespace itk
{
/** \class MetaGaussianConverter
 *  \brief Converts between MetaObject<->SpatialObject.
 *
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template< unsigned int NDimensions = 3 >
class ITK_TEMPLATE_EXPORT MetaGaussianConverter :
    public MetaConverterBase< NDimensions >
{
public:
  /** Standard class typedefs */
  typedef MetaGaussianConverter            Self;
  typedef MetaConverterBase< NDimensions > Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaGaussianConverter, MetaConverterBase);

  typedef typename Superclass::SpatialObjectType SpatialObjectType;
  typedef typename SpatialObjectType::Pointer    SpatialObjectPointer;
  typedef typename Superclass::MetaObjectType    MetaObjectType;

  /** Specific class types for conversion */
  typedef GaussianSpatialObject<NDimensions>               GaussianSpatialObjectType;
  typedef typename GaussianSpatialObjectType::Pointer      GaussianSpatialObjectPointer;
  typedef typename GaussianSpatialObjectType::ConstPointer GaussianSpatialObjectConstPointer;
  typedef MetaGaussian                                     GaussianMetaObjectType;

  /** Convert the MetaObject to Spatial Object */
  SpatialObjectPointer MetaObjectToSpatialObject(const MetaObjectType *mo) override;

  /** Convert the SpatialObject to MetaObject */
  MetaObjectType *SpatialObjectToMetaObject(const SpatialObjectType *spatialObject) override;

protected:
  /** Create the specific MetaObject for this class */
  MetaObjectType *CreateMetaObject() override;

  MetaGaussianConverter();
  ~MetaGaussianConverter() override {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaGaussianConverter);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaGaussianConverter.hxx"
#endif

#endif
