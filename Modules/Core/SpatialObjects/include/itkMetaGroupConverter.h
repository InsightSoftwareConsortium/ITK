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
#ifndef itkMetaGroupConverter_h
#define itkMetaGroupConverter_h

#include "itkGroupSpatialObject.h"
#include "metaGroup.h"
#include "itkMetaConverterBase.h"

namespace itk
{
/** \class MetaGroupConverter
 *  \brief converts between MetaObject<->SpatialObject.
 *
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template< unsigned int NDimensions = 3 >
class ITK_TEMPLATE_EXPORT MetaGroupConverter :
    public MetaConverterBase< NDimensions >
{
public:
  /** Standard class typedefs */
  typedef MetaGroupConverter               Self;
  typedef MetaConverterBase< NDimensions > Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaGroupConverter, MetaConverterBase);

  typedef typename Superclass::SpatialObjectType SpatialObjectType;
  typedef typename SpatialObjectType::Pointer    SpatialObjectPointer;
  typedef typename Superclass::MetaObjectType    MetaObjectType;

  /** Specific class types for conversion */
  typedef GroupSpatialObject<NDimensions>               GroupSpatialObjectType;
  typedef typename GroupSpatialObjectType::Pointer      GroupSpatialObjectPointer;
  typedef typename GroupSpatialObjectType::ConstPointer GroupSpatialObjectConstPointer;
  typedef MetaGroup                                     GroupMetaObjectType;

  /** Convert the MetaObject to Spatial Object */
  virtual SpatialObjectPointer MetaObjectToSpatialObject(const MetaObjectType *mo) ITK_OVERRIDE;

  /** Convert the SpatialObject to MetaObject */
  virtual MetaObjectType *SpatialObjectToMetaObject(const SpatialObjectType *spatialObject) ITK_OVERRIDE;

protected:
  /** Create the specific MetaObject for this class */
  virtual MetaObjectType *CreateMetaObject() ITK_OVERRIDE;

  MetaGroupConverter();
  ~MetaGroupConverter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaGroupConverter);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaGroupConverter.hxx"
#endif

#endif
