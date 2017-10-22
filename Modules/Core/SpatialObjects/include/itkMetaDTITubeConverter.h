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
#ifndef itkMetaDTITubeConverter_h
#define itkMetaDTITubeConverter_h

#include "itkMetaConverterBase.h"
#include "metaDTITube.h"
#include "itkDTITubeSpatialObject.h"

namespace itk
{

/** \class MetaDTITubeConverter
 *  \brief converts between MetaObject<->SpatialObject
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template< unsigned int NDimensions = 3 >
class ITK_TEMPLATE_EXPORT MetaDTITubeConverter :
    public MetaConverterBase< NDimensions >
{
public:
  /** Standard class typedefs */
  typedef MetaDTITubeConverter             Self;
  typedef MetaConverterBase< NDimensions > Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaDTITubeConverter, MetaConverterBase);

  typedef typename Superclass::SpatialObjectType SpatialObjectType;
  typedef typename SpatialObjectType::Pointer    SpatialObjectPointer;
  typedef typename Superclass::MetaObjectType    MetaObjectType;

  /** Specific class types for conversion */
  typedef DTITubeSpatialObject<NDimensions>               DTITubeSpatialObjectType;
  typedef typename DTITubeSpatialObjectType::Pointer      DTITubeSpatialObjectPointer;
  typedef typename DTITubeSpatialObjectType::ConstPointer DTITubeSpatialObjectConstPointer;
  typedef MetaDTITube                                     DTITubeMetaObjectType;

  /** Convert the MetaObject to Spatial Object */
  virtual SpatialObjectPointer MetaObjectToSpatialObject(const MetaObjectType *mo) ITK_OVERRIDE;

  /** Convert the SpatialObject to MetaObject */
  virtual MetaObjectType *SpatialObjectToMetaObject(const SpatialObjectType *spatialObject) ITK_OVERRIDE;

protected:
  /** Create the specific MetaObject for this class */
  virtual MetaObjectType *CreateMetaObject() ITK_OVERRIDE;

  MetaDTITubeConverter();
  ~MetaDTITubeConverter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaDTITubeConverter);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaDTITubeConverter.hxx"
#endif

#endif
