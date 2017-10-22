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

template< unsigned int NDimensions = 3 >
class ITK_TEMPLATE_EXPORT MetaFEMObjectConverter :
    public MetaConverterBase< NDimensions >
{
public:
  /** Standard class typedefs */
  typedef MetaFEMObjectConverter           Self;
  typedef MetaConverterBase< NDimensions > Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaFEMObjectConverter, MetaConverterBase);

  typedef typename Superclass::SpatialObjectType SpatialObjectType;
  typedef typename SpatialObjectType::Pointer    SpatialObjectPointer;
  typedef typename Superclass::MetaObjectType    MetaObjectType;

  /** Specific class types for conversion */
  typedef FEMObjectSpatialObject<NDimensions>               FEMObjectSpatialObjectType;
  typedef typename FEMObjectSpatialObjectType::Pointer      FEMObjectSpatialObjectPointer;
  typedef typename FEMObjectSpatialObjectType::ConstPointer FEMObjectSpatialObjectConstPointer;
  typedef MetaFEMObject                                     FEMObjectMetaObjectType;

  /** Convert the MetaObject to Spatial Object */
  virtual SpatialObjectPointer MetaObjectToSpatialObject(const MetaObjectType *mo) ITK_OVERRIDE;

  /** Convert the SpatialObject to MetaObject */
  virtual MetaObjectType *SpatialObjectToMetaObject(const SpatialObjectType *spatialObject) ITK_OVERRIDE;

protected:
  /** Create the specific MetaObject for this class */
  virtual MetaObjectType *CreateMetaObject() ITK_OVERRIDE;

  MetaFEMObjectConverter();
  ~MetaFEMObjectConverter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaFEMObjectConverter);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaFEMObjectConverter.hxx"
#endif


#endif
