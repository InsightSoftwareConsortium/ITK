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
#ifndef itkMetaEllipseConverter_h
#define itkMetaEllipseConverter_h

#include "itkMetaConverterBase.h"
#include "itkEllipseSpatialObject.h"
#include "metaEllipse.h"

namespace itk
{
/** \class MetaEllipseConverter
 *  \brief converts between MetaObject<->SpatialObject.
 *
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template< unsigned int NDimensions = 3 >
class ITK_TEMPLATE_EXPORT MetaEllipseConverter :
    public MetaConverterBase< NDimensions >
{
public:
  /** Standard class typedefs */
  typedef MetaEllipseConverter             Self;
  typedef MetaConverterBase< NDimensions > Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaEllipseConverter, MetaConverterBase);

  typedef typename Superclass::SpatialObjectType SpatialObjectType;
  typedef typename SpatialObjectType::Pointer    SpatialObjectPointer;
  typedef typename Superclass::MetaObjectType    MetaObjectType;

  /** Specific class types for conversion */
  typedef EllipseSpatialObject<NDimensions>               EllipseSpatialObjectType;
  typedef typename EllipseSpatialObjectType::Pointer      EllipseSpatialObjectPointer;
  typedef typename EllipseSpatialObjectType::ConstPointer EllipseSpatialObjectConstPointer;
  typedef MetaEllipse                                     EllipseMetaObjectType;

  /** Convert the MetaObject to Spatial Object */
  SpatialObjectPointer MetaObjectToSpatialObject(const MetaObjectType *mo) override;

  /** Convert the SpatialObject to MetaObject */
  MetaObjectType *SpatialObjectToMetaObject(const SpatialObjectType *spatialObject) override;

protected:
  /** Create the specific MetaObject for this class */
  MetaObjectType *CreateMetaObject() override;

  MetaEllipseConverter();
  ~MetaEllipseConverter() override {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaEllipseConverter);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaEllipseConverter.hxx"
#endif

#endif
