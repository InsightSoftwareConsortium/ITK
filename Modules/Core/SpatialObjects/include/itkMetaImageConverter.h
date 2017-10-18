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
#ifndef itkMetaImageConverter_h
#define itkMetaImageConverter_h

// to avoid an Internal Compiler Error in Visual Studio 6.0
#include "metaImage.h"
#include "itkMetaConverterBase.h"
#include "itkImageSpatialObject.h"
#include "itkMetaConverterBase.h"

namespace itk
{

/** \class MetaImageConverter
 *  \brief converts between MetaObject<->SpatialObject.
 *
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template< unsigned int NDimensions = 3,
          typename TPixel = unsigned char,
          typename TSpatialObjectType = ImageSpatialObject< NDimensions,TPixel > >
class ITK_TEMPLATE_EXPORT MetaImageConverter :
    public MetaConverterBase< NDimensions >
{
public:
  /** Standard class typedefs */
  typedef MetaImageConverter               Self;
  typedef MetaConverterBase< NDimensions > Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaImageConverter, MetaConverterBase);

  typedef typename Superclass::SpatialObjectType SpatialObjectType;
  typedef typename SpatialObjectType::Pointer    SpatialObjectPointer;
  typedef typename Superclass::MetaObjectType    MetaObjectType;

  /** Specific class types for conversion */
  typedef TSpatialObjectType                            ImageSpatialObjectType;
  typedef typename ImageSpatialObjectType::Pointer      ImageSpatialObjectPointer;
  typedef typename ImageSpatialObjectType::ConstPointer ImageSpatialObjectConstPointer;
  typedef MetaImage                                     ImageMetaObjectType;
  typedef Image<TPixel,NDimensions>                     ImageType;
  /** Convert the MetaObject to Spatial Object */
  virtual SpatialObjectPointer MetaObjectToSpatialObject(const MetaObjectType *mo) ITK_OVERRIDE;

  /** Convert the SpatialObject to MetaObject */
  virtual MetaObjectType *SpatialObjectToMetaObject(const SpatialObjectType *spatialObject) ITK_OVERRIDE;

protected:
  /** Create the specific MetaObject for this class */
  virtual MetaObjectType *CreateMetaObject() ITK_OVERRIDE;
  virtual const char *GetMetaObjectSubType();

  typename ImageType::Pointer AllocateImage(const ImageMetaObjectType *image);

  MetaImageConverter();
  ~MetaImageConverter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaImageConverter);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaImageConverter.hxx"
#endif

#endif
