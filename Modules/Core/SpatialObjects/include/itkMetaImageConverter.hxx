/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMetaImageConverter_hxx
#define itkMetaImageConverter_hxx

#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"

namespace itk
{
template <unsigned int VDimension, typename PixelType, typename TSpatialObjectType>
auto
MetaImageConverter<VDimension, PixelType, TSpatialObjectType>::CreateMetaObject() -> MetaObjectType *
{
  MetaObjectType * mo = dynamic_cast<MetaObjectType *>(new ImageMetaObjectType);
  mo->APIVersion(1);
  mo->FileFormatVersion(1);
  return mo;
}

template <unsigned int VDimension, typename PixelType, typename TSpatialObjectType>
const char *
MetaImageConverter<VDimension, PixelType, TSpatialObjectType>::GetMetaObjectSubType()
{
  return "Image";
}

template <unsigned int VDimension, typename PixelType, typename TSpatialObjectType>
auto
MetaImageConverter<VDimension, PixelType, TSpatialObjectType>::AllocateImage(const ImageMetaObjectType * image) ->
  typename ImageType::Pointer
{
  auto rval = ImageType::New();

  using SizeType = typename ImageType::SizeType;
  using SpacingType = typename ImageType::SpacingType;
  using DirectionType = typename ImageType::DirectionType;
  using PointType = typename ImageType::PointType;
  using RegionType = typename ImageType::RegionType;

  SizeType      size;
  PointType     origin;
  SpacingType   spacing;
  DirectionType direction;

  for (unsigned int i = 0; i < VDimension; ++i)
  {
    size[i] = image->DimSize()[i];
    origin[i] = image->ElementOrigin()[i];
    if (Math::ExactlyEquals(image->ElementSpacing()[i], typename SpacingType::ValueType{}))
    {
      spacing[i] = 1;
    }
    else
    {
      spacing[i] = image->ElementSpacing()[i];
    }
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      direction[i][j] = image->ElementDirection(i, j);
    }
  }

  const RegionType region(size);
  rval->SetRegions(region);
  rval->SetOrigin(origin);
  rval->SetSpacing(spacing);
  rval->SetDirection(direction);
  rval->Allocate();
  return rval;
}

/** Convert a metaImage into an ImageMaskSpatialObject  */
template <unsigned int VDimension, typename PixelType, typename TSpatialObjectType>
auto
MetaImageConverter<VDimension, PixelType, TSpatialObjectType>::MetaObjectToSpatialObject(const MetaObjectType * mo)
  -> SpatialObjectPointer
{
  const auto * imageMO = dynamic_cast<const ImageMetaObjectType *>(mo);

  if (imageMO == nullptr)
  {
    itkExceptionMacro("Can't convert MetaObject to MetaImage");
  }

  ImageSpatialObjectPointer imageSO = ImageSpatialObjectType::New();

  typename ImageType::Pointer myImage = this->AllocateImage(imageMO);

  this->MetaObjectToSpatialObjectBase(imageMO, imageSO);

  itk::ImageRegionIteratorWithIndex<ImageType> it(myImage, myImage->GetLargestPossibleRegion());
  for (unsigned int i = 0; !it.IsAtEnd(); i++, ++it)
  {
    it.Set(static_cast<typename ImageType::PixelType>(imageMO->ElementData(i)));
  }

  imageSO->SetImage(myImage);

  return imageSO.GetPointer();
}


/** Convert an Image SpatialObject into a metaImage */
template <unsigned int VDimension, typename PixelType, typename TSpatialObjectType>
auto
MetaImageConverter<VDimension, PixelType, TSpatialObjectType>::SpatialObjectToMetaObject(const SpatialObjectType * so)
  -> MetaObjectType *
{
  const ImageSpatialObjectConstPointer imageSO = dynamic_cast<const ImageSpatialObjectType *>(so);

  if (imageSO.IsNull())
  {
    itkExceptionMacro("Can't downcast SpatialObject to ImageSpatialObject");
  }
  using ImageConstPointer = typename ImageType::ConstPointer;

  ImageConstPointer SOImage = imageSO->GetImage();

  int    size[VDimension];
  double spacing[VDimension];
  double direction[VDimension * VDimension];
  double origin[VDimension];

  for (unsigned int i = 0; i < VDimension; ++i)
  {
    size[i] = SOImage->GetLargestPossibleRegion().GetSize()[i];
    spacing[i] = SOImage->GetSpacing()[i];
    origin[i] = SOImage->GetOrigin()[i];
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      direction[i * VDimension + j] = SOImage->GetDirection()[i][j];
    }
  }

  auto * imageMO = new MetaImage(VDimension, size, spacing, MET_GetPixelType(typeid(PixelType)));
  imageMO->APIVersion(1);
  imageMO->FileFormatVersion(1);

  this->SpatialObjectToMetaObjectBase(imageSO, imageMO);

  imageMO->ElementOrigin(origin);
  imageMO->ElementDirection(direction);

  itk::ImageRegionConstIterator<ImageType> it(SOImage, SOImage->GetLargestPossibleRegion());
  for (unsigned int i = 0; !it.IsAtEnd(); i++, ++it)
  {
    imageMO->ElementData(i, it.Get());
  }

  imageMO->BinaryData(true);
  imageMO->ElementDataFileName("LOCAL");
  imageMO->ObjectSubTypeName(this->GetMetaObjectSubType());

  if (this->GetWriteImagesInSeparateFile())
  {
    std::string filename = imageSO->GetProperty().GetName();
    if (filename.empty())
    {
      std::cout << "Error: you should set the image name when using"
                << " WriteImagesInSeparateFile." << std::endl;
      std::cout << "The image will be written locally." << std::endl;
    }
    else
    {
      filename += ".raw";
      imageMO->ElementDataFileName(filename.c_str());
    }
  }

  return imageMO;
}

} // end namespace itk

#endif
