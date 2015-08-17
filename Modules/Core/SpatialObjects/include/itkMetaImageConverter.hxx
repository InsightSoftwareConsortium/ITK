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
#ifndef itkMetaImageConverter_hxx
#define itkMetaImageConverter_hxx

#include "itkMetaImageConverter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions, typename PixelType , typename TSpatialObjectType >
MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >
::MetaImageConverter()
{}

template< unsigned int NDimensions, typename PixelType , typename TSpatialObjectType >
typename MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >::MetaObjectType *
MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new ImageMetaObjectType);
}

template< unsigned int NDimensions, typename PixelType , typename TSpatialObjectType >
const char *
MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >
::GetMetaObjectSubType()
{
  return "Image";
}

template< unsigned int NDimensions, typename PixelType , typename TSpatialObjectType >
typename MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >::ImageType::Pointer
MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >
::AllocateImage(const ImageMetaObjectType *image)
{
  typename ImageType::Pointer rval = ImageType::New();

  typedef typename ImageType::SizeType    SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef typename ImageType::RegionType  RegionType;

  SizeType size;
  SpacingType spacing;

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    size[i] = image->DimSize()[i];
    if ( Math::ExactlyEquals(image->ElementSpacing()[i], NumericTraits< typename SpacingType::ValueType >::ZeroValue()) )
      {
      spacing[i] = 1;
      }
    else
      {
      spacing[i] = image->ElementSpacing()[i];
      }
    }

  RegionType region;
  region.SetSize(size);
  itk::Index< NDimensions > zeroIndex;
  zeroIndex.Fill(0);
  region.SetIndex(zeroIndex);
  rval->SetLargestPossibleRegion(region);
  rval->SetBufferedRegion(region);
  rval->SetRequestedRegion(region);
  rval->SetSpacing(spacing);
  rval->Allocate();
  return rval;
}

/** Convert a metaImage into an ImageMaskSpatialObject  */
template< unsigned int NDimensions, typename PixelType , typename TSpatialObjectType >
typename MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >::SpatialObjectPointer
MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const ImageMetaObjectType *imageMO =
    dynamic_cast<const ImageMetaObjectType *>(mo);

  if(imageMO == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaImage" );
    }

  ImageSpatialObjectPointer imageSO = ImageSpatialObjectType::New();


  typename ImageType::Pointer myImage = this->AllocateImage(imageMO);

  itk::ImageRegionIteratorWithIndex< ImageType >
    it(myImage,myImage->GetLargestPossibleRegion());
  for ( unsigned int i = 0; !it.IsAtEnd(); i++, ++it )
    {
    it.Set(
      static_cast< typename ImageType::PixelType >( imageMO->ElementData(i) ) );
    }

  imageSO->SetImage(myImage);
  imageSO->SetId( imageMO->ID() );
  imageSO->SetParentId( imageMO->ParentID() );
  imageSO->GetProperty()->SetName( imageMO->Name() );

  return imageSO.GetPointer();
}


/** Convert an Image SpatialObject into a metaImage */
template< unsigned int NDimensions, typename PixelType , typename TSpatialObjectType >
typename MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >::MetaObjectType *
MetaImageConverter< NDimensions, PixelType, TSpatialObjectType >
::SpatialObjectToMetaObject(const SpatialObjectType *so)
{
  const ImageSpatialObjectConstPointer imageSO =
    dynamic_cast<const ImageSpatialObjectType *>(so);

  if(imageSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to ImageSpatialObject");
    }
  typedef typename ImageType::ConstPointer     ImageConstPointer;

  ImageConstPointer SOImage = imageSO->GetImage();

  float spacing[NDimensions];
  int   size[NDimensions];

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    size[i] = SOImage->GetLargestPossibleRegion().GetSize()[i];
    spacing[i] = SOImage->GetSpacing()[i];
    }

  ImageMetaObjectType *imageMO =
    new MetaImage( NDimensions, size,
                   spacing, MET_GetPixelType( typeid( PixelType ) ) );

  itk::ImageRegionConstIterator< ImageType > it( SOImage,
                                                 SOImage->GetLargestPossibleRegion() );
  for ( unsigned int i = 0; !it.IsAtEnd(); i++, ++it )
    {
    imageMO->ElementData( i, it.Get() );
    }

  imageMO->ID( imageSO->GetId() );
  imageMO->BinaryData(true);

  imageMO->ElementDataFileName("LOCAL");

  imageMO->ObjectSubTypeName(this->GetMetaObjectSubType());

  if ( this->GetWriteImagesInSeparateFile())
    {
    std::string filename = imageSO->GetProperty()->GetName();
    if ( filename.size() == 0 )
      {
      std::cout << "Error: you should set the image name when using"
                << " WriteImagesInSeparateFile." << std::endl;
      std::cout << "The image will be written locally." << std::endl;
      }
    else
      {
      filename += ".raw";
      imageMO->ElementDataFileName( filename.c_str() );
      }
    }
  return imageMO;
}

} // end namespace itk

#endif
