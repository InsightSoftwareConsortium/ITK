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
// Disable warning for long symbol names in this file only

/*
* This is a test file for the itkImageMaskSpatialObject class.
* The suported pixel types does not include itkRGBPixel, itkRGBAPixel, etc...
* So far it only allows to manage images of simple types like unsigned short,
* unsigned int, or itk::Vector<...>.
*/


#include "itkImageRegionIterator.h"

#include "itkImageMaskSpatialObject.h"


int itkImageMaskSpatialObjectTest(int, char* [])
{
  const unsigned int NDimensions = 3;

  typedef itk::ImageMaskSpatialObject<NDimensions> ImageMaskSpatialObject;
  typedef ImageMaskSpatialObject::PixelType        PixelType;
  typedef ImageMaskSpatialObject::ImageType        ImageType;
  typedef itk::ImageRegionIterator<ImageType>      Iterator;

  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size = {{ 50, 50, 50 }};
  ImageType::IndexType index = {{ 0, 0, 0 }};
  ImageType::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  image->SetRegions( region );
  image->Allocate(true); // initialize buffer to zero

  ImageType::RegionType insideRegion;
  ImageType::SizeType insideSize   = {{ 30, 30, 30 }};
  ImageType::IndexType insideIndex = {{ 10, 10, 10 }};
  insideRegion.SetSize( insideSize );
  insideRegion.SetIndex( insideIndex );


  Iterator it( image, insideRegion );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( itk::NumericTraits< PixelType >::max() );
    ++it;
    }

  ImageMaskSpatialObject::Pointer maskSO = ImageMaskSpatialObject::New();
  maskSO->Print(std::cout);

  maskSO->SetImage(image);

  maskSO->ComputeObjectToWorldTransform();

  Iterator itr( image, region );
  itr.GoToBegin();

  while( !itr.IsAtEnd() )
    {
    const ImageType::IndexType constIndex =  itr.GetIndex();
    const bool reference = insideRegion.IsInside( constIndex );
    ImageType::PointType point;
    image->TransformIndexToPhysicalPoint( constIndex, point );
    const bool test      = maskSO->IsInside( point );
      if( test != reference )
        {
        std::cerr << "Error in the evaluation of IsInside() " << std::endl;
        std::cerr << "Index failed = " << constIndex << std::endl;
        return EXIT_FAILURE;
        }
    ++itr;
    }

  maskSO->Print(std::cout);

  return EXIT_SUCCESS;
}
