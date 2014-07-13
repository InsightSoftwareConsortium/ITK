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

/*
 * This test addresses bug
 * http://public.kitware.com/Bug/view.php?id=11972
 *
 */

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageMaskSpatialObject.h"

int itkImageMaskSpatialObjectTest3(int, char* [])
{
  const unsigned int NDimensions = 3;
  int retval=EXIT_SUCCESS;

  typedef itk::ImageMaskSpatialObject<NDimensions>     ImageMaskSpatialObjectType;
  typedef ImageMaskSpatialObjectType::PixelType        PixelType;
  typedef itk::Image<PixelType,NDimensions>            ImageType;

  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size = {{ 5, 5, 5 }};
  ImageType::PointType origin;
  origin[0] = origin[1] =  origin[2]=0.0;
  image->SetOrigin(origin);

  ImageType::SpacingType spacing;
  spacing[0] = spacing[1] = spacing[2]=1.0;
  image->SetSpacing( spacing );

  ImageType::IndexType index = {{ 0, 0, 0 }};

  ImageType::DirectionType direction;
  direction.Fill(0.0);
  direction[0][1]=1;
  direction[1][0]=1;
  direction[2][2]=1;
  image->SetDirection(direction);

  ImageType::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);
  image->SetRegions( region );
  image->Allocate(true); // initialize buffer to zero

  ImageMaskSpatialObjectType::Pointer imageMaskSpatialObject =
    ImageMaskSpatialObjectType::New();
  imageMaskSpatialObject->SetImage(image);

  ImageMaskSpatialObjectType::RegionType maskRegion =
    imageMaskSpatialObject->GetAxisAlignedBoundingBoxRegion();
  std::cout << maskRegion << std::endl;
  ImageMaskSpatialObjectType::RegionType::SizeType regionSize =
    maskRegion.GetSize();
  ImageMaskSpatialObjectType::IndexType::IndexType regionIndex =
    maskRegion.GetIndex();

  for(unsigned int i = 0; i < 3; i++)
    {
    if(regionSize[i] != 0)
      {
      std::cerr << "Invalid Region Size " << regionSize << std::endl;
      retval = EXIT_FAILURE;
      break;
      }
    if(regionIndex[i] < 0 || regionIndex[i] >= (int)size[i])
      {
      std::cerr << "Invalid Region Index " << regionIndex << std::endl;
      retval = EXIT_FAILURE;
      break;
      }
    }
  if(retval == EXIT_FAILURE)
    {
    return retval;
    }

  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageRegionIteratorType;
  ImageRegionIteratorType it(image,image->GetLargestPossibleRegion());
  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    ImageType::PointType point;
    image->TransformIndexToPhysicalPoint(it.GetIndex(),point);
    if(imageMaskSpatialObject->IsInside(point))
      {
      std::cerr << "Pixel Reported Inside mask, even though mask image is all zeros"
                << std::endl;
      retval = EXIT_FAILURE;
      break;
      }
    }
  return retval;
}
