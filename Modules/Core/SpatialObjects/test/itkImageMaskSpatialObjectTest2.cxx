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
 * http://public.kitware.com/Bug/view.php?id=0006340
 *
 */

#include "itkImageRegionIterator.h"
#include "itkImageMaskSpatialObject.h"
#include "itkMath.h"


int itkImageMaskSpatialObjectTest2(int, char* [])
{
  const unsigned int NDimensions = 3;
  int retval=EXIT_SUCCESS;

  typedef itk::ImageMaskSpatialObject<NDimensions> ImageMaskSpatialObject;
  typedef ImageMaskSpatialObject::PixelType        PixelType;
  typedef itk::Image<PixelType,NDimensions>        ImageType;
  typedef itk::Image<PixelType,NDimensions>        ImageType2;
  typedef itk::ImageRegionIterator<ImageType>      Iterator;
  typedef itk::ImageRegionIterator<ImageType2>     Iterator2;

  // Direction was not taken into account in the image spatial object
  // explicitly test using images with directions set.
  // Also explicitly uses nonzero origin, non identity scales
  // just to see

  {
  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size = {{ 50, 50, 50 }};
  ImageType::PointType origin;
  origin[0]=1.5;
  origin[1]=2.1;
  origin[2]=-3;
  image->SetOrigin(origin);

  ImageType::SpacingType spacing;
  spacing[0]=0.5;
  spacing[1]=0.7;
  spacing[2]=1.1;
  image->SetSpacing( spacing );
  ImageType::IndexType index = {{ 0, 0, 0 }};

  // Does it make sense to set the direction for a non-oriented image
  // well it happens frequently enough, so better test it
  ImageType::DirectionType direction;
  direction.Fill(0.0);
  direction[0][1]=-1;
  direction[1][0]=1;
  direction[2][2]=1;
  image->SetDirection(direction);

  ImageType::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);
  image->SetRegions( region );
  image->Allocate(true); // initialize buffer to zero

  ImageType::RegionType insideRegion;
  const unsigned int INSIDE_SIZE = 30;
  const unsigned int INSIDE_INDEX = 10;
    {
    const ImageType::SizeType insideSize   = {{ INSIDE_SIZE, INSIDE_SIZE, INSIDE_SIZE }};
    insideRegion.SetSize( insideSize );
    }
    {
    const ImageType::IndexType insideIndex = {{ INSIDE_INDEX, INSIDE_INDEX, INSIDE_INDEX }};
    insideRegion.SetIndex( insideIndex );
    }


  Iterator it( image, insideRegion );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( itk::NumericTraits< PixelType >::max() );
    ++it;
    }

  ImageMaskSpatialObject::Pointer maskSO = ImageMaskSpatialObject::New();

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
      std::cerr << "Point failed = " << point << std::endl;
      std::cerr << "Image is a: "<<image->GetNameOfClass()<<std::endl;
      std::cerr << "Direction is: "<<std::endl<<image->GetDirection()<<std::endl;
      retval=EXIT_FAILURE;
      break;
      }

    ++itr;
    }

  if(retval==EXIT_SUCCESS)
    {
    std::cout<<"Test with "<<image->GetNameOfClass()<<" passed."<<std::endl;
    }

  // Check if insideregion is properly computed at the image boundary
  {
  ImageType::IndexType startPointIndex =
    {{ INSIDE_SIZE-2,
       INSIDE_SIZE-2,
       INSIDE_SIZE-2 }};
  ImageType::IndexType endPointIndex =
    {{ INSIDE_INDEX+INSIDE_SIZE+2,
       INSIDE_INDEX+INSIDE_SIZE+2,
       INSIDE_INDEX+INSIDE_SIZE+2 }};
  ImageType::PointType startPoint;
  ImageType::PointType endPoint;
  image->TransformIndexToPhysicalPoint( startPointIndex, startPoint );
  image->TransformIndexToPhysicalPoint( endPointIndex, endPoint );

  // Traverse along the line that goes through mask boundaries and
  // check if the value and the mask is consistent
  const int numberOfSteps = static_cast<int> (
    std::sqrt(double(INSIDE_SIZE*INSIDE_SIZE+
                    INSIDE_SIZE*INSIDE_SIZE+
                    INSIDE_SIZE*INSIDE_SIZE))*100.0
    );
  const ImageType::SpacingType incrementVector =
    (endPoint-startPoint)/static_cast<double>(numberOfSteps);
  ImageType::PointType point=startPoint;
  for (int i=0; i<numberOfSteps; i++)
    {
    point += incrementVector;
    const bool isInside = maskSO->IsInside( point );
    double value = itk::NumericTraits< PixelType >::ZeroValue();
    maskSO->ValueAt( point, value );
    const bool isZero = (itk::Math::ExactlyEquals(value, itk::NumericTraits< PixelType >::ZeroValue()));
    if( (isInside && isZero) || (!isInside && !isZero) )
      {
      ImageType::IndexType pointIndex;
      image->TransformPhysicalPointToIndex( point, pointIndex );
      std::cerr << "Error in the evaluation ValueAt and IsInside (all the points inside the mask shall have non-zero value) " << std::endl;
      std::cerr << "isInside = " << isInside << std::endl;
      std::cerr << "value = " << value << std::endl;
      std::cerr << "Index failed = " << pointIndex << std::endl;
      std::cerr << "Point failed = " << point << std::endl;
      std::cerr << "Image is a: "<< image->GetNameOfClass()<<std::endl;
      retval= EXIT_FAILURE;
      break;
      }
    }
  }
  }

  {
  ImageType2::Pointer image = ImageType2::New();
  ImageType2::SizeType size = {{ 50, 50, 50 }};
  ImageType::PointType origin;
  origin[0]=1.5;
  origin[1]=2.1;
  origin[2]=-3;
  image->SetOrigin(origin);

  ImageType::SpacingType spacing;
  spacing[0]=0.5;
  spacing[1]=0.7;
  spacing[2]=1.1;
  image->SetSpacing( spacing );

  ImageType2::IndexType index = {{ 0, 0, 0 }};
  ImageType2::DirectionType direction;
  direction.Fill(0.0);
  direction[0][1]=-1;
  direction[1][0]=1;
  direction[2][2]=1;
  image->SetDirection(direction);

  ImageType::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  image->SetRegions( region );
  image->Allocate(true); // initialize buffer to zero

  ImageType::RegionType insideRegion;
  const unsigned int INSIDE_SIZE = 30;
  const unsigned int INSIDE_INDEX = 10;
    {
    const ImageType::SizeType insideSize   = {{ INSIDE_SIZE, INSIDE_SIZE, INSIDE_SIZE }};
    insideRegion.SetSize( insideSize );
    }
    {
    const ImageType::IndexType insideIndex = {{ INSIDE_INDEX, INSIDE_INDEX, INSIDE_INDEX }};
    insideRegion.SetIndex( insideIndex );
    }


  Iterator2 it( image, insideRegion );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( itk::NumericTraits< PixelType >::max() );
    ++it;
    }

  ImageMaskSpatialObject::Pointer maskSO = ImageMaskSpatialObject::New();

  maskSO->SetImage(image);

  maskSO->ComputeObjectToWorldTransform();

  Iterator2 itr( image, region );
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
      std::cerr << "Point failed = " << point << std::endl;
      std::cerr << "Image is a: "<<image->GetNameOfClass()<<std::endl;
      std::cerr << "Direction is: "<<std::endl<<image->GetDirection()<<std::endl;
      retval= EXIT_FAILURE;
      break;
      }
      ++itr;
    }
  if(retval==EXIT_SUCCESS)
    {
    std::cout<<"Test with "<<image->GetNameOfClass()<<" passed."<<std::endl;
    }
  }

  return retval;
}
