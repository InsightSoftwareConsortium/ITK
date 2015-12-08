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

/*
* This is a test file for the itkImageMaskSpatialObject class.
* the Bounding box for an itk::ImageMaskSpatialObject should
* be related to the inside (axis aligned region) of the spatial object
* It Tests both 2D and 3D cases
*/

/*
 * This test addresses bug
 * https://issues.itk.org/jira/browse/ITK-3153
 *
 */

#include <itkSpatialObjectToImageFilter.h>
#include <itkBoxSpatialObject.h>
#include <itkImageMaskSpatialObject.h>

namespace
{
int Test3dImageMask()
{
  typedef itk::BoxSpatialObject< 3 >                BoxType;
  typedef BoxType::TransformType                    TransformType;

  //Box1 ---
  // bounding box of [0,1] x [0,1] x [0,1]
  // then translate it by 7 in all directions
  // bounding box should by [7,8] x [7,8] x [7,8]

  BoxType::Pointer box1 = BoxType::New();
  BoxType::SizeType                  sizeArray;
  sizeArray[0] = 1;
  sizeArray[1] = 1;
  sizeArray[2] = 1;
  box1->SetSize( sizeArray );
  box1->SetDefaultInsideValue(1);
  box1->SetDefaultOutsideValue(0);

  TransformType::Pointer transform = TransformType::New();
  TransformType::OutputVectorType translation;

  transform->SetIdentity();
  translation[0] = 7;
  translation[1] = 7;
  translation[2] = 7;
  transform->Translate(translation);

  box1->SetObjectToWorldTransform( transform.GetPointer() );

  box1->ComputeBoundingBox();
  BoxType::BoundingBoxType::Pointer box1BoundingBox = box1->GetBoundingBox();
  BoxType::BoundingBoxType::BoundsArrayType box1Bounds = box1BoundingBox->GetBounds();
  if ( itk::Math::NotAlmostEquals( box1Bounds[0], 7.0 ) || itk::Math::NotAlmostEquals( box1Bounds[2], 7.0 ) || itk::Math::NotAlmostEquals( box1Bounds[4], 7.0 ) ||
       itk::Math::NotAlmostEquals( box1Bounds[1], 8.0 ) || itk::Math::NotAlmostEquals( box1Bounds[3], 8.0 ) || itk::Math::NotAlmostEquals( box1Bounds[5], 8.0 ) )
  {
    std::cout << "Box1 - Bounding box Error"<<std::endl;
    std::cout << " Expecting a bounding box of [7 8 7 8 7 8]" << std::endl;
    std::cout << " But received one of " << box1Bounds << std::endl;
    return EXIT_FAILURE;
  }

  //Now generate an imageMaskSpatial Object from box1
  //Should have the same bounding box.
  typedef itk::Image<unsigned char, 3>              ImageType;
  typedef itk::ImageMaskSpatialObject<3>            ImageMaskSpatialObjectType;

  typedef itk::SpatialObjectToImageFilter<
    BoxType, ImageType >   SpatialObjectToImageFilterType;

  SpatialObjectToImageFilterType::Pointer imageFilter =
    SpatialObjectToImageFilterType::New();

  // note visual studio 2015 u1  (release mode) fails to exectute .Fill properly here by not initializing the last member. With initializer it is happy.
  itk::Size<3> size = { {10, 10 ,10} };
//size.Fill(10)


  //  The SpatialObjectToImageFilter requires that the user defines the grid
  //  parameters of the output image. This includes the number of pixels along
  //  each dimension, the pixel spacing, image direction and
  imageFilter->SetSize( size );
  double origin[3];
  origin[0] = 5;
  origin[1] = 5;
  origin[2] = 5;
  imageFilter->SetOrigin( origin );

  ImageType::SpacingType spacing;
  spacing[0] =  5.0 / size[0];
  spacing[1] =  5.0 / size[1];
  spacing[2] =  5.0 / size[2];
  imageFilter->SetSpacing( spacing );

  imageFilter->SetInsideValue(255); // white
  imageFilter->SetOutsideValue( 0 );
  imageFilter->SetInput(box1);
  imageFilter->Update();

  ImageMaskSpatialObjectType::Pointer maskSpatialObject = ImageMaskSpatialObjectType::New();
  maskSpatialObject->SetImage(imageFilter->GetOutput());

  maskSpatialObject->ComputeBoundingBox();

  ImageMaskSpatialObjectType::BoundingBoxType::Pointer maskBoundingBox = maskSpatialObject->GetBoundingBox();
  ImageMaskSpatialObjectType::BoundingBoxType::BoundsArrayType maskBounds = maskBoundingBox->GetBounds();

  //Test a few points...
  ImageMaskSpatialObjectType::PointType point;

  std::cout << "Mask -- Bounds : " <<  maskBounds << std::endl;
  point[0] = 0;point[1] = 0;point[2] = 0;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 6;point[1] = 7;point[2] = 7;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 7;point[1] = 7;point[2] = 7;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 8;point[1] = 7;point[2] = 7;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 8;point[1] = 8;point[2] = 8;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 9;point[1] = 7;point[2] = 7;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;

  if(    itk::Math::NotAlmostEquals(maskBounds[0], 7.0)
      || itk::Math::NotAlmostEquals(maskBounds[1], 8.5)
      || itk::Math::NotAlmostEquals(maskBounds[2], 7.0)
      || itk::Math::NotAlmostEquals(maskBounds[3], 8.5)
      || itk::Math::NotAlmostEquals(maskBounds[4], 7.0)
      || itk::Math::NotAlmostEquals(maskBounds[5], 8.5) )
    {
    std::cout << "[FAILED] " << std::endl;
    std::cout << "Test returned : " << maskSpatialObject->GetBoundingBox()->GetBounds() << std::endl;
    std::cout << "Instead of    : [7, 8.5, 7, 8.5, 7, 8.5]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[Passed] -- 3D test" << std::endl;
  return EXIT_SUCCESS;
}

int Test2dImageMask()
{
  typedef itk::BoxSpatialObject< 2 >                BoxType;
  typedef BoxType::TransformType                    TransformType;

  //Box1 ---
  // bounding box of [0,1] x [0,1]
  // then translate it by 7 in all directions
  // bounding box should by [7,8] x [7,8]

  BoxType::Pointer box1 = BoxType::New();
  BoxType::SizeType                  sizeArray;
  sizeArray[0] = 1;
  sizeArray[1] = 1;
  box1->SetSize( sizeArray );
  box1->SetDefaultInsideValue(1);
  box1->SetDefaultOutsideValue(0);

  TransformType::Pointer transform = TransformType::New();
  TransformType::OutputVectorType translation;

  transform->SetIdentity();
  translation[0] = 7;
  translation[1] = 7;
  transform->Translate(translation);

  box1->SetObjectToWorldTransform( transform.GetPointer() );

  box1->ComputeBoundingBox();
  BoxType::BoundingBoxType::Pointer box1BoundingBox = box1->GetBoundingBox();
  BoxType::BoundingBoxType::BoundsArrayType box1Bounds = box1BoundingBox->GetBounds();
  if ( itk::Math::NotAlmostEquals( box1Bounds[0], 7.0 ) || itk::Math::NotAlmostEquals( box1Bounds[2], 7.0 ) ||
       itk::Math::NotAlmostEquals( box1Bounds[1], 8.0 ) || itk::Math::NotAlmostEquals( box1Bounds[3], 8.0 ) )
  {
    std::cout << "Box1 - Bounding box Error"<<std::endl;
    std::cout << " Expecting a bounding box of [7 8 7 8]" << std::endl;
    std::cout << " But received one of " << box1Bounds << std::endl;
    return EXIT_FAILURE;
  }

  //Now generate an imageMaskSpatial Object from box1
  //Should have the same bounding box. withing pixelation bounds
  typedef itk::Image<unsigned char, 2>              ImageType;
  typedef itk::ImageMaskSpatialObject<2>            ImageMaskSpatialObjectType;

  typedef itk::SpatialObjectToImageFilter<
    BoxType, ImageType >   SpatialObjectToImageFilterType;

  SpatialObjectToImageFilterType::Pointer imageFilter =
    SpatialObjectToImageFilterType::New();

  itk::Size<2> size;
  size.Fill(10);
  //  The SpatialObjectToImageFilter requires that the user defines the grid
  //  parameters of the output image. This includes the number of pixels along
  //  each dimension, the pixel spacing, image direction and
  imageFilter->SetSize( size );

  double origin[2];
  origin[0] = 5;
  origin[1] = 5;
  imageFilter->SetOrigin( origin );

  ImageType::SpacingType spacing;
  spacing[0] =  5.0 / size[0];
  spacing[1] =  5.0 / size[1];
  imageFilter->SetSpacing( spacing );

  imageFilter->SetInsideValue(255); // white
  imageFilter->SetOutsideValue( 0 );
  imageFilter->SetInput(box1);
  imageFilter->Update();

  ImageMaskSpatialObjectType::Pointer maskSpatialObject = ImageMaskSpatialObjectType::New();
  maskSpatialObject->SetImage(imageFilter->GetOutput());

  maskSpatialObject->ComputeBoundingBox();

  ImageMaskSpatialObjectType::BoundingBoxType::Pointer maskBoundingBox = maskSpatialObject->GetBoundingBox();
  ImageMaskSpatialObjectType::BoundingBoxType::BoundsArrayType maskBounds = maskBoundingBox->GetBounds();

  //Test a few points...
  ImageMaskSpatialObjectType::PointType point;

  std::cout << "Mask -- Bounds : " <<  maskBounds << std::endl;
  point[0] = 0;point[1] = 0;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 6;point[1] = 7;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 7;point[1] = 7;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 8;point[1] = 7;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 8;point[1] = 8;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;
  point[0] = 9;point[1] = 7;
  std::cout << "   " << point << " isInside?  : " << maskSpatialObject->IsInside(point) << std::endl;

  if(    itk::Math::NotAlmostEquals(maskBounds[0], 7.0)
      || itk::Math::NotAlmostEquals(maskBounds[1], 8.5)
      || itk::Math::NotAlmostEquals(maskBounds[2], 7.0)
      || itk::Math::NotAlmostEquals(maskBounds[3], 8.5))
    {
    std::cout << "[FAILED] " << std::endl;
    std::cout << "Test returned : " << maskSpatialObject->GetBoundingBox()->GetBounds() << std::endl;
    std::cout << "Instead of    : [7, 8.5, 7, 8.5]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[Passed] -- 2D test" << std::endl;
  return EXIT_SUCCESS;
}

} //end empty namespace
int itkImageMaskSpatialObjectTest4(int, char* [])
{
  if (Test3dImageMask() == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }
  if (Test2dImageMask() == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
