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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

namespace m = itk::Math;

int itkLabelImageToShapeLabelMapFilterTest2(int , char *[])
{

  const unsigned int dim = 2;

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ShapeLabelObject< PixelType, dim >     LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >            LabelMapType;


  ImageType::RegionType region;
  ImageType::SizeType size;
  size[0] = 11;
  size[1] = 13;
  ImageType::IndexType index;
  index.Fill(0);
  region.SetSize(size);
  region.SetIndex(index);

  LabelMapType::Pointer labelMap;
  LabelObjectType::Pointer labelObject;

  LabelObjectType::OrientedBoundingBoxPointType obbOrigin;
  LabelObjectType::OrientedBoundingBoxSizeType  obbSize;

  ImageType::Pointer image = ImageType::New();
  image->SetRegions(region);
  image->Allocate(true);

  //
  // Test case one pixel
  //

  index[0] = 5;
  index[1] = 7;
  image->SetPixel(index, 1);

  typedef itk::LabelImageToShapeLabelMapFilter< ImageType, LabelMapType> L2SType;
  L2SType::Pointer l2s = L2SType::New();
  l2s->SetInput( image );
  l2s->SetComputeOrientedBoundingBox(true);
  TEST_SET_GET_VALUE(true, l2s->GetComputeOrientedBoundingBox() );

  TRY_EXPECT_NO_EXCEPTION( l2s->Update() );

  labelMap = l2s->GetOutput();
  labelObject = labelMap->GetLabelObject(1);

  obbSize = labelObject->GetOrientedBoundingBoxSize();
  obbOrigin = labelObject->GetOrientedBoundingBoxOrigin();
  TEST_EXPECT_TRUE( m::AlmostEquals(obbSize[0], 1.0) && m::AlmostEquals(obbSize[1], 1.0));

  //
  // Test case two diagonal pixels
  //

  ++index[0];
  ++index[1];

  image->SetPixel(index, 1);
  image->Modified();

  TRY_EXPECT_NO_EXCEPTION( l2s->Update() );

  labelMap = l2s->GetOutput();
  labelObject = labelMap->GetLabelObject(1);

  obbSize = labelObject->GetOrientedBoundingBoxSize();
  obbOrigin = labelObject->GetOrientedBoundingBoxOrigin();
  TEST_EXPECT_TRUE( m::AlmostEquals(obbSize[0], m::sqrt2) && m::AlmostEquals(obbSize[1], 2.0*m::sqrt2));
  TEST_EXPECT_TRUE( m::AlmostEquals(obbOrigin[0], 4.0) && m::AlmostEquals(obbOrigin[1], 7.0));

  //
  // Test case two diagonal pixels, with flip direction matrix
  //
  ImageType::DirectionType direction;
  direction.Fill(0.0);
  direction(0,1) = 1.0;
  direction(1,0) = 1.0;

  image->SetDirection(direction);
  image->Modified();


  TRY_EXPECT_NO_EXCEPTION( l2s->Update() );

  labelMap = l2s->GetOutput();
  labelObject = labelMap->GetLabelObject(1);

  obbSize = labelObject->GetOrientedBoundingBoxSize();
  obbOrigin = labelObject->GetOrientedBoundingBoxOrigin();
  TEST_EXPECT_TRUE( m::AlmostEquals(obbSize[0], m::sqrt2) && m::AlmostEquals(obbSize[1], 2.0*m::sqrt2));
  TEST_EXPECT_TRUE( m::AlmostEquals(obbOrigin[0], 6.0) && m::AlmostEquals(obbOrigin[1], 5.0));


  //
  // Test case 2x4 rectangle
  //
  image->FillBuffer(0);
  direction.SetIdentity();
  image->SetDirection(direction);
  for (unsigned int i = 4; i < 6; ++i)
    {
    for (unsigned int j = 3; j < 7; ++j)
      {
      ImageType::IndexType idx;
      idx[0] = i;
      idx[1] = j;
      image->SetPixel(idx, 1);
      }
    }

  image->Modified();


  TRY_EXPECT_NO_EXCEPTION( l2s->Update() );

  labelMap = l2s->GetOutput();
  labelObject = labelMap->GetLabelObject(1);

  obbSize = labelObject->GetOrientedBoundingBoxSize();
  obbOrigin = labelObject->GetOrientedBoundingBoxOrigin();
  std::cout << labelObject;
  TEST_EXPECT_TRUE( m::AlmostEquals(obbSize[0], 2.0) && m::AlmostEquals(obbSize[1], 4.0));
  TEST_EXPECT_TRUE( m::AlmostEquals(obbOrigin[0], 3.5) && m::AlmostEquals(obbOrigin[1], 2.5));


  // Just exercise these methods
  std::cout << "OBB vertices: " << labelObject->GetOrientedBoundingBoxVertices() << std::endl;
  std::cout << "OBB direction: " << labelObject->GetOrientedBoundingBoxDirection();

  return EXIT_SUCCESS;
}
