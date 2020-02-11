/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkAddImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"


int
itkAddImageFilterFrameTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int myDimension = 3;

  // Declare the types of the images
  using myImageType1 = itk::Image<float, myDimension>;
  using myImageType2 = itk::Image<float, myDimension>;
  using myImageType3 = itk::Image<float, myDimension>;

  // Declare the type of the index to access images
  using myIndexType = itk::Index<myDimension>;

  // Declare the type of the size
  using mySizeType = itk::Size<myDimension>;

  // Declare the type of the Region
  using myRegionType = itk::ImageRegion<myDimension>;

  // Declare the type for the ADD filter
  using myFilterType = itk::AddImageFilter<myImageType1, myImageType2, myImageType3>;

  // Declare the pointers to images
  using myImageType1Pointer = myImageType1::Pointer;
  using myImageType2Pointer = myImageType2::Pointer;
  using myImageType3Pointer = myImageType3::Pointer;
  using myFilterTypePointer = myFilterType::Pointer;

  // Create two images
  myImageType1Pointer inputImageA = myImageType1::New();
  myImageType2Pointer inputImageB = myImageType2::New();

  // Define their size, and start index
  mySizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  myIndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  myRegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  // Initialize Image A
  inputImageA->SetLargestPossibleRegion(region);
  inputImageA->SetBufferedRegion(region);
  inputImageA->SetRequestedRegion(region);
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion(region);
  inputImageB->SetBufferedRegion(region);
  inputImageB->SetRequestedRegion(region);
  inputImageB->Allocate();

  // Declare Iterator types apropriated for each image
  using myIteratorType1 = itk::ImageRegionIteratorWithIndex<myImageType1>;
  using myIteratorType2 = itk::ImageRegionIteratorWithIndex<myImageType2>;

  // Create one iterator for Image A (this is a light object)
  myIteratorType1 it1(inputImageA, inputImageA->GetBufferedRegion());

  // Initialize the content of Image A
  std::cout << "First operand " << std::endl;
  while (!it1.IsAtEnd())
  {
    it1.Set(2.0);
    std::cout << it1.Get() << std::endl;
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  myIteratorType2 it2(inputImageB, inputImageB->GetBufferedRegion());

  // Initialize the content of Image B
  std::cout << "Second operand " << std::endl;
  while (!it2.IsAtEnd())
  {
    it2.Set(3.0);
    std::cout << it2.Get() << std::endl;
    ++it2;
  }


  // Create an ADD Filter
  myFilterTypePointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput1(inputImageA);
  filter->SetInput2(inputImageB);


  // Get the Smart Pointer to the Filter Output
  myImageType3Pointer outputImage = filter->GetOutput();

  // Make Image B have a different origin
  myImageType2::PointType borigin;
  borigin.Fill(0.01);
  inputImageB->SetOrigin(borigin);


  // Execute the filter
  try
  {
    filter->Update();
    std::cout << "No exception thrown for a difference in origins!" << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cout << "Known exception caught (origin)! " << exc << std::endl;
  }


  // Make Image B have a different spacing
  inputImageB->CopyInformation(inputImageA);

  myImageType2::SpacingType bspacing;
  bspacing.Fill(1.001);
  inputImageB->SetSpacing(bspacing);

  // Execute the filter
  try
  {
    filter->Update();
    std::cout << "No exception thrown for a difference in spacings!" << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cout << "Known exception caught (spacing)! " << exc << std::endl;
  }


  // Make Image B have different direction cosines
  inputImageB->CopyInformation(inputImageA);

  myImageType2::DirectionType bdirections;
  bdirections.SetIdentity();
  bdirections[2][2] = -1.0;
  inputImageB->SetDirection(bdirections);

  // Execute the filter
  try
  {
    filter->Update();
    std::cout << "No exception thrown for a difference in directions!" << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cout << "Known exception caught (directions)! " << exc << std::endl;
  }


  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
