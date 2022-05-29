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

#include "itkJoinImageFilter.h"
#include "itkRGBAPixel.h"
#include "vnl/vnl_sample.h"
#include "itkImageRegionIterator.h"
#include "itkTestingMacros.h"

int
itkJoinImageFilterTest(int, char *[])
{
  // Define the dimension of the images
  constexpr unsigned int myDimension = 2;

  // Declare the types of the images
  using myImageType1 = itk::Image<char, myDimension>;
  using myImageType2 = itk::Image<itk::Vector<unsigned short, 2>, myDimension>;
  using myImageType3 = itk::Image<itk::RGBAPixel<short>, myDimension>;

  // Declare the type of the index to access images
  using myIndexType = itk::Index<myDimension>;

  // Declare the type of the size
  using mySizeType = itk::Size<myDimension>;

  // Declare the type of the Region
  using myRegionType = itk::ImageRegion<myDimension>;

  // Create three images
  auto inputImageA = myImageType1::New();
  auto inputImageB = myImageType2::New();
  auto inputImageC = myImageType3::New();

  // Define their size, and start index
  mySizeType size;
  size[0] = 5;
  size[1] = 8;

  myIndexType start;
  start[0] = 0;
  start[1] = 0;

  myRegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  // Initialize Image A
  inputImageA->SetRegions(region);
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetRegions(region);
  inputImageB->Allocate();

  // Initialize Image C
  inputImageC->SetRegions(region);
  inputImageC->Allocate();

  // Declare Iterator types apropriated for each image
  using myIteratorType1 = itk::ImageRegionIterator<myImageType1>;
  using myIteratorType2 = itk::ImageRegionIterator<myImageType2>;
  using myIteratorType3 = itk::ImageRegionIterator<myImageType3>;

  // Create one iterator for Image A (this is a light object)
  myIteratorType1 it1(inputImageA, region);

  // Initialize the content of Image A
  std::cout << "Image #1 " << std::endl;
  while (!it1.IsAtEnd())
  {
    it1.Set(static_cast<char>(vnl_sample_uniform(0, 255)));
    std::cout << static_cast<int>(it1.Get()) << std::endl;
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  myIteratorType2 it2(inputImageB, region);

  // Initialize the content of Image B
  std::cout << std::endl;
  std::cout << "Image #2 " << std::endl;
  itk::Vector<unsigned short, 2> vec;
  while (!it2.IsAtEnd())
  {
    vec[0] = static_cast<unsigned short>(vnl_sample_uniform(0, 32765));
    vec[1] = static_cast<unsigned short>(vnl_sample_uniform(0, 32765));
    it2.Set(vec);
    std::cout << it2.Get() << std::endl;
    ++it2;
  }

  // Create one iterator for Image C (this is a light object)
  myIteratorType3 itRGBA(inputImageC, region);

  // Initialize the content of Image C
  std::cout << std::endl;
  std::cout << "Image #3 " << std::endl;
  itk::RGBAPixel<short> rgbaVec;
  while (!itRGBA.IsAtEnd())
  {
    rgbaVec[0] = static_cast<short>(vnl_sample_uniform(0, 255));
    rgbaVec[1] = static_cast<short>(vnl_sample_uniform(0, 255));
    rgbaVec[2] = static_cast<short>(vnl_sample_uniform(0, 255));
    rgbaVec[3] = static_cast<short>(vnl_sample_uniform(0, 255));
    itRGBA.Set(rgbaVec);
    //  std::cout << itRGBA.Get() << std::endl;
    ++itRGBA;
  }

  // Declare the types for the Join Filters
  using myFilterType = itk::JoinImageFilter<myImageType1, myImageType2>;
  using myFilterType1 = itk::JoinImageFilter<myImageType2, myImageType1>;
  using myFilterType2 = itk::JoinImageFilter<myImageType1, myImageType1>;
  using myFilterType3 = itk::JoinImageFilter<myFilterType::OutputImageType, myImageType3>;
  using myFilterType4 = itk::JoinImageFilter<myImageType2, myImageType2>;

  using myOutputIteratorType = itk::ImageRegionIterator<myFilterType::OutputImageType>;
  using myOutputIteratorType1 = itk::ImageRegionIterator<myFilterType1::OutputImageType>;
  using myOutputIteratorType2 = itk::ImageRegionIterator<myFilterType2::OutputImageType>;
  using myOutputIteratorType3 = itk::ImageRegionIterator<myFilterType3::OutputImageType>;
  using myOutputIteratorType4 = itk::ImageRegionIterator<myFilterType4::OutputImageType>;


  //
  // Join image #1 and #2
  //

  // Setup a JoinImageFilter
  auto filter = myFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, JoinImageFilter, BinaryGeneratorImageFilter);


  filter->SetInput1(inputImageA);
  filter->SetInput2(inputImageB);

  //
  // Join image #1#2 and #3
  //

  // Setup a JoinImageFilter
  auto filter123 = myFilterType3::New();
  filter123->SetInput1(filter->GetOutput());
  filter123->SetInput2(inputImageC);
  filter123->Update(); // This Update will force filter to execute, then filter123

  // Create an iterator for going through the image #1#2
  myFilterType::OutputImageType::Pointer outputImage = filter->GetOutput();
  myOutputIteratorType                   it3(outputImage, outputImage->GetRequestedRegion());

  //  Print the content of the result image
  std::cout << std::endl;
  std::cout << "Joining #1 and #2 image " << std::endl;
  while (!it3.IsAtEnd())
  {
    std::cout << it3.Get() << std::endl;
    ++it3;
  }

  // Create an iterator for going through the image #1#2#3
  myFilterType3::OutputImageType::Pointer outputImage123 = filter123->GetOutput();
  myOutputIteratorType3                   it123(outputImage123, outputImage123->GetRequestedRegion());

  //  Print the content of the result image
  std::cout << std::endl;
  std::cout << "Joining #1#2 and #3 image " << std::endl;
  while (!it123.IsAtEnd())
  {
    std::cout << it123.Get() << std::endl;
    ++it123;
  }

  //
  // Join image #2 and #1
  //

  // Setup a JoinImageFilter
  auto filter1 = myFilterType1::New();
  filter1->SetInput1(inputImageB);
  filter1->SetInput2(inputImageA);
  filter1->Update();

  // Create an iterator for going through the image output
  myFilterType1::OutputImageType::Pointer outputImage1 = filter1->GetOutput();
  myOutputIteratorType1                   it4(outputImage1, outputImage1->GetRequestedRegion());

  //  Print the content of the result image
  std::cout << std::endl;
  std::cout << "Joining #2 and #1 image " << std::endl;
  while (!it4.IsAtEnd())
  {
    std::cout << it4.Get() << std::endl;
    ++it4;
  }

  //
  // Join image #1 and #1
  //

  // Setup a JoinImageFilter
  auto filter2 = myFilterType2::New();
  filter2->SetInput1(inputImageA);
  filter2->SetInput2(inputImageA);
  filter2->Update();

  // Create an iterator for going through the image output
  myFilterType2::OutputImageType::Pointer outputImage2 = filter2->GetOutput();
  myOutputIteratorType2                   it5(outputImage2, outputImage2->GetRequestedRegion());

  //  Print the content of the result image
  //  std::cout << std::endl;
  std::cout << "Joining #1 and #1 image " << std::endl;
  while (!it5.IsAtEnd())
  {
    std::cout << static_cast<int>(it5.Get()[0]) << "  " << static_cast<int>(it5.Get()[1]) << std::endl;
    ++it5;
  }

  //
  // Join image #2 and #2
  //

  // Setup a JoinImageFilter
  auto filter4 = myFilterType4::New();
  filter4->SetInput1(inputImageB);
  filter4->SetInput2(inputImageB);
  filter4->Update();

  // Create an iterator for going through the image output
  myFilterType4::OutputImageType::Pointer outputImage4 = filter4->GetOutput();
  myOutputIteratorType4                   it6(outputImage4, outputImage4->GetRequestedRegion());

  //  Print the content of the result image
  //  std::cout << std::endl;
  std::cout << "Joining #2 and #2 image " << std::endl;
  while (!it6.IsAtEnd())
  {
    std::cout << it6.Get() << std::endl;
    ++it6;
  }


  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
