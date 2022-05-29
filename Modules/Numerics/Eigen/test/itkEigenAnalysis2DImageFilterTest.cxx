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

#include "itkEigenAnalysis2DImageFilter.h"
#include "itkSimpleFilterWatcher.h"

template <unsigned int myDimension = 2,
          typename TComputeType = double,
          typename myVectorType = itk::Vector<TComputeType, myDimension>>
class EigenAnalysis2DImageFilterTester
{
  // Define the dimension of the images
  // Declare type for Eigen Vectors

  // Declare the types of the images
  using myImageType = itk::Image<TComputeType, myDimension>;
  using myVectorImageType = itk::Image<myVectorType, myDimension>;

  // Declare the type of the index to access images
  using myIndexType = itk::Index<myDimension>;

  // Declare the type of the size
  using mySizeType = itk::Size<myDimension>;

  // Declare the type of the Region
  using myRegionType = itk::ImageRegion<myDimension>;


  // Declare Iterator types apropriated for each image
  using myIteratorType = itk::ImageRegionIteratorWithIndex<myImageType>;
  using myVectorIteratorType = itk::ImageRegionIteratorWithIndex<myVectorImageType>;


  // Declare the Filter
  using myFilterType = itk::EigenAnalysis2DImageFilter<myImageType, myImageType, myVectorImageType>;

  // Function for image initialization
  void
  InitializeImage(myImageType * image, double value)
  {

    typename myImageType::Pointer inputImage(image);

    // Define their size, and start index
    mySizeType size;
    size.Fill(2);

    myIndexType start;
    start.Fill(0);

    myRegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    inputImage->SetRegions(region);
    inputImage->Allocate();

    myIteratorType it(inputImage, inputImage->GetRequestedRegion());

    it.GoToBegin();
    while (!it.IsAtEnd())
    {
      it.Set(value);
      ++it;
    }
  }

  // Function for image printing
  void
  PrintImage(myImageType * image, const char * text)
  {

    typename myImageType::Pointer imagePtr(image);

    // Create an iterator for going through the image
    myIteratorType it(imagePtr, imagePtr->GetRequestedRegion());

    it.GoToBegin();

    //  Print the content of the image
    std::cout << text << std::endl;
    while (!it.IsAtEnd())
    {
      std::cout << it.Get() << std::endl;
      ++it;
    }
  }


  // Function for image printing
  void
  PrintImage(myVectorImageType * image, const char * text)
  {

    typename myVectorImageType::Pointer imagePtr(image);

    // Create an iterator for going through the image
    myVectorIteratorType it(imagePtr, imagePtr->GetRequestedRegion());

    it.GoToBegin();

    //  Print the content of the image
    std::cout << text << std::endl;
    while (!it.IsAtEnd())
    {
      std::cout << it.Get() << std::endl;
      ++it;
    }
  }

public:
  int
  Run()
  {
    // Create the images
    auto inputImageXX = myImageType::New();
    auto inputImageXY = myImageType::New();
    auto inputImageYY = myImageType::New();


    InitializeImage(inputImageXX, std::cos(itk::Math::pi / 6.0));
    InitializeImage(inputImageXY, std::sin(itk::Math::pi / 6.0));
    InitializeImage(inputImageYY, std::cos(itk::Math::pi / 6.0));


    // Create a  Filter
    auto                     filter = myFilterType::New();
    itk::SimpleFilterWatcher watcher(filter);

    // Connect the input images
    filter->SetInput1(inputImageXX);
    filter->SetInput2(inputImageXY);
    filter->SetInput3(inputImageYY);


    // Execute the filter
    filter->Update();

    // Get
    typename myImageType::Pointer maxEigenValue = filter->GetMaxEigenValue();
    typename myImageType::Pointer minEigenValue = filter->GetMinEigenValue();

    typename myVectorImageType::Pointer maxEigenVector = filter->GetMaxEigenVector();

    PrintImage(maxEigenValue, "Max Eigen Value");
    PrintImage(minEigenValue, "Min Eigen Value");
    PrintImage(maxEigenVector, "Max Eigen Vector");

    // All objects should be automatically destroyed at this point
    return EXIT_SUCCESS;
  }
};

int
itkEigenAnalysis2DImageFilterTest(int, char *[])
{
  int status = EXIT_SUCCESS;
  {
    using myComputeType = double;
    constexpr unsigned int myDimension = 2;
    using myVectorType = itk::Vector<myComputeType, 2>;
    auto t1 = EigenAnalysis2DImageFilterTester<myDimension, myComputeType, myVectorType>();
    status |= t1.Run();
  }
  {
    using myComputeType = float;
    constexpr unsigned int myDimension = 2;
    using myVectorType = itk::Vector<myComputeType, 2>;
    auto t1 = EigenAnalysis2DImageFilterTester<myDimension, myComputeType, myVectorType>();
    status |= t1.Run();
  }
  {
    using myComputeType = float;
    constexpr unsigned int myDimension = 3;
    using myVectorType = itk::Vector<myComputeType, 2>;
    auto t1 = EigenAnalysis2DImageFilterTester<myDimension, myComputeType, myVectorType>();
    status |= t1.Run();
  }
  /* The two test below should cause compilation errors.
  {
    using myComputeType = float;
    constexpr unsigned int myDimension = 3;
    <-- ******* error: static assertion failed: Error: PixelType of EigenVector Image must have exactly 2 elements!
    using myVectorType = itk::Vector<myComputeType, 3>;
    auto t1 = EigenAnalysis2DImageFilterTester<myDimension, myComputeType, myVectorType>();
    status |= t1.Run();
  }
  {
    using myComputeType = float;
    constexpr unsigned int myDimension = 3;
    using myVectorType = std::array<myComputeType, 2>;
    <-- ******* error: no type named ‘ValueType’ in ‘using PixelType = struct std::array<float, 2>
    auto t1 = EigenAnalysis2DImageFilterTester<myDimension, myComputeType, myVectorType>();
    status |= t1.Run();
  }
  */
  return status;
}
