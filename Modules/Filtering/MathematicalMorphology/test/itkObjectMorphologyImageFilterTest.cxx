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

#include <cstdlib>
#include <ctime>

#include "itkImage.h"
#include "itkIndex.h"
#include "itkDilateObjectMorphologyImageFilter.h"
#include "itkErodeObjectMorphologyImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkImageRegionIterator.h"
#include "itkMacro.h"

int
itkObjectMorphologyImageFilterTest(int, char *[])
{
  // Define the dimension of the images
  constexpr unsigned int myDimension = 3;

  // Define the values of the input images
  constexpr unsigned short fgValue = 1;
  constexpr unsigned short bgValue = 0;

  // Declare the types of the images
  using myImageType = itk::Image<unsigned short, myDimension>;

  // Declare the type of the index to access images
  using myIndexType = itk::Index<myDimension>;

  // Declare the type of the size
  using mySizeType = itk::Size<myDimension>;

  // Declare the type of the Region
  using myRegionType = itk::ImageRegion<myDimension>;

  // Create an image
  auto inputImage = myImageType::New();

  // Define their size, and start index
  mySizeType size;
  size[0] = 20;
  size[1] = 20;
  size[2] = 20;

  myIndexType index;
  index[0] = 0;
  index[1] = 0;
  index[2] = 0;

  myRegionType region{ index, size };

  // Initialize Image
  inputImage->SetRegions(region);
  inputImage->Allocate();

  // Declare Iterator types apropriated for each image
  using myIteratorType = itk::ImageRegionIterator<myImageType>;

  // Initialize the content of Image
  inputImage->FillBuffer(bgValue);

  myImageType::IndexType ind;
  ind[0] = 10;
  ind[1] = 10;
  ind[2] = 10;
  inputImage->SetPixel(ind, fgValue);
  ind[0] = 2;
  ind[1] = 2;
  ind[2] = 8;
  inputImage->SetPixel(ind, fgValue);

  ind[0] = 9;
  ind[1] = 10;
  ind[2] = 5;
  inputImage->SetPixel(ind, fgValue);

  ind[0] = 9;
  ind[1] = 0;
  ind[2] = 15;
  inputImage->SetPixel(ind, fgValue);

  ind[0] = 9;
  ind[1] = 9;
  ind[2] = 7;
  inputImage->SetPixel(ind, fgValue);

  ind[0] = 0;
  ind[1] = 4;
  ind[2] = 17;
  inputImage->SetPixel(ind, fgValue);

  // Declare the type for the structuring element
  using myKernelType = itk::BinaryBallStructuringElement<unsigned short, myDimension>;

  // Declare the type for the morphology Filter
  using myDilateFilterType = itk::DilateObjectMorphologyImageFilter<myImageType, myImageType, myKernelType>;
  using binDilateFilterType = itk::BinaryDilateImageFilter<myImageType, myImageType, myKernelType>;


  using myErodeFilterType = itk::ErodeObjectMorphologyImageFilter<myImageType, myImageType, myKernelType>;

  using binErodeFilterType = itk::BinaryErodeImageFilter<myImageType, myImageType, myKernelType>;

  // Create the filter
  auto dilateFilter = myDilateFilterType::New();
  auto erodeFilter = myErodeFilterType::New();
  auto binDilateFilter = binDilateFilterType::New();
  auto binErodeFilter = binErodeFilterType::New();

  // Create the structuring element
  myKernelType           ball;
  myKernelType::SizeType ballSize;
  ballSize[0] = 5;
  ballSize[1] = 4;
  ballSize[2] = 3;
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  // Connect the input image
  dilateFilter->SetInput(inputImage);
  dilateFilter->SetKernel(ball);
  dilateFilter->SetObjectValue(fgValue);
  myImageType::Pointer outputImage = dilateFilter->GetOutput();

  clock_t start, end;
  double  elapsedTime;

  // Execute the filter
  std::cout << "Object Dilate..." << '\n';

  start = clock();

  ITK_TRY_EXPECT_NO_EXCEPTION(dilateFilter->Update());


  end = clock();

  elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);

  // Print the content of the result image
  std::cout << "  Success: " << '\n';
  std::cout << "    Time = " << elapsedTime << '\n';

  binDilateFilter->SetInput(inputImage);
  binDilateFilter->SetKernel(ball);
  binDilateFilter->SetDilateValue(fgValue);
  myImageType::Pointer outputBinImage = binDilateFilter->GetOutput();

  // Execute the filter
  std::cout << "Binary Dilate..." << '\n';

  start = clock();

  ITK_TRY_EXPECT_NO_EXCEPTION(binDilateFilter->Update());


  end = clock();

  elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);

  // Print the content of the result image
  std::cout << "  Success: " << '\n';
  std::cout << "    Time = " << elapsedTime << '\n';

  // Create an iterator for going through the image output
  myIteratorType itObj(outputImage, outputImage->GetBufferedRegion());
  myIteratorType itBin(outputBinImage, outputBinImage->GetBufferedRegion());
  std::cout << "Test for Dilate equality..." << '\n';
  start = clock();
  itObj.GoToBegin();
  itBin.GoToBegin();
  int count = 0;
  while (!itObj.IsAtEnd() && !itBin.IsAtEnd())
  {
    if (itObj.Get() != itBin.Get())
    {
      std::cerr << "Error: Dilated images differ!" << '\n';
      std::cerr << "   Slice = " << count / (size[1] * size[0]) << '\n';
      unsigned int  x, y;
      itk::Index<3> i;
      i[2] = count / (size[1] * size[0]);
      for (y = 0; y < size[1]; ++y)
      {
        i[1] = y;
        for (x = 0; x < size[0]; ++x)
        {
          i[0] = x;
          std::cerr << outputImage->GetPixel(i) << outputBinImage->GetPixel(i) << ' ';
        }
        std::cerr << '\n';
      }
      return -1;
    }
    ++itObj;
    ++itBin;
    ++count;
  }
  end = clock();
  elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);
  std::cout << "  Success: " << '\n';
  std::cout << "    Time = " << elapsedTime << '\n';

  ballSize[0] = 2;
  ballSize[1] = 2;
  ballSize[2] = 2;
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  // Connect the input image
  erodeFilter->SetInput(outputImage);
  erodeFilter->SetKernel(ball);
  erodeFilter->SetObjectValue(fgValue);
  erodeFilter->SetBackgroundValue(bgValue);
  myImageType::Pointer output2Image = erodeFilter->GetOutput();

  // Execute the filter
  std::cout << "Object Erode..." << '\n';

  start = clock();

  ITK_TRY_EXPECT_NO_EXCEPTION(erodeFilter->Update());


  end = clock();

  elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);

  // Print the content of the result image
  std::cout << "  Success: " << '\n';
  std::cout << "    Time = " << elapsedTime << '\n';

  binErodeFilter->SetInput(outputImage);
  binErodeFilter->SetKernel(ball);
  binErodeFilter->SetErodeValue(fgValue);
  myImageType::Pointer outputBin2Image = binErodeFilter->GetOutput();

  // Execute the filter
  std::cout << "Binary Erode..." << '\n';

  start = clock();

  ITK_TRY_EXPECT_NO_EXCEPTION(binErodeFilter->Update());


  end = clock();

  elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);

  // Print the content of the result image
  std::cout << "  Success: " << '\n';
  std::cout << "    Time = " << elapsedTime << '\n';

  // Create an iterator for going through the image output
  myIteratorType it2Obj(output2Image, output2Image->GetBufferedRegion());
  myIteratorType it2Bin(outputBin2Image, outputBin2Image->GetBufferedRegion());
  std::cout << "Test for Erode equality..." << '\n';
  start = clock();
  count = 0;
  while (!it2Obj.IsAtEnd())
  {
    if (it2Obj.Get() != it2Bin.Get())
    {
      std::cout << "As expected: Error: Eroded images differ!" << '\n';
      std::cout << "  Please see documentation - ErodeObject and BinaryErode";
      std::cout << '\n' << "    produce different results" << '\n';
      std::cout << "   Slice = " << count / (size[1] * size[0]) << '\n';
      unsigned int  x, y;
      itk::Index<3> i;
      i[2] = count / (size[1] * size[0]);
      for (y = 0; y < size[1]; ++y)
      {
        i[1] = y;
        for (x = 0; x < size[0]; ++x)
        {
          i[0] = x;
          std::cout << output2Image->GetPixel(i) << outputBin2Image->GetPixel(i) << ' ';
        }
        std::cout << '\n';
      }
      break;
    }
    ++it2Obj;
    ++it2Bin;
    ++count;
  }
  end = clock();
  elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);
  std::cout << "  Success: " << '\n';
  std::cout << "    Time = " << elapsedTime << '\n';

  // All objects should be automatically destroyed at this point

  std::cout << "Test finished." << '\n';
  return EXIT_SUCCESS;
}
