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

#include <stdlib.h>
#include <time.h>

#include "itkImage.h"
#include "itkIndex.h"
#include "itkDilateObjectMorphologyImageFilter.h"
#include "itkErodeObjectMorphologyImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkImageRegionIterator.h"
#include "itkMacro.h"

int itkObjectMorphologyImageFilterTest(int, char* [] )
{
  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Define the values of the input images
  const unsigned short fgValue = 1;
  const unsigned short bgValue = 0;

  // Declare the types of the images
  typedef itk::Image<unsigned short, myDimension>  myImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>         myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension>          mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create an image
  myImageType::Pointer inputImage  = myImageType::New();

  // Define their size, and start index
  mySizeType size;
  size[0] = 20;
  size[1] = 20;
  size[2] = 20;

  myIndexType index;
  index[0] = 0;
  index[1] = 0;
  index[2] = 0;

  myRegionType region;
  region.SetIndex( index );
  region.SetSize( size );

  // Initialize Image
  inputImage->SetRegions( region );
  inputImage->Allocate();

  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIterator<myImageType>  myIteratorType;

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
  typedef itk::BinaryBallStructuringElement<unsigned short, myDimension>
    myKernelType;

  // Declare the type for the morphology Filter
  typedef itk::DilateObjectMorphologyImageFilter<myImageType, myImageType,
                                                 myKernelType>
    myDilateFilterType;
  typedef itk::BinaryDilateImageFilter<myImageType, myImageType,
                                                 myKernelType>
    binDilateFilterType;


  typedef itk::ErodeObjectMorphologyImageFilter<myImageType, myImageType,
                                                 myKernelType>
    myErodeFilterType;

  typedef itk::BinaryErodeImageFilter<myImageType, myImageType,
                                                 myKernelType>
    binErodeFilterType;

  // Create the filter
  myDilateFilterType::Pointer dilateFilter = myDilateFilterType::New();
  myErodeFilterType::Pointer erodeFilter = myErodeFilterType::New();
  binDilateFilterType::Pointer binDilateFilter = binDilateFilterType::New();
  binErodeFilterType::Pointer binErodeFilter = binErodeFilterType::New();

  // Create the structuring element
  myKernelType ball;
  myKernelType::SizeType ballSize;
  ballSize[0] = 5;
  ballSize[1] = 4;
  ballSize[2] = 3;
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  // Connect the input image
  dilateFilter->SetInput( inputImage );
  dilateFilter->SetKernel( ball );
  dilateFilter->SetObjectValue( fgValue );
  myImageType::Pointer outputImage = dilateFilter->GetOutput();

  clock_t start, end;
  double elapsedTime;

  // Execute the filter
  try
    {
    std::cout << "Object Dilate..." << std::endl;
    start = clock();
    dilateFilter->Update();
    end = clock();

    elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

    //  Print the content of the result image
    std::cout << "  Success: " << std::endl;
    std::cout << "    Time = " << elapsedTime << std::endl;
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception caught during dilate filter Update\n"  << e;
    return -1;
    }

  binDilateFilter->SetInput( inputImage );
  binDilateFilter->SetKernel( ball );
  binDilateFilter->SetDilateValue( fgValue );
  myImageType::Pointer outputBinImage = binDilateFilter->GetOutput();
  try
    {
    std::cout << "Binary Dilate..." << std::endl;

    start = clock();
    binDilateFilter->Update();
    end = clock();

    elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

    //  Print the content of the result image
    std::cout << "  Success: " << std::endl;
    std::cout << "    Time = " << elapsedTime << std::endl;
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception caught during dilate filter Update\n"  << e;
    return -1;
    }

  // Create an iterator for going through the image output
  myIteratorType itObj(outputImage, outputImage->GetBufferedRegion());
  myIteratorType itBin(outputBinImage, outputBinImage->GetBufferedRegion());
  std::cout << "Test for Dilate equality..." << std::endl;
  start = clock();
  itObj.GoToBegin();
  itBin.GoToBegin();
  int count = 0;
  while( !itObj.IsAtEnd() && !itBin.IsAtEnd() )
    {
    if(itObj.Get() != itBin.Get())
      {
      std::cerr << "Error: Dilated images differ!" << std::endl;
      std::cerr << "   Slice = " << count/(size[1]*size[0]) << std::endl;
      unsigned int x, y;
      itk::Index<3> i;
      i[2] = count/(size[1]*size[0]);
      for(y=0; y<size[1]; y++)
        {
        i[1] = y;
        for(x=0; x<size[0]; x++)
          {
          i[0] = x;
          std::cerr << outputImage->GetPixel(i)
                    << outputBinImage->GetPixel(i) << " ";
          }
        std::cerr << std::endl;
        }
      return -1;
      }
    ++itObj;
    ++itBin;
    ++count;
    }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;
  std::cout << "  Success: " << std::endl;
  std::cout << "    Time = " << elapsedTime << std::endl;

  ballSize[0] = 2;
  ballSize[1] = 2;
  ballSize[2] = 2;
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  // Connect the input image
  erodeFilter->SetInput( outputImage );
  erodeFilter->SetKernel( ball );
  erodeFilter->SetObjectValue( fgValue );
  erodeFilter->SetBackgroundValue( bgValue );
  myImageType::Pointer output2Image = erodeFilter->GetOutput();

  // Execute the filter
  try
    {
    std::cout << "Object Erode..." << std::endl;
    start = clock();
    erodeFilter->Update();
    end = clock();

    elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

    //  Print the content of the result image
    std::cout << "  Success: " << std::endl;
    std::cout << "    Time = " << elapsedTime << std::endl;
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception caught during erode filter Update\n"  << e;
    return -1;
    }

  binErodeFilter->SetInput( outputImage );
  binErodeFilter->SetKernel( ball );
  binErodeFilter->SetErodeValue( fgValue );
  myImageType::Pointer outputBin2Image = binErodeFilter->GetOutput();

  // Execute the filter
  try
    {
    std::cout << "Binary Erode..." << std::endl;
    start = clock();
    binErodeFilter->Update();
    end = clock();

    elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

    //  Print the content of the result image
    std::cout << "  Success: " << std::endl;
    std::cout << "    Time = " << elapsedTime << std::endl;
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception caught during erode filter Update\n"  << e;
    return -1;
    }

  // Create an iterator for going through the image output
  myIteratorType it2Obj(output2Image, output2Image->GetBufferedRegion());
  myIteratorType it2Bin(outputBin2Image, outputBin2Image->GetBufferedRegion());
  std::cout << "Test for Erode equality..." << std::endl;
  start = clock();
  count = 0;
  while( !it2Obj.IsAtEnd() )
    {
    if(it2Obj.Get() != it2Bin.Get())
      {
      std::cout << "As expected: Error: Eroded images differ!" << std::endl;
      std::cout << "  Please see documentation - ErodeObject and BinaryErode";
      std::cout << std::endl << "    produce different results" << std::endl;
      std::cout << "   Slice = " << count/(size[1]*size[0]) << std::endl;
      unsigned int x, y;
      itk::Index<3> i;
      i[2] = count/(size[1]*size[0]);
      for(y=0; y<size[1]; y++)
        {
        i[1] = y;
        for(x=0; x<size[0]; x++)
          {
          i[0] = x;
          std::cout << output2Image->GetPixel(i)
                    << outputBin2Image->GetPixel(i) << " ";
          }
        std::cout << std::endl;
        }
      break;
      }
    ++it2Obj;
    ++it2Bin;
    ++count;
    }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;
  std::cout << "  Success: " << std::endl;
  std::cout << "    Time = " << elapsedTime << std::endl;

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}
