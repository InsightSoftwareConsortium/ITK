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
#include "itkVoronoiSegmentationRGBImageFilter.h"
#include "itkImageRegionIterator.h"
#include <iostream>
#include "itkMath.h"

// typedefs for all functions
typedef itk::RGBPixel<unsigned char>    PixelType;
typedef itk::Image<PixelType,2>         ImageType;
typedef itk::Image<unsigned char,2>     SegmentationType;
typedef itk::ImageFileReader<ImageType> ReaderType;
typedef itk::VoronoiSegmentationRGBImageFilter<ImageType, SegmentationType>
                                        FilterType;
typedef FilterType::BinaryObjectImage   BinaryObjectImage;

namespace VoronoiSegRGBTest
{

//
// global constants
//
const unsigned int width = 256;
const unsigned int height = 256;
const unsigned char bgMean = 64;
const unsigned char bgStd = 10;
const unsigned char fgMean = 128;
const unsigned char fgStd = 5;
const unsigned int objAStartX = 30;
const unsigned int objAEndX = 94;
const unsigned int objAStartY = 30;
const unsigned int objAEndY = 94;
const unsigned int objBStartX = 150;
const unsigned int objBEndX = 214;
const unsigned int objBStartY = 150;
const unsigned int objBEndY = 214;
const double minCorrectRate = .875;  // .875 is all classified as background


//
// Function to set up input image
//
ImageType::Pointer SetUpInputImage()
{
  // initialize the test input image
  ImageType::Pointer inputImage = ImageType::New();
  ImageType::SizeType size = {{width,height}};
  ImageType::RegionType region;
  region.SetSize(size);
  inputImage->SetLargestPossibleRegion(region);
  inputImage->SetBufferedRegion(region);
  inputImage->SetRequestedRegion(region);
  inputImage->Allocate();

  // add background random field
  itk::ImageRegionIterator<ImageType> iter(inputImage, region);
  while (!iter.IsAtEnd())
    {
    PixelType px;
    px[0] = (unsigned char)(vnl_sample_uniform(bgMean-bgStd,bgMean+bgStd));
    px[1] = (unsigned char)(vnl_sample_uniform(bgMean-bgStd,bgMean+bgStd));
    px[2] = (unsigned char)(vnl_sample_uniform(bgMean-bgStd,bgMean+bgStd));
    iter.Set(px);
    ++iter;
    }

  // add objects to image
  for (unsigned int x = objAStartX; x < objAEndX; x++)
    {
    for (unsigned int y = objAStartY; y < objAEndY; y++)
      {
      ImageType::IndexType idx;
      idx[0] = x;
      idx[1] = y;

      PixelType px;
      px[0] = (unsigned char)(vnl_sample_uniform(fgMean-fgStd,fgMean+fgStd));
      px[1] = (unsigned char)(vnl_sample_uniform(fgMean-fgStd,fgMean+fgStd));
      px[2] = (unsigned char)(vnl_sample_uniform(fgMean-fgStd,fgMean+fgStd));
      inputImage->SetPixel(idx, px);
      }
    }
  for (unsigned int x = objBStartX; x < objBEndX; x++)
    {
    for (unsigned int y = objBStartY; y < objBEndY; y++)
      {
      ImageType::IndexType idx;
      idx[0] = x;
      idx[1] = y;

      PixelType px;
      px[0] = (unsigned char)(vnl_sample_uniform(fgMean-fgStd,fgMean+fgStd));
      px[1] = (unsigned char)(vnl_sample_uniform(fgMean-fgStd,fgMean+fgStd));
      px[2] = (unsigned char)(vnl_sample_uniform(fgMean-fgStd,fgMean+fgStd));
      inputImage->SetPixel(idx, px);
      }
    }

  return inputImage;
}


//
// Function to check the results
//
double CheckResults(SegmentationType::Pointer outputImage)
{
  // walk the image and count the correctly segmented pixels
  unsigned int correctInterior = 0;
  unsigned int correctExterior = 0;
  unsigned int falseInterior = 0;
  unsigned int falseExterior = 0;
  for (unsigned int x = 0; x < width; x++)
    {
    for (unsigned int y = 0; y < height; y++)
      {
      SegmentationType::IndexType idx;
      idx[0] = x;
      idx[1] = y;

      bool inInterior = false;

      // check if in objA
      if (x >= objAStartX && x < objAEndX && y >= objAStartY && y < objAEndY)
        {
        inInterior = true;
        }

      // check if in objB
      else if (x >= objBStartX && x < objBEndX && y >= objBStartY && y < objBEndY)
        {
        inInterior = true;
        }

      if (inInterior)
        {
        if (outputImage->GetPixel(idx) == 1)
          {
          correctInterior++;
          }
        else
          {
          falseExterior++;
          }
        }
      else
        {
        if (outputImage->GetPixel(idx) == 0)
          {
          correctExterior++;
          }
        else
          {
          falseInterior++;
          }
        }
      }
    }

  // report the results and fail if too many incorrectly segmented
  std::cout << "Correct Interior: " << correctInterior << std::endl;
  std::cout << "Correct Exterior: " << correctExterior << std::endl;
  std::cout << "False Interior: " << falseInterior << std::endl;
  std::cout << "False Exterior: " << falseExterior << std::endl;
  double percentCorrect = (double)(correctInterior+correctExterior)/(double)(width*height);
  std::cout << "Percent Correct = " << percentCorrect *100 << "%" << std::endl;

  return percentCorrect;
}


//
// Test with no prior
//
int TestNoPrior(ImageType::Pointer inputImage)
{

  std::cout << "Beginning no-prior test" << std::endl;

  // set up the filter
  FilterType::Pointer filter = FilterType::New();
  std::cout << "Setting filter input" << std::endl;
  filter->SetInput(inputImage);

  // explicitly set mean and std
  std::cout << "Setting up mean and std for filter" << std::endl;
  double mean[6];
  mean[0] = fgMean;
  mean[1] = fgMean;
  mean[2] = fgMean;
  mean[3] = 0;
  mean[4] = 0;
  mean[5] = 50;
  filter->SetMean(mean);
  double std[6];
  std[0] = fgStd;
  std[1] = fgStd;
  std[2] = fgStd;
  std[3] = 255;
  std[4] = 10;
  std[5] = 10;
  filter->SetSTD(std);
  filter->SetNumberOfSeeds(400);
  filter->SetSteps(5);
  filter->SetMaxValueOfRGB(255);

  // test GetMaxValueOfRGB
  std::cout << "Checking GetMaxValueOfRGB" << std::endl;
  if (itk::Math::NotExactlyEquals(filter->GetMaxValueOfRGB(), 255))
    {
    std::cout << "[FAILED] Didn't set max RGB correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // run the filter
  std::cout << "Running the filter" << std::endl;
  filter->Update();

  // check the results
  std::cout << "Checking the filter results" << std::endl;
  double percentCorrect = CheckResults(filter->GetOutput());
  if (percentCorrect <= minCorrectRate)
    {
    std::cout << "[FAILED] Did not segment over "<< minCorrectRate*100 <<"% correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // return successfully
  return EXIT_SUCCESS;
}


//
// Test with a prior
//
int TestWithPrior(ImageType::Pointer inputImage)
{
  // set up the filter
  std::cout << "Setting up the filter and image" << std::endl;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(inputImage);

  // set up the prior
  std::cout << "Setting up the prior image" << std::endl;
  BinaryObjectImage::Pointer prior = BinaryObjectImage::New();
  BinaryObjectImage::SizeType size = {{width,height}};
  BinaryObjectImage::RegionType region;
  region.SetSize(size);
  prior->SetLargestPossibleRegion(region);
  prior->SetBufferedRegion(region);
  prior->SetRequestedRegion(region);
  prior->Allocate();

  // create prior as 100% segmentation
  for (unsigned int x = 0; x < width; x++)
    {
    for (unsigned int y = 0; y < height; y++)
      {
      SegmentationType::IndexType idx;
      idx[0] = x;
      idx[1] = y;

      bool inInterior = false;

      // check if in objA
      if (x >= objAStartX && x < objAEndX && y >= objAStartY && y < objAEndY)
        {
        inInterior = true;
        }

      // check if in objB
      else if (x >= objBStartX && x < objBEndX && y >= objBStartY && y < objBEndY)
        {
        inInterior = true;
        }

      if (inInterior)
        {
        prior->SetPixel(idx,1);
        }
      else
        {
        prior->SetPixel(idx,0);
        }
      }
    }

  // add prior to filter
  filter->TakeAPrior(prior);

  // run the filter
  std::cout << "Running the filter" << std::endl;
  filter->Update();

  // check the results
  std::cout << "Checking the results of the filter" << std::endl;
  double percentCorrect = CheckResults(filter->GetOutput());
  if (percentCorrect <= minCorrectRate)
    {
    std::cout << "[FAILED] Did not segment over "<< minCorrectRate*100 <<"% correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // return successfully
  return EXIT_SUCCESS;
}

} // end namespace VoronoiSegRGBTest


//
// Main test function
//
int itkVoronoiSegmentationRGBImageFilterTest(int, char* [] )
{
  // set up the input image
  ImageType::Pointer inputImage = VoronoiSegRGBTest::SetUpInputImage();

  // test without prior
  std::cout << "[Running test without prior]" << std::endl;
  int noPriorTestResult = VoronoiSegRGBTest::TestNoPrior(inputImage);
  if (noPriorTestResult == EXIT_FAILURE)
    {
    std::cout << "Failed on test without prior" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << std::endl;

  // test with prior
  std::cout << "[Running test with prior]" << std::endl;
  int priorTestResult = VoronoiSegRGBTest::TestWithPrior(inputImage);
  if (priorTestResult == EXIT_FAILURE)
    {
    std::cout << "Failed on test with prior" << std::endl;
    return EXIT_FAILURE;
    }

  // test set/get TestMean
  FilterType::Pointer filter = FilterType::New();
  filter->SetTestMean(VoronoiSegRGBTest::fgMean,VoronoiSegRGBTest::fgMean,VoronoiSegRGBTest::fgMean);
  unsigned int testMean[3];
  filter->GetTestMean(testMean);
  if (testMean[0] != VoronoiSegRGBTest::fgMean ||
      testMean[1] != VoronoiSegRGBTest::fgMean ||
      testMean[2] != VoronoiSegRGBTest::fgMean)
    {
    std::cout << "[FAILED] Didn't set/get TestMean correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // test set/get TestSTD
  filter->SetTestSTD(VoronoiSegRGBTest::fgStd,VoronoiSegRGBTest::fgStd,VoronoiSegRGBTest::fgStd);
  unsigned int testStd[3];
  filter->GetTestSTD(testStd);
  if (testStd[0] != VoronoiSegRGBTest::fgStd ||
      testStd[1] != VoronoiSegRGBTest::fgStd ||
      testStd[2] != VoronoiSegRGBTest::fgStd)
    {
    std::cout << "[FAILED] Didn't set/get TestSTD correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // test set/get MeanPercentError
  double meanPercentErrorIn[6] = {0.1,0.1,0.1,0.1,0.1,0.1};
  filter->SetMeanPercentError(meanPercentErrorIn);
  double meanPercentErrorOut[6];
  filter->GetMeanPercentError(meanPercentErrorOut);
  if (itk::Math::NotExactlyEquals(meanPercentErrorOut[0], 0.1) ||
      itk::Math::NotExactlyEquals(meanPercentErrorOut[1], 0.1) ||
      itk::Math::NotExactlyEquals(meanPercentErrorOut[2], 0.1) ||
      itk::Math::NotExactlyEquals(meanPercentErrorOut[3], 0.1) ||
      itk::Math::NotExactlyEquals(meanPercentErrorOut[4], 0.1) ||
      itk::Math::NotExactlyEquals(meanPercentErrorOut[5], 0.1))
    {
    std::cout << "[FAILED] Didn't set/get MeanPercentError correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // test set/get STDPercentError
  double stdPercentErrorIn[6] = {0.1,0.1,0.1,0.1,0.1,0.1};
  filter->SetSTDPercentError(stdPercentErrorIn);
  double stdPercentErrorOut[6];
  filter->GetSTDPercentError(stdPercentErrorOut);
  if (itk::Math::NotExactlyEquals(stdPercentErrorOut[0], 0.1) ||
      itk::Math::NotExactlyEquals(stdPercentErrorOut[1], 0.1) ||
      itk::Math::NotExactlyEquals(stdPercentErrorOut[2], 0.1) ||
      itk::Math::NotExactlyEquals(stdPercentErrorOut[3], 0.1) ||
      itk::Math::NotExactlyEquals(stdPercentErrorOut[4], 0.1) ||
      itk::Math::NotExactlyEquals(stdPercentErrorOut[5], 0.1))
    {
    std::cout << "[FAILED] Didn't set/get STDPercentError correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // test get mean/std tolerance
  double meanTolerance[6];
  filter->GetMeanTolerance(meanTolerance);
  std::cout << "Mean Tolerance = ("
            << meanTolerance[0] << ","
            << meanTolerance[1] << ","
            << meanTolerance[2] << ","
            << meanTolerance[3] << ","
            << meanTolerance[4] << ","
            << meanTolerance[5] << ")"
            << std::endl;
  double stdTolerance[6];
  filter->GetSTDTolerance(stdTolerance);
  std::cout << "STD Tolerance = ("
            << stdTolerance[0] << ","
            << stdTolerance[1] << ","
            << stdTolerance[2] << ","
            << stdTolerance[3] << ","
            << stdTolerance[4] << ","
            << stdTolerance[5] << ")"
            << std::endl;

  // test type information
  if (strcmp(filter->GetNameOfClass(),"VoronoiSegmentationRGBImageFilter") != 0)
    {
    std::cout << "[FAILED] Didn't report class name correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // test printing
  std::cout << filter << std::endl;

  // return successfully
  return EXIT_SUCCESS;

}
