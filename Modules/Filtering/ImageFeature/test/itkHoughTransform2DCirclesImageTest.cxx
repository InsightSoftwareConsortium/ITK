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

#include "itkHoughTransform2DCirclesImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkCastImageFilter.h"

int itkHoughTransform2DCirclesImageTest(int, char* [])
{
  /** Typedefs */
  typedef   unsigned char                            PixelType;
  typedef   double                                   HoughSpacePixelType;
  typedef   itk::Image< HoughSpacePixelType, 2>      HoughImageType;
  typedef   itk::Image< PixelType, 2>                ImageType;

  const double nPI = 4.0 * std::atan( 1.0 );

  /** Create a black image */
  std::cout << "Creating simulated image" << std::endl;
  ImageType::Pointer m_Image = ImageType::New();
  ImageType::RegionType region;
  ImageType::SizeType size;
  size.Fill(100);
  ImageType::IndexType index;
  index.Fill(0);
  region.SetSize(size);
  region.SetIndex(index);
  m_Image->SetRegions( region );
  m_Image->Allocate(true); // initialize buffer
                                                  // to zero

  /** Create 3 circles */
  unsigned int center[3][2];
  double radius[3];
  center[0][0]=50;
  center[0][1]=50;
  radius[0] = 15;

  for(double i=0;i<=radius[0];i+=0.1)
  {
    for(double angle = 0; angle <= 2 * nPI; angle += nPI / 1000 )
    {
      index[0] = (long int)(center[0][0] + i * std::cos(angle));
      index[1] = (long int)(center[0][1] + i * std::sin(angle));
      m_Image->SetPixel(index,255);
    }
  }

  center[1][0]=25;
  center[1][1]=25;
  radius[1] = 7;

  for(double i=0;i<=radius[1];i+=0.1)
  {
    for(double angle = 0; angle <= 2 * nPI; angle += nPI / 1000 )
    {
      index[0] = (long int)(center[1][0] + i * std::cos(angle));
      index[1] = (long int)(center[1][1] + i * std::sin(angle));
      m_Image->SetPixel(index,255);
    }
  }

  center[2][0]=71;
  center[2][1]=72;
  radius[2] = 5;

  for(double i=0;i<=radius[2];i+=0.1)
  {
    for(double angle = 0; angle <= 2 * nPI; angle += nPI / 1000)
    {
      index[0] = (long int)(center[2][0] + i * std::cos(angle));
      index[1] = (long int)(center[2][1] + i * std::sin(angle));
      m_Image->SetPixel(index,255);
    }
  }

  /** Allocate Hough Space image (accumulator) */
  std::cout << "Allocating Hough Space Image" << std::endl;
  ImageType::Pointer m_HoughSpaceImage = ImageType::New();
  m_HoughSpaceImage->SetRegions( region );
  m_HoughSpaceImage->Allocate(true); // initialize
                                                            // buffer
                                                            // to zero

  /** Apply gradient filter to the input image */
  typedef itk::CastImageFilter<
                        ImageType,
                        HoughImageType    >    CastingFilterType;

  CastingFilterType::Pointer caster = CastingFilterType::New();
  caster->SetInput(m_Image);


  std::cout << "Applying gradient magnitude filter" << std::endl;
  typedef itk::GradientMagnitudeImageFilter<HoughImageType,HoughImageType> GradientFilterType;
  GradientFilterType::Pointer gradFilter =  GradientFilterType::New();
  gradFilter->SetInput(caster->GetOutput());
  gradFilter->Update();

  /** Apply a threshold to the Grad(InputImage) */
  std::cout << "Thresholding" << std::endl;
  typedef itk::ThresholdImageFilter<HoughImageType> ThresholdFilterType;
  ThresholdFilterType::Pointer threshFilter = ThresholdFilterType::New();
  threshFilter->SetInput(gradFilter->GetOutput());
  threshFilter->SetOutsideValue(0);
  unsigned char thresh_below = 10;
  unsigned char thresh_above = 255;
  threshFilter->ThresholdOutside(thresh_below,thresh_above);
  threshFilter->Update();

  /** Define the HoughTransform filter */
  typedef itk::HoughTransform2DCirclesImageFilter<HoughSpacePixelType,HoughSpacePixelType> HoughTransformFilterType;

  HoughTransformFilterType::Pointer houghFilter = HoughTransformFilterType::New();

  houghFilter->SetInput(threshFilter->GetOutput());

  houghFilter->SetThreshold(0.0f);
  if(houghFilter->GetThreshold() != 0.0f)
  {
    std::cout << "Failure" << std::endl;
    return EXIT_FAILURE;
  }

  houghFilter->SetMinimumRadius(0);
  houghFilter->SetMaximumRadius(20);
  houghFilter->SetSigmaGradient(1);
  houghFilter->Update();
  HoughImageType::Pointer m_Accumulator= houghFilter->GetOutput();

  HoughImageType::ConstPointer m_RadiusImage= houghFilter->GetRadiusImage();

  /** Blur the accumulator in order to find the maximum */
  typedef itk::DiscreteGaussianImageFilter<HoughImageType,HoughImageType> GaussianFilterType;
  GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();
  gaussianFilter->SetInput(m_Accumulator);
  double variance[2];
  variance[0]=10;
  variance[1]=10;
  gaussianFilter->SetVariance(variance);
  gaussianFilter->SetMaximumError(.01f);
  gaussianFilter->Update();
  HoughImageType::Pointer postProcessImage = gaussianFilter->GetOutput();

  typedef itk::MinimumMaximumImageCalculator<HoughImageType> MinMaxCalculatorType;
  MinMaxCalculatorType::Pointer minMaxCalculator = MinMaxCalculatorType::New();

  itk::ImageRegionIterator<ImageType> it_output(m_HoughSpaceImage,m_HoughSpaceImage->GetLargestPossibleRegion());
  itk::ImageRegionIterator<HoughImageType> it_input(postProcessImage,postProcessImage->GetLargestPossibleRegion());

 /** Set the number of circles we are looking for. */
  unsigned int numberOfCircles = 3;

  /** Set the disc ratio */
  double discRatio = 1.1;

  /** Search for maxima */
  std::cout << "Search for maxima ..." << std::endl;
  unsigned int center_result[3][2];
  double radius_result[3];
  unsigned int circles=0;
  do{
  minMaxCalculator->SetImage(postProcessImage);
  minMaxCalculator->ComputeMaximum();
  HoughImageType::PixelType   max = minMaxCalculator->GetMaximum();

  it_output.GoToBegin();
  for(it_input.GoToBegin();!it_input.IsAtEnd();++it_input)
  {
    if(it_input.Get() == max)
    {
      it_output.Set(255);
      double radius2 = m_RadiusImage->GetPixel(it_output.GetIndex());
      center_result[circles][0]=it_output.GetIndex()[0];
      center_result[circles][1]=it_output.GetIndex()[1];
      radius_result[circles]=radius2;

      /** Draw the circle */
      for(double angle = 0; angle <= 2 * nPI; angle += nPI / 1000)
      {
        index[0] = (long int)(it_output.GetIndex()[0] + radius2 * std::cos(angle));
        index[1] = (long int)(it_output.GetIndex()[1] + radius2 * std::sin(angle));
        m_HoughSpaceImage->SetPixel(index,255);

        /** Remove the maximum from the accumulator */
        for(double length = 0; length < discRatio*radius2;length+=1)
        {
          index[0] = (long int)(it_output.GetIndex()[0] + length * std::cos(angle));
          index[1] = (long int)(it_output.GetIndex()[1] + length * std::sin(angle));
          postProcessImage->SetPixel(index,0);
        }
      }

      minMaxCalculator->SetImage(postProcessImage);
      minMaxCalculator->ComputeMaximum();
      max = minMaxCalculator->GetMaximum();

      circles++;
      if(circles == numberOfCircles) break;
    }
    ++it_output;
    }
  }
  while(circles<numberOfCircles);

  std::cout << "Done." << std::endl;

  for(unsigned int i=0;i<3;i++)
    {
    if((std::fabs((double)(center_result[i][0])-(double)(center[i][0]))>2.0) ||
       (std::fabs((double)(center_result[i][1])-(double)(center[i][1]))>2.0) ||
       (std::fabs((double)(radius_result[i]-radius[i]))>2.0)
       )
      {
      std::cout << "Failure for circle #" << i << std::endl;
      std::cout << "Excpected center: [" << center_result[i][0] <<"," << center_result[i][1]
                << "] found [" << center[i][0] <<"," << center[i][1] << "]" << std::endl;
      std::cout << "Excpected radius: " << radius_result[i] << " found " << radius[i] << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "Circle #" << i << " [" << center_result[i][0] << ","
                << center_result[i][1] << "] -> radius = " <<  radius_result[i] << std::endl;
      }
    }

  std::cout << "Hough Transform Successful" << std::endl;
  return EXIT_SUCCESS;
}
