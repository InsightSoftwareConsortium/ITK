/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHoughTransform2DCirclesImageTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkHoughTransform2DCirclesImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkThresholdImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include <itkGradientMagnitudeImageFilter.h>
#include <itkDiscreteGaussianImageFilter.h>
#include <itkCastImageFilter.h>

int itkHoughTransform2DCirclesImageTest(int, char* [])
{
  /** Typedefs */
  typedef   unsigned char                            PixelType;
  typedef   double                                   HoughSpacePixelType;
  typedef   itk::Image< HoughSpacePixelType, 2>      HoughImageType;
  typedef   itk::Image< PixelType, 2>                ImageType;

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
  m_Image->Allocate();
  m_Image->FillBuffer(0);

  /** Create 3 circles */
  unsigned int center[3][2];
  double radius[3];
  center[0][0]=50;
  center[0][1]=50;
  radius[0] = 20;

  for(double i=0;i<=radius[0];i+=0.1)
  {
    for(double angle = 0; angle <= 2*PI ; angle += PI/1000)
    {
      index[0] = (long int)(center[0][0] + i * cos(angle));
      index[1] = (long int)(center[0][1] + i * sin(angle));
      m_Image->SetPixel(index,255);
    }
  }

  center[1][0]=25;
  center[1][1]=25;
  radius[1] = 10;

  for(double i=0;i<=radius[1];i+=0.1)
  {
    for(double angle = 0; angle <= 2*PI ; angle += PI/1000)
    {
      index[0] = (long int)(center[1][0] + i * cos(angle));
      index[1] = (long int)(center[1][1] + i * sin(angle));
      m_Image->SetPixel(index,255);
    }
  }

  center[2][0]=71;
  center[2][1]=72;
  radius[2] = 5;

  for(double i=0;i<=radius[2];i+=0.1)
  {
    for(double angle = 0; angle <= 2*PI ; angle += PI/1000)
    {
      index[0] = (long int)(center[2][0] + i * cos(angle));
      index[1] = (long int)(center[2][1] + i * sin(angle));
      m_Image->SetPixel(index,255);
    }
  }

  /** Allocate Hough Space image (accumulator) */
  std::cout << "Allocating Hough Space Image" << std::endl;
  ImageType::Pointer m_HoughSpaceImage = ImageType::New();
  m_HoughSpaceImage->SetRegions( region );
  m_HoughSpaceImage->Allocate();
  m_HoughSpaceImage->FillBuffer(0);

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
  houghFilter->SetMaximumRadius(25);
  houghFilter->SetSigmaGradient(1);
  houghFilter->Update();
  HoughImageType::Pointer m_Accumulator= houghFilter->GetOutput();


  HoughImageType::Pointer m_RadiusImage= houghFilter->GetRadiusImage();

  /** Blur the accumulator in order to find the maximum */
  HoughImageType::Pointer m_PostProcessImage = HoughImageType::New();
  typedef itk::DiscreteGaussianImageFilter<HoughImageType,HoughImageType> GaussianFilterType;
  GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();
  gaussianFilter->SetInput(m_Accumulator);
  double variance[2];
  variance[0]=10;
  variance[1]=10;
  gaussianFilter->SetVariance(variance);
  gaussianFilter->SetMaximumError(.01f);
  gaussianFilter->Update();
  m_PostProcessImage = gaussianFilter->GetOutput();

  typedef itk::MinimumMaximumImageCalculator<HoughImageType> MinMaxCalculatorType;
  MinMaxCalculatorType::Pointer minMaxCalculator = MinMaxCalculatorType::New();

  itk::ImageRegionIterator<ImageType> it_output(m_HoughSpaceImage,m_HoughSpaceImage->GetLargestPossibleRegion());
  itk::ImageRegionIterator<HoughImageType> it_input(m_PostProcessImage,m_PostProcessImage->GetLargestPossibleRegion());

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
  minMaxCalculator->SetImage(m_PostProcessImage);
  minMaxCalculator->ComputeMaximum();
  HoughImageType::PixelType   max = minMaxCalculator->GetMaximum();

  it_output.GoToBegin();
  for(it_input.GoToBegin();!it_input.IsAtEnd();++it_input)
  {
    if(it_input.Get() == max) 
    {
      it_output.Set(255);
      double radius = m_RadiusImage->GetPixel(it_output.GetIndex());
      center_result[circles][0]=it_output.GetIndex()[0];
      center_result[circles][1]=it_output.GetIndex()[1];
      radius_result[circles]=radius;

      /** Draw the circle */
      for(double angle = 0; angle <= 2*PI ; angle += PI/1000)
      {
        index[0] = (long int)(it_output.GetIndex()[0] + radius * cos(angle));
        index[1] = (long int)(it_output.GetIndex()[1] + radius * sin(angle));
        m_HoughSpaceImage->SetPixel(index,255);
        
        /** Remove the maximum from the accumulator */
        for(double lenght = 0; lenght < discRatio*radius;lenght+=1)
        {
          index[0] = (long int)(it_output.GetIndex()[0] + lenght * cos(angle));
          index[1] = (long int)(it_output.GetIndex()[1] + lenght* sin(angle));
          m_PostProcessImage->SetPixel(index,0);
        } 
      }

      minMaxCalculator->SetImage(m_PostProcessImage);
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
     if((fabs((double)(center_result[i][0])-(double)(center[i][0]))>2.0) ||
        (fabs((double)(center_result[i][1])-(double)(center[i][1]))>2.0) ||
        (fabs((double)(radius_result[i]-radius[i]))>2.0)
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
