/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHoughTransform2DLinesImageTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkHoughTransform2DLinesImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkThresholdImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include <itkGradientMagnitudeImageFilter.h>
#include <itkDiscreteGaussianImageFilter.h>
#include <itkCastImageFilter.h>
#include <list>

/**
 * This program looks for straight lines whithin an image
 * It uses the ITK HoughTransform2DLinesImageFilter.
 * - Read the image.
 * - Apply a gradient and thresholding functions.
 * - Compute the accumulator by running the filter.
 * - Blur the accumulator.
 * - Find maxima in the accumulator.
 * - Display the results
 */

/** Hough Point structure */
struct houghPoint
{
  double radius;
  double angle;
};

/** Main program */
int itkHoughTransform2DLinesImageTest(int, char* [])
{
  /** Typedefs */
  typedef   unsigned char                            PixelType;
  typedef   double                                   HoughSpacePixelType;
  typedef   itk::Image< HoughSpacePixelType, 2>      HoughImageType;
  typedef   itk::Image< PixelType, 2>                ImageType;
  itk::Index<2> m_Index;

  /** Create a line image with one line */
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


  /** Create a line */
  float teta = 0.20; // radians
  float radius = 50;
  
  double Vx = radius*cos( teta );
  double Vy = radius*sin( teta );

  double norm = sqrt(Vx*Vx+Vy*Vy);
  double VxNorm = Vx/norm;
  double VyNorm = Vy/norm;
       
  unsigned int maxval = size[0]*size[1];

    
  for(unsigned int i=0;i<maxval;i+=1)
  {    
    m_Index[0]=(long int)(Vx-VyNorm*i);
    m_Index[1]=(long int)(Vy+VxNorm*i);

    if( ((m_Index[0]<(long)size[0]) && (m_Index[0]>=0))
         && ((m_Index[1]<(long)size[1]) && (m_Index[1]>=0))
      )
    {
       m_Image->SetPixel(m_Index,255);
    }
  } 

  /** Allocate Hough Space image (accumulator) */
  std::cout << "Allocating Hough Space Image" << std::endl;
  HoughImageType::Pointer m_HoughSpaceImage = HoughImageType::New();
  m_HoughSpaceImage->SetRegions( region );
  m_HoughSpaceImage->Allocate();

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
  unsigned char thresh_above = 200;
  threshFilter->ThresholdOutside(thresh_below,thresh_above);
  threshFilter->Update();
   
  /** Define the HoughTransform filter */
  typedef itk::HoughTransform2DLinesImageFilter<HoughSpacePixelType,HoughSpacePixelType> HoughTransformFilterType;
  
  HoughTransformFilterType::Pointer houghFilter = HoughTransformFilterType::New();

  houghFilter->SetInput(threshFilter->GetOutput());
  houghFilter->Update();
  HoughImageType::Pointer m_Accumulator= houghFilter->GetOutput();

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

  itk::ImageRegionIterator<HoughImageType> it_output(m_HoughSpaceImage,m_HoughSpaceImage->GetLargestPossibleRegion());
  itk::ImageRegionIterator<HoughImageType> it_input(m_PostProcessImage,m_PostProcessImage->GetLargestPossibleRegion());

  /** Set the number of lines we are looking for. */ 
  unsigned int m_NumberOfLines=1;
  /** Each time we find a maximum we remove it by drawing a black disc
      this define the size of this disc */
  unsigned int m_HoughDiscRadius=10;

  unsigned int lines=0;
  std::list<houghPoint> m_LinesList;

  /** Find maxima */
  do{
    minMaxCalculator->SetImage(m_PostProcessImage);
    minMaxCalculator->ComputeMaximum();
    HoughImageType::PixelType  max = minMaxCalculator->GetMaximum();
    
    for(it_input.GoToBegin();!it_input.IsAtEnd();++it_input)
    {
      if(it_input.Get() == max) 
      { 
        houghPoint m_HoughPoint;
        m_HoughPoint.radius = it_input.GetIndex()[0];
        m_HoughPoint.angle  = ((it_input.GetIndex()[1])*2*PI/houghFilter->GetAngleResolution())-PI ;
        
        m_LinesList.push_back(m_HoughPoint);
        
        // Remove a black disc from the hough space domain
        for(double angle = 0; angle <= 2*PI ; angle += PI/1000)
        {     
          for(double lenght = 0; lenght < m_HoughDiscRadius;lenght += 1)
          {
            m_Index[0] = (long int)(it_input.GetIndex()[0] + lenght * cos(angle));
            m_Index[1] = (long int)(it_input.GetIndex()[1] + lenght * sin(angle));
            if( ((m_Index[0]<=sqrt((double)400*400+400*400)) && (m_Index[0]>=0))
              && ((m_Index[1]<=500) && (m_Index[1]>=0))
            )
            {
              m_Accumulator->SetPixel(m_Index,0);
            }
          } 
        }
        minMaxCalculator->SetImage(m_Accumulator);
        minMaxCalculator->ComputeMaximum();
        max = minMaxCalculator->GetMaximum();
      
        lines++;
        if(lines == m_NumberOfLines) break;
      }
    }
  } while(lines<m_NumberOfLines);


  std::list<houghPoint>::iterator it_list = m_LinesList.begin();

  while(it_list != m_LinesList.end())
  {
    std::cout << "Angle = " << it_list->angle << " (expected " << teta << ")"<< std::endl;
    std::cout << "Radius = " << it_list->radius << " (expected " << radius << ")"<< std::endl;
    
    if(fabs(it_list->angle-teta)>0.1)
    {
      std::cout << "Failure" << std::endl;
      return EXIT_FAILURE;
    }
    if(fabs(it_list->radius-radius)>1.0)
    {
      std::cout << "Failure" << std::endl;
      return EXIT_FAILURE;
    } 
    it_list++;  
  }


  std::cout << "Hough Transform Successful" << std::endl;
  return EXIT_SUCCESS;
}



