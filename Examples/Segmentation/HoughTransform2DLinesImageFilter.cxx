/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    HoughTransform2DLinesImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkHoughTransform2DLinesImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkThresholdImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include <itkGradientMagnitudeImageFilter.h>
#include <itkDiscreteGaussianImageFilter.h>
#include <list>
#include "itkCastImageFilter.h"

/** Hough Point structure */
struct houghPoint
{
  double radius;
  double angle;
};

int main( int argc, char *argv[] )
{

  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0] << std::endl;
    std::cerr << " inputImage " << std::endl;  
    std::cerr << " outputImage" << std::endl;  
    std::cerr << " numberOfLines " << std::endl;  
    std::cerr << " variance of the accumulator blurring (default = 5) " << std::endl;  
    std::cerr << " radius of the disk to remove from the accumulator (default = 10) "<< std::endl;   
    return 1;
    }

  typedef   unsigned char   PixelType;
  typedef   float           AccumulatorPixelType;
  const     unsigned int    Dimension = 2;
  typedef itk::Image< PixelType, Dimension >  ImageType;
  typedef itk::Image< AccumulatorPixelType, Dimension > AccumulatorImageType;  

  typedef  unsigned char    OutputPixelType;
  typedef  itk::Image< OutputPixelType, Dimension > OutputImageType;  
  typedef  itk::ImageFileReader< ImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  typedef itk::CastImageFilter< 
                        ImageType, 
                        AccumulatorImageType    >    CastingFilterType;
  
  CastingFilterType::Pointer caster = CastingFilterType::New();

   try
    {
      reader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  ImageType::Pointer m_Image = reader->GetOutput();

  /** Allocate output */
  OutputImageType::Pointer  m_OutputImage = OutputImageType::New();

  OutputImageType::RegionType region;
  region.SetSize(m_Image->GetLargestPossibleRegion().GetSize());
  region.SetIndex(m_Image->GetLargestPossibleRegion().GetIndex());
  m_OutputImage->SetRegions( region );
  m_OutputImage->SetOrigin(m_Image->GetOrigin());
  m_OutputImage->SetSpacing(m_Image->GetSpacing());
  m_OutputImage->Allocate();
  m_OutputImage->FillBuffer(0);

  /** Aplly gradient filter to the input image */
  std::cout << "Applying gradient magnitude filter" << std::endl;
  typedef itk::GradientMagnitudeImageFilter<AccumulatorImageType,
                                            AccumulatorImageType> GradientFilterType;
  GradientFilterType::Pointer gradFilter =  GradientFilterType::New();
  caster->SetInput(m_Image);
  gradFilter->SetInput(caster->GetOutput());
  gradFilter->Update();
 
  /** Apply a threshold to the Grad(InputImage) */
  std::cout << "Thresholding" << std::endl;
  typedef itk::ThresholdImageFilter<AccumulatorImageType> ThresholdFilterType;
  ThresholdFilterType::Pointer threshFilter = ThresholdFilterType::New();
  threshFilter->SetInput( gradFilter->GetOutput());
  threshFilter->SetOutsideValue(0);
  unsigned char thresh_below = 0;
  unsigned char thresh_above = 255;
  threshFilter->ThresholdOutside(thresh_below,thresh_above);
  threshFilter->Update();
   
  /** Define the HoughTransform filter */
  std::cout << "Computing Hough Map" << std::endl;
  typedef itk::HoughTransform2DLinesImageFilter<AccumulatorPixelType,
                                                AccumulatorPixelType>   HoughTransformFilterType;
  HoughTransformFilterType::Pointer houghFilter = HoughTransformFilterType::New();
  houghFilter->SetInput(threshFilter->GetOutput());
  houghFilter->Update();
  AccumulatorImageType::Pointer m_Accumulator = houghFilter->GetOutput();
  houghFilter->SetNumberOfLines(atoi(argv[3]));
  houghFilter->SetVariance(atof(argv[4]));
  houghFilter->SetDiscRadius(atof(argv[5]));
  HoughTransformFilterType::LinesListType lines = houghFilter->GetLines(2);
  lines = houghFilter->GetLines(atoi(argv[3]));
  std::cout << "Found " << lines.size() << " line(s)." << std::endl;

  itk::Index<2> m_Index;
  /** Draw the resulting lines */
  HoughTransformFilterType::LinesListType::const_iterator it_lines = lines.begin();
  while(it_lines != lines.end())
  {
    // Draw the lines
    HoughTransformFilterType::LineType::PointListType points_list = (*it_lines)->GetPoints();
    HoughTransformFilterType::LineType::PointListType::const_iterator it_points = points_list.begin();
    
    itk::Size<2> size = m_OutputImage->GetLargestPossibleRegion().GetSize();
    float diag = sqrt((float)(size[0]*size[0]+size[1]*size[1]));

   
    double u[2];
    u[0] = (*it_points).GetPosition()[0];
    u[1] = (*it_points).GetPosition()[1];
    it_points++;
    double v[2];
    v[0] = u[0]-(*it_points).GetPosition()[0];
    v[1] = u[1]-(*it_points).GetPosition()[1];

    double norm = sqrt(v[0]*v[0]+v[1]*v[1]);
    v[0]/=norm;
    v[1]/=norm;

    for(int i=static_cast<int>(-diag);i<static_cast<int>(diag);i++)
    {
      m_Index[0]=(long int)(u[0]+i*v[0]);
      m_Index[1]=(long int)(u[1]+i*v[1]);

      if(m_Index[0]>=0 && m_Index[0]<(long)m_OutputImage->GetLargestPossibleRegion().GetSize()[0]
         && m_Index[1]>=0 && m_Index[1]<(long)m_OutputImage->GetLargestPossibleRegion().GetSize()[1])
      {
        m_OutputImage->SetPixel(m_Index,255);
      }
    }
    
    it_lines++;
  }


  writer->SetInput(m_OutputImage);

  try
    {
      writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  return 0;
}



