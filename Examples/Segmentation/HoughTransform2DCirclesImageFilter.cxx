/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    HoughTransform2DCirclesImageFilter.cxx
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

#ifndef PI
  #define PI 3.1415926535897932384626433832795
#endif

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{HoughTransform2DCirclesImageFilter}
// to find circles in a 2-dimensional image.
//
// First, we include the header files of the filter.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkHoughTransform2DCirclesImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkThresholdImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include <itkGradientMagnitudeImageFilter.h>
#include <itkDiscreteGaussianImageFilter.h>
#include <list>
#include "itkCastImageFilter.h"

int main( int argc, char *argv[] )
{

  if( argc < 6 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0] << std::endl;
    std::cerr << " inputImage " << std::endl;  
    std::cerr << " outputImage" << std::endl;  
    std::cerr << " numberOfCircles " << std::endl;
    std::cerr << " radius Min " << std::endl;
    std::cerr << " radius Max " << std::endl;
    std::cerr << " sweep Angle (default = 0)" << std::endl;
    std::cerr << " SigmaGradient (default = 1) " << std::endl;  
    std::cerr << " variance of the accumulator blurring (default = 5) " << std::endl;  
    std::cerr << " radius of the disk to remove from the accumulator (default = 10) "<< std::endl;   
    return 1;
    }

  //  Software Guide : BeginLatex
  //  
  //  Next, we declare the pixel type and image dimension and 
  //  specify the image type to be used as input. We also specify the image type
  //  of the accumulator used in the HoughTransform filter
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   unsigned char   PixelType;
  typedef   float           AccumulatorPixelType;
  const     unsigned int    Dimension = 2;
  typedef itk::Image< PixelType, Dimension >  ImageType;
  ImageType::IndexType m_Index;
  typedef itk::Image< AccumulatorPixelType, Dimension > AccumulatorImageType;  
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We setup a reader to load the input image.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef  itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
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
  // Software Guide : EndCodeSnippet
  //  Software Guide : BeginLatex
  //  
  //  We create the HoughTransform2DCirclesImageFilter based on the pixel type of the input image 
  //  (the resulting image from the ThresholdImageFilter).
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  std::cout << "Computing Hough Map" << std::endl;
  typedef itk::HoughTransform2DCirclesImageFilter<PixelType,
                                                  AccumulatorPixelType>   HoughTransformFilterType;
  HoughTransformFilterType::Pointer houghFilter = HoughTransformFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We set the input of the filter to be the resulting image of the ImageFileReader
  //  We set also the number of circles we are looking.
  //  Basically, the filter computes the Hough map, blur it using a certain variance and find maxima
  //  in the Hough map. After a maximum is found, the local neighborhood, a circle, is removed from the 
  //  Hough map. SetDiscRadiusRatio() defines the radius of this disc proportional to the radius of the disc found.
  //  The Hough Map is computed by looking at the points above a certain threshold in the input image. Then, for each
  //  point, a gaussian derivative function is computed to find the direction of the normal at that point. The standard
  //  deviation of the derivative function can be adjusted by SetSigmaGradient(). The accumulator is filled by drawing
  //  a line along the normal and the lenght of this line is defined by the minimum radius (SetMinimumRadius()) and
  //  the maximum radius (SetMaximumRadius()).
  //  Moreover, a sweep angle can be defined by SetSweepAngle() (default 0.0) to increase the accuracy of detection.
  //
  //  The output of the filter is the accumulator.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  houghFilter->SetInput(reader->GetOutput());
  houghFilter->SetNumberOfCircles(atoi(argv[3]));
  houghFilter->SetMinimumRadius(atof(argv[4]));
  houghFilter->SetMaximumRadius(atof(argv[5]));

  if( argc > 6 )
    {
     houghFilter->SetSweepAngle(atof(argv[6]));
    }

  if( argc > 7 )
    {
     houghFilter->SetSigmaGradient(atoi(argv[7]));
    }
  if( argc > 8 )
    {
    houghFilter->SetVariance(atof(argv[8]));
    }
  if( argc > 9 )
    {
    houghFilter->SetDiscRadiusRatio(atof(argv[9]));
    }
    
  houghFilter->Update();
  AccumulatorImageType::Pointer m_Accumulator = houghFilter->GetOutput();   
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We can also get the circles as \doxygen{EllipseSpatialObject}. The GetCircles() function
  //  return a list of those.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  HoughTransformFilterType::CirclesListType circles;
  circles = houghFilter->GetCircles(atoi(argv[3]));
  std::cout << "Found " << circles.size() << " circle(s)." << std::endl;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We can then allocate an image to draw the resulting circles as binary objects.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef  unsigned char    OutputPixelType;
  typedef  itk::Image< OutputPixelType, Dimension > OutputImageType;  

  OutputImageType::Pointer  m_OutputImage = OutputImageType::New();

  OutputImageType::RegionType region;
  region.SetSize(m_Image->GetLargestPossibleRegion().GetSize());
  region.SetIndex(m_Image->GetLargestPossibleRegion().GetIndex());
  m_OutputImage->SetRegions( region );
  m_OutputImage->SetOrigin(m_Image->GetOrigin());
  m_OutputImage->SetSpacing(m_Image->GetSpacing());
  m_OutputImage->Allocate();
  m_OutputImage->FillBuffer(0);
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  We iterate through the list of circles and we draw them.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  HoughTransformFilterType::CirclesListType::const_iterator it_circles = circles.begin();
  while(it_circles != circles.end())
    {
    std::cout << "Center: " << (*it_circles)->GetObjectToParentTransform()->GetOffset() << std::endl;
    std::cout << "Radius: " << (*it_circles)->GetRadius()[0] << std::endl;
  
    
  // Software Guide : EndCodeSnippet
  //  Software Guide : BeginLatex
  //  
  //  We draw a white pixel in the Output image to represent each circle.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
    for(double angle = 0;angle <= 2*PI;angle+=PI/60)
      {
      m_Index[0]=(*it_circles)->GetObjectToParentTransform()->GetOffset()[0] + (*it_circles)->GetRadius()[0]*cos(angle);
      m_Index[1]=(*it_circles)->GetObjectToParentTransform()->GetOffset()[1] + (*it_circles)->GetRadius()[0]*sin(angle);

      if(m_Index[0]>=0 && m_Index[0]<(long)m_OutputImage->GetLargestPossibleRegion().GetSize()[0]
         && m_Index[1]>=0 && m_Index[1]<(long)m_OutputImage->GetLargestPossibleRegion().GetSize()[1])
        {
          m_OutputImage->SetPixel(m_Index,255);
        }
      }
    it_circles++;
  }

  
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We setup a writer to write out the binary image created.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef  itk::ImageFileWriter< ImageType  > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput(m_OutputImage );

  try
    {
      writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }
  // Software Guide : EndCodeSnippet

  return 0;
}



