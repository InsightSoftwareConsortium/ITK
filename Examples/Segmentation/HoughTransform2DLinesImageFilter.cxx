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


// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{HoughTransform2DLinesImageFilter}
// to find straight lines in a 2-dimensional image.
//
// First, we include the header files of the filter.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkHoughTransform2DLinesImageFilter.h"
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
  //  Once the image is loaded, we apply a \doxygen{GradientMagnitudeImageFilter} to segment edges.
  //  This needs to cast the input image using a \doxygen{CastImageFilter}.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter< 
                        ImageType, 
                        AccumulatorImageType    >    CastingFilterType;
  
  CastingFilterType::Pointer caster = CastingFilterType::New();

  std::cout << "Applying gradient magnitude filter" << std::endl;
  typedef itk::GradientMagnitudeImageFilter<AccumulatorImageType,
                                            AccumulatorImageType> GradientFilterType;
  GradientFilterType::Pointer gradFilter =  GradientFilterType::New();
  caster->SetInput(m_Image);
  gradFilter->SetInput(caster->GetOutput());
  gradFilter->Update();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  The next step is to apply a threshold filter on the gradient magnitude image to keep only
  //  bright values. Only pixels with a high value will be used by the HoughTransformFilter.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  std::cout << "Thresholding" << std::endl;
  typedef itk::ThresholdImageFilter<AccumulatorImageType> ThresholdFilterType;
  ThresholdFilterType::Pointer threshFilter = ThresholdFilterType::New();
  threshFilter->SetInput( gradFilter->GetOutput());
  threshFilter->SetOutsideValue(0);
  unsigned char thresh_below = 0;
  unsigned char thresh_above = 255;
  threshFilter->ThresholdOutside(thresh_below,thresh_above);
  threshFilter->Update();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We create the HoughTransform2DLinesImageFilter based on the pixel type of the input image 
  //  (the resulting image from the ThresholdImageFilter).
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  std::cout << "Computing Hough Map" << std::endl;
  typedef itk::HoughTransform2DLinesImageFilter<AccumulatorPixelType,
                                                AccumulatorPixelType>   
                                                         HoughTransformFilterType;
  HoughTransformFilterType::Pointer houghFilter = HoughTransformFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We set the input of the filter to be the resulting image of the ThresholdImageFilter
  //  We set also the number of lines we are looking.
  //  Basically, the filter computes the Hough map, blur it using a certain variance and find maxima
  //  in the Hough map. After a maximum is found, the local neighborhood, a circle, is removed from the 
  //  Hough map. SetDiscRadius() defines the radius of this disc.
  //  
  //  The output of the filter is the accumulator.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  houghFilter->SetInput(threshFilter->GetOutput());
  houghFilter->SetNumberOfLines(atoi(argv[3]));
  houghFilter->SetVariance(atof(argv[4]));
  houghFilter->SetDiscRadius(atof(argv[5]));
  houghFilter->Update();
  AccumulatorImageType::Pointer m_Accumulator = houghFilter->GetOutput();   
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We can also get the lines as \doxygen{LineSpatialObject}. The GetLines() function
  //  return a list of those.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  HoughTransformFilterType::LinesListType lines;
  lines = houghFilter->GetLines(atoi(argv[3]));
  std::cout << "Found " << lines.size() << " line(s)." << std::endl;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We can then allocate an image to draw the resulting lines as binary objects.
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
  //  We iterate through the list of lines and we draw them.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  HoughTransformFilterType::LinesListType::const_iterator it_lines = lines.begin();
  while(it_lines != lines.end())
  {  
  // Software Guide : EndCodeSnippet
  //  Software Guide : BeginLatex
  //  
  //  We get the list of points which consists of two points to represent a straight line.
  //  Then, from these two points, we compute a fixed point $u$ and a unit 
  //  vector $\vec{v}$ to parameterize the line.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
    HoughTransformFilterType::LineType::PointListType 
                                              points_list = (*it_lines)->GetPoints();
    HoughTransformFilterType::LineType::PointListType::const_iterator 
                                              it_points = points_list.begin();
    
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
  // Software Guide : EndCodeSnippet
  //  Software Guide : BeginLatex
  //  
  //  We draw a white pixel in the Output image to represent the line.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
    itk::Size<2> size = m_OutputImage->GetLargestPossibleRegion().GetSize();
    float diag = sqrt((float)(size[0]*size[0]+size[1]*size[1]));
    for(int i=static_cast<int>(-diag);i<static_cast<int>(diag);i++)
    {
      m_Index[0]=(long int)(u[0]+i*v[0]);
      m_Index[1]=(long int)(u[1]+i*v[1]);

      if(m_Index[0]>=0 
         && m_Index[0]<(long)m_OutputImage->GetLargestPossibleRegion().GetSize()[0]
         && m_Index[1]>=0 
         && m_Index[1]<(long)m_OutputImage->GetLargestPossibleRegion().GetSize()[1])
      {
        m_OutputImage->SetPixel(m_Index,255);
      }
    }
    it_lines++;
  }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We setup a writer to write out the binary image created.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
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
  // Software Guide : EndCodeSnippet

  return 0;
}



