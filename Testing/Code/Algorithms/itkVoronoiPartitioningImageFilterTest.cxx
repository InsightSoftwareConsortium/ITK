/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiPartitioningImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif



#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVoronoiPartitioningImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkFixedArray.h"
#include "../BasicFilters/itkFilterWatcher.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkScalarToRGBPixelFunctor.h"

int itkVoronoiPartitioningImageFilterTest(int argc, char* argv[])
{
  typedef itk::Image<float,2> FloatImage;
  typedef itk::Image<unsigned char,2> UnsignedCharImage;
  typedef itk::Image<unsigned short,2> UnsignedShortImage;
  typedef itk::Image<bool,2> BoolImage;

  if (argc != 4)
    {
    std::cout << "Usage: itkVoronoiPartitioningImageFilterTest input output show_boundaries" << std::endl;
    return EXIT_FAILURE;
    }
  

  /* ------------------------------------------------
   * Load an image
   */
  itk::ImageFileReader<FloatImage>::Pointer original
    = itk::ImageFileReader<FloatImage>::New();
  original->SetFileName( argv[1] );
  
  /* -------------------------------------------------
   * Preprocess the image
   */
  itk::DiscreteGaussianImageFilter<FloatImage, FloatImage>::Pointer gaussian3
    = itk::DiscreteGaussianImageFilter<FloatImage, FloatImage>::New();
  // FilterWatcher gaussian3Watcher(gaussian3);
  gaussian3->SetInput(original->GetOutput());
  gaussian3->SetVariance(1.0); 
  //gaussian3->UpdateLargestPossibleRegion();

  // Try the Voronoi partitioning
  //
  typedef itk::VoronoiPartitioningImageFilter<FloatImage, FloatImage>
    VoronoiSegmentationType;
  
  VoronoiSegmentationType::Pointer voronoi=VoronoiSegmentationType::New();
  voronoi->SetInput( gaussian3->GetOutput() );
  voronoi->SetNumberOfSeeds( 6 );
  voronoi->SetOutputBoundary( atoi(argv[3])==1 );
  voronoi->SetSteps( 7 );
  voronoi->SetSigmaThreshold( 4.0 );
  voronoi->SetMinRegion( 10 );
  voronoi->InteractiveSegmentationOn();

  FilterWatcher voronoiWatcher(voronoi);

  // Write out an image of the voronoi diagram
  typedef itk::RGBPixel<unsigned char>   RGBPixelType;
  typedef itk::Image<RGBPixelType, 2> RGBImageType;
  typedef itk::Functor::ScalarToRGBPixelFunctor<float>
    ColorMapFunctorType;
  typedef itk::UnaryFunctorImageFilter<FloatImage,
    RGBImageType, ColorMapFunctorType> ColorMapFilterType;
  typedef itk::ImageFileWriter<  RGBImageType  > WriterType;

  WriterType::Pointer writer = WriterType::New();
  ColorMapFilterType::Pointer colormapper = ColorMapFilterType::New();

  try
    {
    colormapper->SetInput( voronoi->GetOutput() );
    writer->SetInput( colormapper->GetOutput() );
    writer->SetFileName( argv[2] );
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }
  
  // 
  return EXIT_SUCCESS;

}
