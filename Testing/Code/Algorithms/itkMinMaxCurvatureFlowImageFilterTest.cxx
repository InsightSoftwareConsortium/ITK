/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinMaxCurvatureFlowImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkOutputWindow.h"
#include "itkCommand.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_sample.h"


// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};


/**
 * This file tests the functionality of the MinMaxCurvatureFlowImageFilter.
 * The test uses a binary image of a circle/sphere with intensity value
 * of 0 (black). The background is white ( intensity = 255 ).
 * X% salt and pepper noise is added to the the input image. Specifically,
 * X% of the pixels is replaced with a value choosen from a uniform 
 * distribution between 0 and 255.
 *
 * We then test the ability of MinMaxCurvatureFlowImageFilter to denoise
 * the image.
 */
int main()
{

  typedef float PixelType;
  enum { ImageDimension = 2 };
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::ImageRegionIterator<ImageType> IteratorType;
  typedef itk::MinMaxCurvatureFlowImageFilter<ImageType,ImageType> DenoiserType;
  DenoiserType::Pointer denoiser = DenoiserType::New();

  int j;


  /**
   * Create an image containing a circle/sphere with intensity of 0
   * and background of 255 with added salt and pepper noise.
   */
  double sqrRadius = vnl_math_sqr( 20.0 );  // radius of the circle/sphere
  double fractionNoise = 0.30;              // salt & pepper noise fraction
  PixelType foreground = 0.0;               // intensity value of the foreground
  PixelType background = 255.0;             // intensity value of the background

  std::cout << "Create an image of circle/sphere with noise" << std::endl;
  ImageType::Pointer circleImage = ImageType::New();

  ImageType::SizeType size;
  for ( j = 0; j < ImageDimension; j++ )
    { 
    size[j] = 64;
    }
//  size[2] = 16;
  
  ImageType::RegionType region;
  region.SetSize( size );
  
  circleImage->SetLargestPossibleRegion( region );
  circleImage->SetBufferedRegion( region );
  circleImage->Allocate();

  IteratorType circleIter( circleImage, circleImage->GetBufferedRegion() );

  
  for ( ; !circleIter.IsAtEnd() ; ++circleIter )
    {
    ImageType::IndexType index = circleIter.GetIndex();
    float value;

    double lhs = 0.0;
    for ( j = 0; j < ImageDimension; j++ )
      {
      lhs += vnl_math_sqr( (double) index[j] - (double) size[j] * 0.5 );
      }
    if ( lhs < sqrRadius )
      {
      value = foreground;
      }
    else
      {
      value = background;
      }

    if ( vnl_sample_uniform( 0.0, 1.0 ) < fractionNoise )
      {
      value = vnl_sample_uniform( vnl_math_min(foreground,background), 
        vnl_math_max(foreground,background) );
      }

    circleIter.Set( value );

    }

  /**
   * Run MinMaxCurvatureFlowImageFilter several times using the previous
   * output as the input in the next run.
   */
#define MAXRUNS 5 // maximum number of runs

  int numberOfRuns = 2;    // number of times to run the filter
  unsigned int niter[MAXRUNS] = { 100, 100 };  // number of iterations
  DenoiserType::RadiusValueType radii[MAXRUNS] = { 1, 3 }; // stencil radius

  std::cout << "Run MinMaxCurvatureFlowImageFiler.." << std::endl;

  // set other denoiser parameters here
  denoiser->SetTimeStep( 0.15 );

  // attach a progress watcher to the denoiser
  ShowProgressObject progressWatch(denoiser);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  denoiser->AddObserver( itk::ProgressEvent(), command);


  ImageType::Pointer swapPointer = circleImage;

  for ( int j = 0; j < numberOfRuns; j++ )
    {

    denoiser->SetInput( swapPointer );

    // set the stencil radius and number of iterations
    denoiser->SetStencilRadius( radii[j] );
    denoiser->SetNumberOfIterations( niter[j] );
  
    std::cout << " Run: " << j;
    std::cout << " Radius: " << denoiser->GetStencilRadius();
    std::cout << " Iter: " << denoiser->GetNumberOfIterations();
    std::cout << std::endl;

    // run the filter
    denoiser->Update();

    swapPointer = denoiser->GetOutput();
    swapPointer->DisconnectPipeline();
    }


  /**
   * Check the quality of the output by comparing it against a
   * clean image of the circle/sphere.
   * An output pixel is okay if it is within 
   * 0.1 * |foreground - background| of the true value.
   * This test is considered as passed if the fraction of wrong
   * pixels is less than the original noise fraction.
   */
  std::cout << "Checking the output..." << std::endl;

  IteratorType outIter( swapPointer, 
    swapPointer->GetBufferedRegion() );
  
  PixelType tolerance = vnl_math_abs( foreground - background ) * 0.1;

  unsigned long numPixelsWrong = 0;

  for ( ; !outIter.IsAtEnd(); ++outIter )
    {
    
    ImageType::IndexType index = outIter.GetIndex();
    PixelType value = outIter.Get();

    double lhs = 0.0;
    for ( j = 0; j < ImageDimension; j++ )
      {
      lhs += vnl_math_sqr( (double) index[j] - (double) size[j] * 0.5 );
      }
    if ( lhs < sqrRadius )
      {
      if ( vnl_math_abs( foreground - value ) > tolerance )
        {
        numPixelsWrong++;
        }
      }
    else if ( vnl_math_abs( background - value ) > tolerance )
      {
      numPixelsWrong++;
      }
    }

  double fractionWrong = (double) numPixelsWrong / 
    (double) region.GetNumberOfPixels();

  std::cout << "Noise reduced from " << fractionNoise << " to ";
  std::cout << fractionWrong << std::endl;

  bool passed = true;
  if ( fractionWrong > fractionNoise ) 
    { 
    passed = false; 
    }


  /**
   * Exercise other member functions here
   */
  denoiser->Print( std::cout );

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
