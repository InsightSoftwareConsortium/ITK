/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMinMaxCurvatureFlowImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkBinaryMinMaxCurvatureFlowImageFilter.h"
#include "itkOutputWindow.h"
#include "itkCommand.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_sample.h"


// The following class is used to support callbacks
// on the filter in the pipeline that follows later
namespace
{class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};
}


#define MAXRUNS 5 // maximum number of runs

template<unsigned int VImageDimension>
int testBinaryMinMaxCurvatureFlow(
  itk::Size<VImageDimension> & size,
  double threshold,
  double radius,
  int numberOfRuns,
  unsigned int niter[],
  unsigned long radii[] );

/**
 * This file tests the functionality of the BinaryMinMaxCurvatureFlowImageFilter.
 * The test uses a binary image of a circle/sphere with intensity value
 * of 0 (black). The background is white ( intensity = 255 ).
 * X% salt and pepper noise is added to the the input image. Specifically,
 * X% of the pixels is replaced with a value choosen from a uniform
 * distribution between 0 and 255.
 *
 * We then test the ability of BinaryMinMaxCurvatureFlowImageFilter to denoise
 * the binary image.
 */
int itkBinaryMinMaxCurvatureFlowImageFilterTest(int, char* [] )
{

  double radius;
  int numberOfRuns;
  unsigned int niter[MAXRUNS];
  unsigned long radii[MAXRUNS];

  itk::Size<2> size2D;
  size2D[0] = 64; size2D[1] = 64;
  radius = 20.0;
  numberOfRuns = 2;
  niter[0] = 100; niter[1] = 100;
  radii[0] = 1; radii[1] = 3;
  int err2D = testBinaryMinMaxCurvatureFlow( size2D, 127.5, radius, numberOfRuns,
    niter, radii );

  if ( err2D )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;

}


template<unsigned int VImageDimension>
int testBinaryMinMaxCurvatureFlow(
  itk::Size<VImageDimension> & size, // ND image size
  double threshold,
  double radius,                     // ND-sphere radius
  int numberOfRuns,                  // number of times to run the filter
  unsigned int niter[],              // number of iterations
  unsigned long radii[]              // stencil radius
)
{

  typedef float PixelType;
  enum { ImageDimension = VImageDimension };
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::ImageRegionIterator<ImageType> IteratorType;
  typedef itk::BinaryMinMaxCurvatureFlowImageFilter<ImageType,ImageType> DenoiserType;
  typename DenoiserType::Pointer denoiser = DenoiserType::New();

  int j;

  /**
   * Create an image containing a circle/sphere with intensity of 0
   * and background of 255 with added salt and pepper noise.
   */
  double sqrRadius = vnl_math_sqr( radius );  // radius of the circle/sphere
  double fractionNoise = 0.30;              // salt & pepper noise fraction
  PixelType foreground = 0.0;               // intensity value of the foreground
  PixelType background = 255.0;             // intensity value of the background

  std::cout << "Create an image of circle/sphere with noise" << std::endl;
  typename ImageType::Pointer circleImage = ImageType::New();


  typename ImageType::RegionType region;
  region.SetSize( size );

  circleImage->SetLargestPossibleRegion( region );
  circleImage->SetBufferedRegion( region );
  circleImage->Allocate();

  IteratorType circleIter( circleImage, circleImage->GetBufferedRegion() );


  for ( ; !circleIter.IsAtEnd() ; ++circleIter )
    {
    typename ImageType::IndexType index = circleIter.GetIndex();
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

  std::cout << "Run BinaryMinMaxCurvatureFlowImageFiler.." << std::endl;

  // set other denoiser parameters here
  denoiser->SetTimeStep( 0.05 );
  denoiser->SetThreshold( threshold );

  // attach a progress watcher to the denoiser
  ShowProgressObject progressWatch(denoiser);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  denoiser->AddObserver( itk::ProgressEvent(), command);


  typename ImageType::Pointer swapPointer = circleImage;

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

    typename ImageType::IndexType index = outIter.GetIndex();
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
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
   }


  /**
   * Exercise other member functions here
   */
  denoiser->Print( std::cout );
  std::cout << "GetThreshold: " << denoiser->GetThreshold() << std::endl;

 /**
  * Exercise error handling
  */
  typedef itk::CurvatureFlowFunction<ImageType> WrongFunctionType;
  typename WrongFunctionType::Pointer wrongFunction = WrongFunctionType::New();

  passed = false;
  try
    {
    denoiser->SetDifferenceFunction( wrongFunction );
    denoiser->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    passed = true;
    std::cout << "Caught expected exception." << std::endl;
    std::cout << err << std::endl;
    }

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
