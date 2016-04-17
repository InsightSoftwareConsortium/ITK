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

#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkCommand.h"
#include "vnl/vnl_sample.h"

namespace
{

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
}

#define MAXRUNS 5 // maximum number of runs

template<unsigned int VImageDimension>
int testMinMaxCurvatureFlow(
  itk::Size<VImageDimension> & size,
  double radius,
  int numberOfRuns,
  unsigned int niter[],
  unsigned long radii[] );

/**
 * This file tests the functionality of the MinMaxCurvatureFlowImageFilter.
 * The test uses a binary image of a circle/sphere with intensity value
 * of 0 (black). The background is white ( intensity = 255 ).
 * X% salt and pepper noise is added to the the input image. Specifically,
 * X% of the pixels is replaced with a value chosen from a uniform
 * distribution between 0 and 255.
 *
 * We then test the ability of MinMaxCurvatureFlowImageFilter to denoise
 * the image.
 */
int itkMinMaxCurvatureFlowImageFilterTest(int, char* [] )
{

  double radius;
  int numberOfRuns;
  unsigned int niter[MAXRUNS];
  unsigned long radii[MAXRUNS];

  itk::Size<2> size2D;
  size2D[0] = 32; size2D[1] = 32;
  radius = 10.0;
  // numberOfRuns = 2;  /* reduced to speedup purify */
  numberOfRuns = 1;
  niter[0] = 100; niter[1] = 100;
  radii[0] = 1; radii[1] = 3;
  int err2D = testMinMaxCurvatureFlow( size2D, radius, numberOfRuns,
    niter, radii );


  /* Dummy tests to test 3D and ND.
     Tests were taking too long on purify.
     Reduced number of iterations to 1 which is not
     sufficient to denoise the image.
     Increase the number of interations to get better results.
   */
  itk::Size<3> size3D;
  size3D[0] = 32; size3D[1] = 32; size3D[2] = 32;
  radius = 10.0;
  numberOfRuns = 1;
  // niter[0] = 20;  /* reduced to speedup purify */
  niter[0] = 1;
  radii[1] = 1;
  int err3D = testMinMaxCurvatureFlow( size3D, radius, numberOfRuns,
    niter, radii );

  itk::Size<4> size4D;
  size4D[0] = 8; size4D[1] = 8; size4D[2] = 8; size4D[3] = 8;
  radius = 2.6;
  // niter[0] = 10;  /* reduced to speedup purify */
  numberOfRuns = 1;
  niter[0] = 1;
  radii[1] = 1;
  int err4D = testMinMaxCurvatureFlow( size4D, radius, numberOfRuns,
    niter, radii );

  std::cout << "2D Test passed: " << !err2D << std::endl;
  std::cout << "3D Test passed: " << !err3D << std::endl;
  std::cout << "4D Test passed: " << !err4D << std::endl;

  if ( err2D /*|| err3D || err4D*/ )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;

}


template<unsigned int VImageDimension>
int testMinMaxCurvatureFlow(
  itk::Size<VImageDimension> & size, // ND image size
  double radius,                     // ND-sphere radius
  int numberOfRuns,                  // number of times to run the filter
  unsigned int niter[],              // number of iterations
  unsigned long radii[]              // stencil radius
)
{

  typedef float PixelType;
  enum { ImageDimension = VImageDimension };
  typedef itk::Image<PixelType, ImageDimension>                    ImageType;
  typedef itk::ImageRegionIterator<ImageType>                      IteratorType;
  typedef itk::MinMaxCurvatureFlowImageFilter<ImageType,ImageType> DenoiserType;
  typename DenoiserType::Pointer denoiser = DenoiserType::New();

  int j;

  /**
   * Create an image containing a circle/sphere with intensity of 0
   * and background of 255 with added salt and pepper noise.
   */
  double sqrRadius = itk::Math::sqr( radius );  // radius of the circle/sphere
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


  for (; !circleIter.IsAtEnd(); ++circleIter )
    {
    typename ImageType::IndexType index = circleIter.GetIndex();
    float value;

    double lhs = 0.0;
    for ( j = 0; j < ImageDimension; j++ )
      {
      lhs += itk::Math::sqr( (double) index[j] - (double) size[j] * 0.5 );
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
      value = vnl_sample_uniform( std::min(foreground,background),
        std::max(foreground,background) );
      }

    circleIter.Set( value );

    }

  /**
   * Run MinMaxCurvatureFlowImageFilter several times using the previous
   * output as the input in the next run.
   */

  std::cout << "Run MinMaxCurvatureFlowImageFiler.." << std::endl;

  // set other denoiser parameters here
  denoiser->SetTimeStep( 0.05 );

  // attach a progress watcher to the denoiser
  ShowProgressObject progressWatch(denoiser);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  denoiser->AddObserver( itk::ProgressEvent(), command);


  typename ImageType::Pointer swapPointer = circleImage;

  for ( j = 0; j < numberOfRuns; j++ )
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

  PixelType tolerance = itk::Math::abs( foreground - background ) * 0.1;

  unsigned long numPixelsWrong = 0;

  for (; !outIter.IsAtEnd(); ++outIter )
    {
    typename ImageType::IndexType index = outIter.GetIndex();
    PixelType value = outIter.Get();

    double lhs = 0.0;
    for ( j = 0; j < ImageDimension; j++ )
      {
      lhs += itk::Math::sqr( (double) index[j] - (double) size[j] * 0.5 );
      }
    if ( lhs < sqrRadius )
      {
      if ( itk::Math::abs( foreground - value ) > tolerance )
        {
        numPixelsWrong++;
        }
      }
    else if ( itk::Math::abs( background - value ) > tolerance )
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

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /**
   * Exercise other member functions here
   */
  denoiser->Print( std::cout );

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
    denoiser->ResetPipeline();
    }

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
