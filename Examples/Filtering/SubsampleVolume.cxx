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

//  Software Guide : BeginLatex
//
//  This example illustrates how to perform subsampling of a volume using ITK
//  classes.  In order to avoid aliasing artifacts, the volume must be
//  processed by a low-pass filter before resampling.  Here we use the
//  \doxygen{RecursiveGaussianImageFilter} as a low-pass filter. The image is
//  then resampled by using three different factors, one per dimension of the
//  image.
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


// Software Guide : BeginLatex
//
// The most important headers to include here are those corresponding to the
// resampling image filter, the transform, the interpolator and the smoothing
// filter.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkResampleImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkCastImageFilter.h"


int main( int argc, char * argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
      << "  inputImageFile  outputImageFile factorX factorY factorZ"
      << std::endl;
    return EXIT_FAILURE;
    }


// Software Guide : BeginLatex
//
// We explicitly instantiate the pixel type and dimension of the input image,
// and the images that will be used internally for computing the resampling.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const     unsigned int    Dimension = 3;

  typedef   unsigned char   InputPixelType;

  typedef   float           InternalPixelType;
  typedef   unsigned char   OutputPixelType;

  typedef itk::Image< InputPixelType,    Dimension >   InputImageType;
  typedef itk::Image< InternalPixelType, Dimension >   InternalImageType;
  typedef itk::Image< OutputPixelType,   Dimension >   OutputImageType;
// Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );


// Software Guide : BeginLatex
//
// In this particular case we take the factors for resampling directly from the
// command line arguments.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const double factorX = atof( argv[3] );
  const double factorY = atof( argv[4] );
  const double factorZ = atof( argv[5] );
// Software Guide : EndCodeSnippet


  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }


  InputImageType::ConstPointer inputImage = reader->GetOutput();


// Software Guide : BeginLatex
//
// A casting filter is instantiated in order to convert the pixel type of the
// input image into the pixel type desired for computing the resampling.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter< InputImageType,
                                InternalImageType >   CastFilterType;

  CastFilterType::Pointer caster = CastFilterType::New();

  caster->SetInput( inputImage );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The smoothing filter of choice is the \code{RecursiveGaussianImageFilter}.
// We create three of them in order to have the freedom of performing smoothing
// with different sigma values along each dimension.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::RecursiveGaussianImageFilter<
                                  InternalImageType,
                                  InternalImageType > GaussianFilterType;

  GaussianFilterType::Pointer smootherX = GaussianFilterType::New();
  GaussianFilterType::Pointer smootherY = GaussianFilterType::New();
  GaussianFilterType::Pointer smootherZ = GaussianFilterType::New();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The smoothing filters are connected in a cascade in the pipeline.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  smootherX->SetInput( caster->GetOutput() );
  smootherY->SetInput( smootherX->GetOutput() );
  smootherZ->SetInput( smootherY->GetOutput() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The sigma values to use in the smoothing filters are computed based on the
// pixel spacing of the input image and the factors provided as arguments.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const InputImageType::SpacingType& inputSpacing = inputImage->GetSpacing();

  const double sigmaX = inputSpacing[0] * factorX;
  const double sigmaY = inputSpacing[1] * factorY;
  const double sigmaZ = inputSpacing[2] * factorZ;

  smootherX->SetSigma( sigmaX );
  smootherY->SetSigma( sigmaY );
  smootherZ->SetSigma( sigmaZ );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We instruct each one of the smoothing filters to act along a particular
// direction of the image, and set them to use normalization across scale space
// in order to account for the reduction of intensity that accompanies the
// diffusion process associated with the Gaussian smoothing.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  smootherX->SetDirection( 0 );
  smootherY->SetDirection( 1 );
  smootherZ->SetDirection( 2 );

  smootherX->SetNormalizeAcrossScale( false );
  smootherY->SetNormalizeAcrossScale( false );
  smootherZ->SetNormalizeAcrossScale( false );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The type of the resampling filter is instantiated using the internal image
// type and the output image type.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ResampleImageFilter<
                  InternalImageType, OutputImageType >  ResampleFilterType;

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Since the resampling is performed in the same physical extent of the input
// image, we select the IdentityTransform as the one to be used by the resampling
// filter.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::IdentityTransform< double, Dimension >  TransformType;

  TransformType::Pointer transform = TransformType::New();
  transform->SetIdentity();
  resampler->SetTransform( transform );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The Linear interpolator is selected because it provides a good run-time
// performance.  For applications that require better precision you may want to
// replace this interpolator with the \doxygen{BSplineInterpolateImageFunction}
// interpolator or with the \doxygen{WindowedSincInterpolateImageFunction}
// interpolator.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::LinearInterpolateImageFunction<
                           InternalImageType, double > InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  resampler->SetInterpolator( interpolator );
// Software Guide : EndCodeSnippet

  resampler->SetDefaultPixelValue( 0 ); // value for regions without source

// Software Guide : BeginLatex
//
// The spacing to be used in the grid of the resampled image is computed using
// the input image spacing and the factors provided in the command line
// arguments.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  OutputImageType::SpacingType spacing;

  spacing[0] = inputSpacing[0] * factorX;
  spacing[1] = inputSpacing[1] * factorY;
  spacing[2] = inputSpacing[2] * factorZ;

  resampler->SetOutputSpacing( spacing );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The origin and direction of the input image are both preserved and passed to
// the output image.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  resampler->SetOutputOrigin( inputImage->GetOrigin() );
  resampler->SetOutputDirection( inputImage->GetDirection() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The number of pixels to use along each direction on the grid of the
// resampled image is computed using the number of pixels in the input image
// and the sampling factors.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  InputImageType::SizeType   inputSize =
              inputImage->GetLargestPossibleRegion().GetSize();

  typedef InputImageType::SizeType::SizeValueType SizeValueType;

  InputImageType::SizeType   size;

  size[0] = static_cast< SizeValueType >( inputSize[0] / factorX );
  size[1] = static_cast< SizeValueType >( inputSize[1] / factorY );
  size[2] = static_cast< SizeValueType >( inputSize[2] / factorZ );

  resampler->SetSize( size );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Finally, the input to the resampler is taken from the output of the
// smoothing filter.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  resampler->SetInput( smootherZ->GetOutput() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// At this point we can trigger the execution of the resampling by calling the
// \code{Update()} method, or we can choose to pass the output of the resampling
// filter to another section of pipeline, for example, an image writer.
//
// Software Guide : EndLatex


  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( resampler->GetOutput() );

  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }

  std::cout << "Resampling Done !" << std::endl;


  return EXIT_SUCCESS;
}
