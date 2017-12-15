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

// Software Guide : BeginLatex
//
// This example illustrates the use of the
// \doxygen{HoughTransform2DCirclesImageFilter} to find circles in a
// 2-dimensional image.
//
// First, we include the header files of the filter.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkHoughTransform2DCirclesImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkThresholdImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include <list>
#include "itkCastImageFilter.h"
#include "itkMath.h"

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
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Next, we declare the pixel type and image dimension and specify the
  //  image type to be used as input. We also specify the image type of the
  //  accumulator used in the Hough transform filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   unsigned char   PixelType;
  typedef   float           AccumulatorPixelType;
  const     unsigned int    Dimension = 2;
  typedef itk::Image< PixelType, Dimension >  ImageType;
  ImageType::IndexType localIndex;
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
    return EXIT_FAILURE;
    }
  ImageType::Pointer localImage = reader->GetOutput();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We create the HoughTransform2DCirclesImageFilter based on the pixel
  //  type of the input image (the resulting image from the
  //  ThresholdImageFilter).
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Computing Hough Map" << std::endl;

  typedef itk::HoughTransform2DCirclesImageFilter<PixelType,
               AccumulatorPixelType> HoughTransformFilterType;
  HoughTransformFilterType::Pointer houghFilter
                                            = HoughTransformFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We set the input of the filter to be the output of the
  //  ImageFileReader. We set also the number of circles we are looking for.
  //  Basically, the filter computes the Hough map, blurs it using a certain
  //  variance and finds maxima in the Hough map. After a maximum is found,
  //  the local neighborhood, a circle, is removed from the Hough map.
  //  SetDiscRadiusRatio() defines the radius of this disc proportional to
  //  the radius of the disc found.  The Hough map is computed by looking at
  //  the points above a certain threshold in the input image. Then, for each
  //  point, a Gaussian derivative function is computed to find the direction
  //  of the normal at that point. The standard deviation of the derivative
  //  function can be adjusted by SetSigmaGradient(). The accumulator is
  //  filled by drawing a line along the normal and the length of this line
  //  is defined by the minimum radius (SetMinimumRadius()) and the maximum
  //  radius (SetMaximumRadius()).  Moreover, a sweep angle can be defined by
  //  SetSweepAngle() (default 0.0) to increase the accuracy of detection.
  //
  //  The output of the filter is the accumulator.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  houghFilter->SetInput( reader->GetOutput() );

  houghFilter->SetNumberOfCircles( atoi(argv[3]) );
  houghFilter->SetMinimumRadius(   atof(argv[4]) );
  houghFilter->SetMaximumRadius(   atof(argv[5]) );

  if( argc > 6 )
    {
    houghFilter->SetSweepAngle( atof(argv[6]) );
    }
  if( argc > 7 )
    {
    houghFilter->SetSigmaGradient( atoi(argv[7]) );
    }
  if( argc > 8 )
    {
    houghFilter->SetVariance( atof(argv[8]) );
    }
  if( argc > 9 )
    {
    houghFilter->SetDiscRadiusRatio( atof(argv[9]) );
    }

  houghFilter->Update();
  AccumulatorImageType::Pointer localAccumulator = houghFilter->GetOutput();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We can also get the circles as \doxygen{EllipseSpatialObject}. The
  //  \code{GetCircles()} function return a list of those.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  HoughTransformFilterType::CirclesListType circles;
  circles = houghFilter->GetCircles();
  std::cout << "Found " << circles.size() << " circle(s)." << std::endl;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We can then allocate an image to draw the resulting circles as binary
  //  objects.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  unsigned char                            OutputPixelType;
  typedef  itk::Image< OutputPixelType, Dimension > OutputImageType;

  OutputImageType::Pointer  localOutputImage = OutputImageType::New();

  OutputImageType::RegionType region;
  region.SetSize(localImage->GetLargestPossibleRegion().GetSize());
  region.SetIndex(localImage->GetLargestPossibleRegion().GetIndex());
  localOutputImage->SetRegions( region );
  localOutputImage->SetOrigin(localImage->GetOrigin());
  localOutputImage->SetSpacing(localImage->GetSpacing());
  localOutputImage->Allocate(true); // initializes buffer to zero
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We iterate through the list of circles and we draw them.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef HoughTransformFilterType::CirclesListType CirclesListType;
  CirclesListType::const_iterator itCircles = circles.begin();

  while( itCircles != circles.end() )
    {
    std::cout << "Center: ";
    std::cout << (*itCircles)->GetObjectToParentTransform()->GetOffset()
              << std::endl;
    std::cout << "Radius: " << (*itCircles)->GetRadius()[0] << std::endl;
    // Software Guide : EndCodeSnippet

    //  Software Guide : BeginLatex
    //
    //  We draw white pixels in the output image to represent each circle.
    //
    //  Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    for( double angle = 0;
         angle <= itk::Math::twopi;
         angle += itk::Math::pi/60.0 )
      {
      typedef HoughTransformFilterType::CircleType::TransformType
        TransformType;
      typedef TransformType::OutputVectorType
        OffsetType;
      const OffsetType offset =
        (*itCircles)->GetObjectToParentTransform()->GetOffset();
      localIndex[0] =
         itk::Math::Round<long int>(offset[0]
                    + (*itCircles)->GetRadius()[0]*std::cos(angle));
      localIndex[1] =
         itk::Math::Round<long int>(offset[1]
                    + (*itCircles)->GetRadius()[0]*std::sin(angle));
      OutputImageType::RegionType outputRegion =
                                  localOutputImage->GetLargestPossibleRegion();

      if( outputRegion.IsInside( localIndex ) )
        {
        localOutputImage->SetPixel( localIndex, 255 );
        }
      }
    itCircles++;
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
  writer->SetInput(localOutputImage );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
