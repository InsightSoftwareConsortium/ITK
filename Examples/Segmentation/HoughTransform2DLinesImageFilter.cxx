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
// \doxygen{HoughTransform2DLinesImageFilter} to find straight lines in a
// 2-dimensional image.
//
// First, we include the header files of the filter.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkHoughTransform2DLinesImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkThresholdImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
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

  typedef itk::Image< PixelType, Dimension >            ImageType;
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
  //  Once the image is loaded, we apply a
  //  \doxygen{GradientMagnitudeImageFilter} to segment edges.  This casts
  //  the input image using a \doxygen{CastImageFilter}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter< ImageType, AccumulatorImageType >
    CastingFilterType;
  CastingFilterType::Pointer caster = CastingFilterType::New();

  std::cout << "Applying gradient magnitude filter" << std::endl;

  typedef itk::GradientMagnitudeImageFilter<AccumulatorImageType,
               AccumulatorImageType > GradientFilterType;
  GradientFilterType::Pointer gradFilter =  GradientFilterType::New();

  caster->SetInput(localImage);
  gradFilter->SetInput(caster->GetOutput());
  gradFilter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The next step is to apply a threshold filter on the gradient magnitude
  //  image to keep only bright values. Only pixels with a high value will be
  //  used by the Hough transform filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Thresholding" << std::endl;
  typedef itk::ThresholdImageFilter<AccumulatorImageType> ThresholdFilterType;
  ThresholdFilterType::Pointer threshFilter = ThresholdFilterType::New();

  threshFilter->SetInput( gradFilter->GetOutput());
  threshFilter->SetOutsideValue(0);
  unsigned char threshBelow = 0;
  unsigned char threshAbove = 255;
  threshFilter->ThresholdOutside(threshBelow,threshAbove);
  threshFilter->Update();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We create the HoughTransform2DLinesImageFilter based on the pixel type
  //  of the input image (the resulting image from the ThresholdImageFilter).
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  std::cout << "Computing Hough Map" << std::endl;
  typedef itk::HoughTransform2DLinesImageFilter<AccumulatorPixelType,
                              AccumulatorPixelType>  HoughTransformFilterType;

  HoughTransformFilterType::Pointer houghFilter
                                            = HoughTransformFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We set the input to the filter to be the output of the
  //  ThresholdImageFilter. We set also the number of lines we are looking
  //  for.  Basically, the filter computes the Hough map, blurs it using a
  //  certain variance and finds maxima in the Hough map. After a maximum is
  //  found, the local neighborhood, a circle, is removed from the Hough
  //  map. SetDiscRadius() defines the radius of this disc.
  //
  //  The output of the filter is the accumulator.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  houghFilter->SetInput(threshFilter->GetOutput());
  houghFilter->SetNumberOfLines(atoi(argv[3]));

  if(argc > 4 )
    {
    houghFilter->SetVariance(atof(argv[4]));
    }

  if(argc > 5 )
    {
    houghFilter->SetDiscRadius(atof(argv[5]));
    }
  houghFilter->Update();
  AccumulatorImageType::Pointer localAccumulator = houghFilter->GetOutput();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We can also get the lines as \doxygen{LineSpatialObject}. The
  //  \code{GetLines()} function return a list of those.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  HoughTransformFilterType::LinesListType lines;
  lines = houghFilter->GetLines();
  std::cout << "Found " << lines.size() << " line(s)." << std::endl;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We can then allocate an image to draw the resulting lines as binary
  //  objects.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  unsigned char                            OutputPixelType;
  typedef  itk::Image< OutputPixelType, Dimension > OutputImageType;

  OutputImageType::Pointer  localOutputImage = OutputImageType::New();

  OutputImageType::RegionType region(localImage->GetLargestPossibleRegion());
  localOutputImage->SetRegions(region);
  localOutputImage->CopyInformation(localImage);
  localOutputImage->Allocate(true); // initialize buffer to zero
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We iterate through the list of lines and we draw them.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef HoughTransformFilterType::LinesListType::const_iterator LineIterator;
  LineIterator itLines = lines.begin();
  while( itLines != lines.end() )
    {
    // Software Guide : EndCodeSnippet

    //  Software Guide : BeginLatex
    //
    //  We get the list of points which consists of two points to represent a
    //  straight line.  Then, from these two points, we compute a fixed point
    //  $u$ and a unit vector $\vec{v}$ to parameterize the line.
    //
    //  Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    typedef HoughTransformFilterType::LineType::PointListType  PointListType;

    PointListType                   pointsList = (*itLines)->GetPoints();
    PointListType::const_iterator   itPoints = pointsList.begin();

    double u[2];
    u[0] = (*itPoints).GetPosition()[0];
    u[1] = (*itPoints).GetPosition()[1];
    itPoints++;
    double v[2];
    v[0] = u[0]-(*itPoints).GetPosition()[0];
    v[1] = u[1]-(*itPoints).GetPosition()[1];

    double norm = std::sqrt(v[0]*v[0]+v[1]*v[1]);
    v[0] /= norm;
    v[1] /= norm;
    // Software Guide : EndCodeSnippet

    //  Software Guide : BeginLatex
    //
    //  We draw a white pixels in the output image to represent the line.
    //
    //  Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    ImageType::IndexType localIndex;
    itk::Size<2> size = localOutputImage->GetLargestPossibleRegion().GetSize();
    float diag = std::sqrt((float)( size[0]*size[0] + size[1]*size[1] ));

    for(int i=static_cast<int>(-diag); i<static_cast<int>(diag); i++)
      {
      localIndex[0]=(long int)(u[0]+i*v[0]);
      localIndex[1]=(long int)(u[1]+i*v[1]);

      OutputImageType::RegionType outputRegion =
                          localOutputImage->GetLargestPossibleRegion();

      if( outputRegion.IsInside( localIndex ) )
        {
        localOutputImage->SetPixel( localIndex, 255 );
        }
      }
    itLines++;
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
  writer->SetInput( localOutputImage );

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
