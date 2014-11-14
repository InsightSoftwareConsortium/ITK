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
//  Due to the use of
//  \href{http://www.boost.org/more/generic_programming.html}{Generic
//  Programming} in the toolkit, most types are resolved at compile-time. Few
//  decisions regarding type conversion are left to run-time. It is up to the
//  user to anticipate the pixel type-conversions required in the data
//  pipeline. In medical imaging applications it is usually not desirable
//  to use a general pixel type since this may result in the loss of
//  valuable information.
//
//  This section introduces the mechanisms for explicit casting of images that
//  flow through the pipeline. The following four filters are treated in this
//  section: \doxygen{CastImageFilter}, \doxygen{RescaleIntensityImageFilter},
//  \doxygen{ShiftScaleImageFilter} and \doxygen{NormalizeImageFilter}.  These
//  filters are not directly related to each other except that they all modify
//  pixel values.  They are presented together here for the purpose of
//  comparing their individual features.
//
//  The CastImageFilter is a very simple filter that acts pixel-wise on an
//  input image, casting every pixel to the type of the output image. Note that
//  this filter does not perform any arithmetic operation on the
//  intensities. Applying CastImageFilter is equivalent to performing a
//  \code{C-Style} cast on every pixel.
//
//  \code{ outputPixel = static\_cast<OutputPixelType>( inputPixel ) }
//
//  The RescaleIntensityImageFilter linearly scales the
//  pixel values in such a way that the minimum and maximum values of the
//  input are mapped to minimum and maximum values provided by the
//  user. This is a typical process for forcing the dynamic range of the image
//  to fit within a particular scale and is common for image display.
//  The linear transformation applied by this filter can be expressed as
//
//  \[ outputPixel = ( inputPixel - inpMin) \times
//                    \frac{(outMax - outMin )}{(inpMax-inpMin)} + outMin \].
//
//  The ShiftScaleImageFilter also applies a linear transformation to
//  the intensities of the input image, but the transformation is specified
//  by the user in the form of a multiplying factor and a value to be added.
//  This can be expressed as
//
//  \[  outputPixel = \left( inputPixel  + Shift \right) \times Scale\].
//
//  The parameters of the linear transformation applied by the
//  NormalizeImageFilter are computed internally such that the
//  statistical distribution of gray levels in the output image have zero
//  mean and a variance of one. This intensity correction is particularly
//  useful in registration applications as a preprocessing step to the
//  evaluation of mutual information metrics. The linear transformation of
//  NormalizeImageFilter is given as
//
//  \[ outputPixel = \frac{( inputPixel - mean )}{ \sqrt{ variance } } \].
//
//  \index{Casting Images}
//  \index{itk::CastImageFilter}
//  \index{itk::RescaleIntensityImageFilter}
//  \index{itk::ShiftScaleImageFilter}
//  \index{itk::NormalizeImageFilter}
//  \index{itk::ShiftScaleImageFilter!header}
//  \index{itk::RescaleIntensityImageFilter!header}
//  \index{itk::NormalizeImageFilter!header}
//  \index{itk::CastImageFilter!header}
//
//  As usual, the first step required to use these filters is to include their
//  header files.
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"

// Software Guide : BeginCodeSnippet
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkNormalizeImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "   inputImageFile " << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Let's define pixel types for the input and output images.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   unsigned char    InputPixelType;
  typedef   float            OutputPixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then, the input and output image types are defined.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  3 >   InputImageType;
  typedef itk::Image< OutputPixelType, 3 >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The filters are instantiated using the defined image types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter<
               InputImageType, OutputImageType >  CastFilterType;

  typedef itk::RescaleIntensityImageFilter<
               InputImageType, OutputImageType >  RescaleFilterType;

  typedef itk::ShiftScaleImageFilter<
               InputImageType, OutputImageType >  ShiftScaleFilterType;

  typedef itk::NormalizeImageFilter<
               InputImageType, OutputImageType >  NormalizeFilterType;
  // Software Guide : EndCodeSnippet

  ReaderType::Pointer reader = ReaderType::New();


  //  Software Guide : BeginLatex
  //
  //  Object filters are created by invoking the \code{New()} method and
  //  assigning the result to \doxygen{SmartPointer}s.
  //
  //  \index{itk::ShiftScaleImageFilter!New()}
  //  \index{itk::RescaleIntensityImageFilter!New()}
  //  \index{itk::NormalizeImageFilter!New()}
  //  \index{itk::CastImageFilter!New()}
  //  \index{itk::ShiftScaleImageFilter!Pointer}
  //  \index{itk::RescaleIntensityImageFilter!Pointer}
  //  \index{itk::NormalizeImageFilter!Pointer}
  //  \index{itk::CastImageFilter!Pointer}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  CastFilterType::Pointer       castFilter       = CastFilterType::New();
  RescaleFilterType::Pointer    rescaleFilter    = RescaleFilterType::New();
  ShiftScaleFilterType::Pointer shiftFilter      = ShiftScaleFilterType::New();
  NormalizeFilterType::Pointer  normalizeFilter = NormalizeFilterType::New();
  // Software Guide : EndCodeSnippet


  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  The output of a reader filter (whose creation is not shown here) is now
  //  connected as input to the various casting filters.
  //
  //  \index{itk::ShiftScaleImageFilter!SetInput()}
  //  \index{itk::RescaleIntensityImageFilter!SetInput()}
  //  \index{itk::NormalizeImageFilter!SetInput()}
  //  \index{itk::CastImageFilter!SetInput()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  castFilter->SetInput(       reader->GetOutput() );
  shiftFilter->SetInput(      reader->GetOutput() );
  rescaleFilter->SetInput(    reader->GetOutput() );
  normalizeFilter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Next we proceed to setup the parameters required by each filter. The
  //  CastImageFilter and the NormalizeImageFilter do not
  //  require any parameters. The RescaleIntensityImageFilter, on
  //  the other hand, requires the user to provide the desired minimum and
  //  maximum pixel values of the output image. This is done by using the
  //  \code{SetOutputMinimum()} and \code{SetOutputMaximum()} methods as
  //  illustrated below.
  //
  //  \index{itk::RescaleIntensityImageFilter!SetOutputMinimum()}
  //  \index{itk::RescaleIntensityImageFilter!SetOutputMaximum()}
  //  \index{SetOutputMinimum()!itk::RescaleIntensityImageFilter}
  //  \index{SetOutputMaximum()!itk::RescaleIntensityImageFilter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  rescaleFilter->SetOutputMinimum(  10 );
  rescaleFilter->SetOutputMaximum( 250 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The ShiftScaleImageFilter requires a multiplication factor (scale) and a
  //  post-scaling additive value (shift). The methods \code{SetScale()} and
  //  \code{SetShift()} are used, respectively, to set these values.
  //
  //  \index{itk::ShiftScaleImageFilter!SetShift()}
  //  \index{itk::ShiftScaleImageFilter!SetScale()}
  //  \index{SetShift()!itk::ShiftScaleImageFilter}
  //  \index{SetScale()!itk::ShiftScaleImageFilter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  shiftFilter->SetScale( 1.2 );
  shiftFilter->SetShift( 25 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally, the filters are executed by invoking the \code{Update()} method.
  //
  //  \index{itk::ShiftScaleImageFilter!Update()}
  //  \index{itk::RescaleIntensityImageFilter!Update()}
  //  \index{itk::NormalizeImageFilter!Update()}
  //  \index{itk::CastImageFilter!Update()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  castFilter->Update();
  shiftFilter->Update();
  rescaleFilter->Update();
  normalizeFilter->Update();
  // Software Guide : EndCodeSnippet


  return EXIT_SUCCESS;
}
