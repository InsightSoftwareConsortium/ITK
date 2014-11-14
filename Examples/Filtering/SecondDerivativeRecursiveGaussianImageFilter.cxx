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
//  This example illustrates how to compute second derivatives of
//  a 3D image using the \doxygen{RecursiveGaussianImageFilter}.
//
//  It's good to be able to compute the raw derivative without any smoothing,
//  but this can be problematic in a medical imaging scenario, when images will
//  often have a certain amount of noise. It's almost always more desirable to
//  include a smoothing step first, where an image is convolved with a Gaussian
//  kernel in whichever directions the user desires a derivative. The nature of
//  the Gaussian kernel makes it easy to combine these two steps into one,
//  using an infinite impulse response (IIR) filter. In this example, all the
//  second derivatives are computed independently in the same way, as if they
//  were intended to be used for building the Hessian matrix of the image (a
//  square matrix of second-order derivatives of an image, which is useful in
//  many image processing techniques).
//
//  Software Guide : EndLatex


//  Software Guide : BeginLatex
//
//  First, we will include the relevant header files: the
//  itkRecursiveGaussianImageFilter, the image reader, writer, and duplicator.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkRecursiveGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageDuplicator.h"
#include <string>
//  Software Guide : EndCodeSnippet

int main(int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputPrefix  [sigma] " << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Next, we declare our pixel type and output pixel type to be floats, and
  //  our image dimension to be $3$.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float            PixelType;
  typedef float            OutputPixelType;

  const unsigned int  Dimension = 3;
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Using these definitions, define the image types, reader and writer types,
  //  and duplicator types, which are templated over the pixel types and
  //  dimension.  Then, instantiate the reader, writer, and duplicator with
  //  the \code{New()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< PixelType,       Dimension >  ImageType;
  typedef itk::Image< OutputPixelType, Dimension >  OutputImageType;

  typedef itk::ImageFileReader< ImageType       >   ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >   WriterType;

  typedef itk::ImageDuplicator< OutputImageType >   DuplicatorType;

  typedef itk::RecursiveGaussianImageFilter<
                                      ImageType,
                                      ImageType >  FilterType;

  ReaderType::Pointer  reader  = ReaderType::New();
  WriterType::Pointer  writer  = WriterType::New();

  DuplicatorType::Pointer duplicator  = DuplicatorType::New();
  // Software Guide : EndCodeSnippet

  reader->SetFileName( argv[1] );

  std::string outputPrefix = argv[2];
  std::string outputFileName;

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem reading the input file" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Here we create three new filters. For each derivative we take, we will
  //  want to smooth in that direction first. So after the filters are created,
  //  each is given a dimension, and set to (in this example) the same sigma.
  //  Note that here, $\sigma$ represents the standard deviation, whereas the
  //  \doxygen{DiscreteGaussianImageFilter} exposes the \code{SetVariance}
  //  method.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  FilterType::Pointer ga = FilterType::New();
  FilterType::Pointer gb = FilterType::New();
  FilterType::Pointer gc = FilterType::New();

  ga->SetDirection( 0 );
  gb->SetDirection( 1 );
  gc->SetDirection( 2 );

  if( argc > 3 )
    {
    const float sigma = atof( argv[3] );
    ga->SetSigma( sigma );
    gb->SetSigma( sigma );
    gc->SetSigma( sigma );
    }
  //  Software Guide: EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  First we will compute the second derivative of the $z$-direction.
  //  In order to do this, we smooth in the $x$- and $y$- directions, and
  //  finally smooth and compute the derivative in the $z$-direction. Taking
  //  the zero-order derivative is equivalent to simply smoothing in that
  //  direction. This result is commonly notated $I_{zz}$.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  ga->SetZeroOrder();
  gb->SetZeroOrder();
  gc->SetSecondOrder();

  ImageType::Pointer inputImage = reader->GetOutput();

  ga->SetInput( inputImage );
  gb->SetInput( ga->GetOutput() );
  gc->SetInput( gb->GetOutput() );

  duplicator->SetInputImage( gc->GetOutput() );

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Izz = duplicator->GetModifiableOutput();
  //  Software Guide: EndCodeSnippet

  writer->SetInput( Izz );
  outputFileName = outputPrefix + "-Izz.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();

  //  Software Guide : BeginLatex
  //
  //  Recall that \code{gc} is the filter responsible for taking the second
  //  derivative. We can now take advantage of the pipeline architecture and,
  //  without much hassle, switch the direction of \code{gc} and \code{gb},
  //  so that \code{gc} now takes the derivatives in the $y$-direction. Now we
  //  only need to call \code{Update()} on \code{gc} to re-run the entire pipeline
  //  from \code{ga} to \code{gc}, obtaining the second-order derivative in the
  //  $y$-direction, which is commonly notated $I_{yy}$.
  //
  // Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  gc->SetDirection( 1 );  // gc now works along Y
  gb->SetDirection( 2 );  // gb now works along Z

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Iyy = duplicator->GetModifiableOutput();
  //  Software Guide : EndCodeSnippet

  writer->SetInput( Iyy );
  outputFileName = outputPrefix + "-Iyy.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();

  //  Software Guide : BeginLatex
  //
  //  Now we switch the directions of \code{gc} with that of \code{ga} in order
  //  to take the derivatives in the $x$-direction. This will give us $I_{xx}$.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  gc->SetDirection( 0 );  // gc now works along X
  ga->SetDirection( 1 );  // ga now works along Y

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Ixx = duplicator->GetModifiableOutput();
  //  Software Guide : EndCodeSnippet

  writer->SetInput( Ixx );
  outputFileName = outputPrefix + "-Ixx.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();

  //  Software Guide : BeginLatex
  //
  //  Now we can reset the directions to their original values, and compute
  //  first derivatives in different directions. Since we set both \code{gb}
  //  and \code{gc} to compute first derivatives, and \code{ga} to zero-order
  //  (which is only smoothing) we will obtain $I_{yz}$.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  ga->SetDirection( 0 );
  gb->SetDirection( 1 );
  gc->SetDirection( 2 );

  ga->SetZeroOrder();
  gb->SetFirstOrder();
  gc->SetFirstOrder();

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Iyz = duplicator->GetModifiableOutput();
  //  Software Guide : EndCodeSnippet

  writer->SetInput( Iyz );
  outputFileName = outputPrefix + "-Iyz.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();

  //  Software Guide : BeginLatex
  //
  //  Here is how you may easily obtain $I_{xz}$.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  ga->SetDirection( 1 );
  gb->SetDirection( 0 );
  gc->SetDirection( 2 );

  ga->SetZeroOrder();
  gb->SetFirstOrder();
  gc->SetFirstOrder();

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Ixz = duplicator->GetModifiableOutput();
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  For the sake of completeness, here is how you may compute
  //  $I_{xz}$ and $I_{xy}$.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  writer->SetInput( Ixz );
  outputFileName = outputPrefix + "-Ixz.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();

  ga->SetDirection( 2 );
  gb->SetDirection( 0 );
  gc->SetDirection( 1 );

  ga->SetZeroOrder();
  gb->SetFirstOrder();
  gc->SetFirstOrder();

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Ixy = duplicator->GetModifiableOutput();

  writer->SetInput( Ixy );
  outputFileName = outputPrefix + "-Ixy.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();
  // Software Guide : EndCodeSnippet

return EXIT_SUCCESS;
}
