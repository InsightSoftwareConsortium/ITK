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

//  Software Guide : BeginCommandLineArgs
//     INPUTS:  {FatMRISlice.png}
//     OUTPUTS: {ImageRegionIteratorOutput.png}
//     ARGUMENTS:    20 70 210 140
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// \index{Iterators!speed}
// The \doxygen{ImageRegionIterator} is optimized for
// iteration speed and is the first choice for iterative, pixel-wise operations
// when location in the image is not
// important. ImageRegionIterator is the least specialized of the ITK
// image iterator classes.  It implements all of the methods described in the
// preceding section.
//
// The following example illustrates the use of
// \doxygen{ImageRegionConstIterator} and ImageRegionIterator.
// Most of the code constructs introduced apply to other ITK iterators as
// well. This simple application crops a subregion from an image by copying
// its pixel values into to a second, smaller image.
//
// \index{Iterators!and image regions}
// \index{itk::ImageRegionIterator!example of using|(}
// We begin by including the appropriate header files.
//
// Software Guide : EndLatex

#include "itkImage.h"
// Software Guide : BeginCodeSnippet
#include "itkImageRegionIterator.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char *argv[] )
{
  // Verify the number of parameters on the command line.
  if ( argc < 7 )
    {
      std::cerr << "Missing parameters. " << std::endl;
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0]
                << " inputImageFile outputImageFile startX startY sizeX sizeY"
                << std::endl;
      return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // Next we define a pixel type and corresponding image type. ITK iterator
  // classes expect the image type as their template parameter.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;

  typedef unsigned char                      PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::ImageRegionConstIterator< ImageType > ConstIteratorType;
  typedef itk::ImageRegionIterator< ImageType>       IteratorType;
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  // Software Guide : BeginLatex
  //
  // Information about the subregion to copy is read from the command line. The
  // subregion is defined by an \doxygen{ImageRegion} object, with a starting
  // grid index and a size (Section~\ref{sec:ImageSection}).
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::RegionType inputRegion;

  ImageType::RegionType::IndexType inputStart;
  ImageType::RegionType::SizeType  size;

  inputStart[0] = ::atoi( argv[3] );
  inputStart[1] = ::atoi( argv[4] );

  size[0]  = ::atoi( argv[5] );
  size[1]  = ::atoi( argv[6] );

  inputRegion.SetSize( size );
  inputRegion.SetIndex( inputStart );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The destination region in the output image is defined using the input region
  // size, but a different start index.  The starting index for the destination
  // region is the corner of the newly generated image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::RegionType outputRegion;

  ImageType::RegionType::IndexType outputStart;

  outputStart[0] = 0;
  outputStart[1] = 0;

  outputRegion.SetSize( size );
  outputRegion.SetIndex( outputStart );
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
    reader->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

    // Check that the region is contained within the input image.
  if ( ! reader->GetOutput()->GetRequestedRegion().IsInside( inputRegion ) )
    {
    std::cerr << "Error" << std::endl;
    std::cerr << "The region " << inputRegion << "is not contained within the input image region "
              << reader->GetOutput()->GetRequestedRegion() << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // After reading the input image and checking that the desired subregion is,
  // in fact, contained in the input, we allocate an output image.  It is
  // fundamental to set valid values to some of the basic image information
  // during the copying process.
  // In particular, the starting index of the output region
  // is now filled up with zero values and the coordinates of the physical
  // origin are computed as a shift from the origin of the input image. This is
  // quite important since it will allow us to later
  // register the extracted region against the original image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::Pointer outputImage = ImageType::New();
  outputImage->SetRegions( outputRegion );
  const ImageType::SpacingType& spacing = reader->GetOutput()->GetSpacing();
  const ImageType::PointType& inputOrigin = reader->GetOutput()->GetOrigin();
  double   outputOrigin[ Dimension ];

  for(unsigned int i=0; i< Dimension; i++)
    {
    outputOrigin[i] = inputOrigin[i] + spacing[i] * inputStart[i];
    }

  outputImage->SetSpacing( spacing );
  outputImage->SetOrigin(  outputOrigin );
  outputImage->Allocate();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // \index{Iterators!construction of} \index{Iterators!and image regions}
  // The necessary images and region definitions are now in place.  All that is
  // left to do is to create the iterators and perform the copy.  Note that image
  // iterators are not accessed via smart pointers so they are light-weight
  // objects that are instantiated on the stack.  Also notice how the input and
  // output iterators are defined over the \emph{same corresponding region}.  Though the
  // images are different sizes, they both contain the same target subregion.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ConstIteratorType inputIt(   reader->GetOutput(), inputRegion  );
  IteratorType      outputIt(  outputImage,         outputRegion );

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while( !inputIt.IsAtEnd() )
    {
    outputIt.Set(  inputIt.Get()  );
    ++inputIt;
    ++outputIt;
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // \index{Iterators!image dimensionality}
  //  The \code{while} loop above is a common construct in ITK.  The beauty of
  //  these four lines of code is that they are equally valid for one, two,
  //  three, or even ten dimensional data, and no knowledge of the size of the
  //  image is necessary.  Consider the ugly alternative of ten nested
  //  \code{for} loops for traversing an image.
  //
  // Software Guide : EndLatex

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( outputImage );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // Let's run this example on the image \code{FatMRISlice.png} found
  // in \code{Examples/Data}.  The command line arguments specify the
  // input and output file names, then the $x$, $y$ origin and the $x$, $y$ size
  // of the cropped subregion.
  //
  // \small
  // \begin{verbatim}
  // ImageRegionIterator FatMRISlice.png ImageRegionIteratorOutput.png 20 70 210 140
  // \end{verbatim}
  // \normalsize
  //
  // The output is the cropped subregion shown in
  // Figure~\ref{fig:ImageRegionIteratorOutput}.
  //
  // \begin{figure}
  // \centering
  // \includegraphics[width=0.4\textwidth]{FatMRISlice}
  // \includegraphics[width=0.3\textwidth]{ImageRegionIteratorOutput}
  // \itkcaption[Copying an image subregion using ImageRegionIterator]{Cropping a
  // region from an image.  The original image is shown at left.  The image on
  // the right is the result of applying the ImageRegionIterator example code.}
  // \protect\label{fig:ImageRegionIteratorOutput}
  // \end{figure}
  //
  // \index{itk::ImageRegionIterator!example of using|)}
  //
  // Software Guide : EndLatex

  return EXIT_SUCCESS;
}
