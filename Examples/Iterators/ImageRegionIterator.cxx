/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegionIterator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// \index{Iterators!speed}
// The \doxygen{ImageRegionIterator} has been optimized for iteration speed.
// It is the first choice for iterative, pixel-wise operations where location
// in the image is not important and no special iteration path through the
// image is required.  \doxygen{ImageRegionIterator} is the most commonly
// used and least specialized of the ITK image iterator classes.  It implements
// all of the methods described in the preceding section.
//
// The following example illustrates the use of
// \doxygen{ImageRegionConstIterator} and \doxygen{ImageRegionIterator}.
// Most of the code constructs introduced apply to other ITK iterators as
// well. This simple application crops a subregion from an image by copying
// pixel values into to a second image.
//
// \index{Iterators!and image regions}
// \index{itk::ImageRegionIterator!example of using|(}
// We begin by including the appropriate header files.
// Software Guide : EndLatex

#include "itkImage.h"
// Software Guide : BeginCodeSnippet
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char ** argv )
{
  // Verify the number of parameters on the command line.
  if ( argc < 7 )
    {
      std::cerr << "Missing parameters. " << std::endl;
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0]
                << " inputImageFile outputImageFile startX startY sizeX sizeY"
                << std::endl;
      return -1;
    }

// Software Guide : BeginLatex
//
// Next we define a pixel type and corresponding image type. ITK iterator classes
// expect the image type as their template parameter.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;
  
  typedef unsigned short PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;
  
  typedef itk::ImageRegionConstIterator< ImageType > ConstIteratorType;
  typedef itk::ImageRegionIterator< ImageType>       IteratorType;
  // Software Guide : EndCodeSnippet
  
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

// Software Guide : BeginLatex
// 
// Information about the subregion to copy is read from the command line. The
// subregion is defined by an \doxygen{ImageRegion} object, with a starting
// grid index and a size (section~\ref{sec:ImageSection}).
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType::RegionType region;
  ImageType::RegionType::IndexType index;
  ImageType::RegionType::SizeType  size;

  index[0] = ::atoi( argv[3] );
  index[1] = ::atoi( argv[4] );
  size[0]  = ::atoi( argv[5] );
  size[1]  = ::atoi( argv[6] );

  region.SetSize(size);
  region.SetIndex(index);
// Software Guide : EndCodeSnippet
  
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
      reader->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
      std::cout << "ExceptionObject caught !" << std::endl; 
      std::cout << err << std::endl; 
      return -1;
    }

  // Check that the region is contained within the input image.
  if ( ! reader->GetOutput()->GetRequestedRegion().IsInside( region ) )
    {
      std::cerr << "Error" << std::endl;
      std::cerr << "The region " << region << "is not contained within the input image region "
                << reader->GetOutput()->GetRequestedRegion() << std::endl;
      return -1;
    }

// Software Guide : BeginLatex
//
// After reading the input image and checking that the desired subregion is,
// in fact, contained in the input, we allocate an output image.  Allocating
// this image with the same region object we just created guarantees that the output image has
// the same starting index and size as the extracted subregion.  Preserving
// the starting index may be an important detail later if the cropped image
// requires registering against the original image. The origin and spacing
// information is passed to the new image by \code{CopyInformation()}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType::Pointer outputImage = ImageType::New();
  outputImage->SetRegions( region );
  outputImage->CopyInformation( reader->GetOutput() );
  outputImage->Allocate();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// \index{Iterators!construction of}
// \index{Iterators!and image regions}
// The necessary images and region definitions are now in place.  All that is
// left is to create the iterators and perform the copy.  Note that image
// iterators are not smart pointers so their constructors are called directly.
// Also notice how the input and output iterators are defined over the
// \emph{same region}.  Though the images are different sizes, they both
// contain the same target subregion.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ConstIteratorType inputIt( reader->GetOutput(), region );
  IteratorType outputIt( outputImage, region );

  for ( inputIt.GoToBegin(), outputIt.GoToBegin(); !inputIt.IsAtEnd();
        ++inputIt, ++outputIt)
    {
      outputIt.Set(inputIt.Get());
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// \index{Iterators!image dimensionality}
// The \code{for} loop above is a common construct in ITK.  The beauty of these
// four lines of code is that they are equally valid for one, two, three, or
// even ten dimensional data.  Consider the ugly alternative of ten nested
// \code{for} loops for traversing an image, which would also require explicit
// knowledge of the size of each image dimension.
//
// Software Guide : EndLatex
  
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput(outputImage);
  try
    {
      writer->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
      std::cout << "ExceptionObject caught !" << std::endl; 
      std::cout << err << std::endl; 
      return -1;   
}

// Software Guide : BeginLatex
//
// Let's run this example on the image \code{FatMRISlice.png} found
// in \code{Insight/Examples/Data}.  The command line arguments specify the
// input and output file names, then the $x$, $y$ origin and the $x$, $y$ size
// of the cropped subregion.
//
// \begin{verbatim}
// ImageRegionIterator FatMRISlice.png ImageRegionIteratorOutput.png 20 70 210 140
// \end{verbatim}
//
// The output is the cropped subregion shown in figure~\ref{fig:ImageRegionIteratorOutput}.
//
// \begin{figure}
// \centering
// \includegraphics[width=0.4\textwidth]{FatMRISlice.eps}
// \includegraphics[width=0.3\textwidth]{ImageRegionIteratorOutput.eps}
// \caption[Copying an image subregion using ImageRegionIterator]{Cropping a
// region from an image.  The original image is shown at left.  The image on
// the right is the result of applying the ImageRegionIterator example code.}
// \protect\label{fig:ImageRegionIteratorOutput}
// \end{figure}
// 
// \index{itk::ImageRegionIterator!example of using|)}
//
// Software Guide : EndLatex

  return 0;
}


