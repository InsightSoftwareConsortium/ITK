/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageLinearIteratorWithIndex.cxx
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
// The \doxygen{ImageLinearIteratorWithIndex} is designed for line-by-line
// processing of an image.  It walks a linear path along a selected image
// direction parallel to one of the coordinate axes of the image. This
// iterator allows developers to work with an image as a set of parallel lines
// spanning the selected image dimension.
//
// \index{Iterators!and image lines}
//
// Like all other ITK image iterators, movement is constrained to within an
// image region, $R$.  The line $\ell$ through which the iterator moves is
// defined by a selected direction and an origin.  The origin itself can be
// iterated across the pixels along the lower boundary of $R$.  The line $\ell$
// extends from the origin to the upper boundary of $R$.
//    
// Several additional methods are defined for this iterator to control movement
// of the iterator along the line $\ell$ and movement of the origin of $\ell$.
//
// %Might need a figure here to describe this iterator.
//
// \begin{itemize}
//
// \index{itk::ImageLinearIteratorWithIndex!NextLine()}
//
// \item \textbf{\code{NextLine()}} Moves the iterator to the beginning pixel
// location of the next line in the image.  The origin of the next line is
// determined by incrementing the current origin along the fastest increasing
// dimension of the subspace of the image that excludes the selected dimension.
//
//
// \index{itk::ImageLinearIteratorWithIndex!PreviousLine()}
//
// \item \textbf{\code{PreviousLine()}} Moves the iterator to the \emph{last valid
// pixel location} in the previous line. The origin of the previous line is
// determined by decrementing the current origin along the fastest increasing
// dimension of the subspace of the image that excludes the selected dimension.
//
// \index{itk::ImageLinearIteratorWithIndex!GoToBeginOfLine()}
// \index{itk::ImageLinearIteratorWithIndex!GoToReverseBeginOfLine()}
//
// \item \textbf{\code{GoToBeginOfLine()}} Moves the iterator to the beginning
// pixel of the current line.
//
// \item \textbf{\code{GoToEndOfLine()}}  Move the iterator to 
// \emph{one past} the last valid pixel of the current line.
//
//
// \index{itk::ImageLinearIteratorWithIndex!IsAtReverseEndOfLine()}
// \index{itk::ImageLinearIteratorWithIndex!IsAtEndOfLine()}
//
// \item \textbf{\code{IsAtReverseEndOfLine()}} Returns true if the iterator points
// to \emph{one position before} the beginning pixel of the current line.
//
// \item \textbf{\code{IsAtEndOfLine()}}  Returns true if the iterator points to
// \emph{one position past} the last valid pixel of the current line.
// \end{itemize}
//
// The following code example shows how to use the
// \doxygen{ImageLinearIteratorWithIndex}.  It implements the same
// algorithm as in the previous example, flipping an image across its $x$-axis.
// Two line iterators are iterated in opposite directions across the $x$-axis.
// After a line is processed, the iterators are stepped to the next line (down the
// $y$-axis).
//
// \index{itk::ImageLinearIteratorWithIndex!example of using|(}
//
// Headers for both the const and non-const versions are needed.
//
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkRGBPixel.h"
// Software Guide : BeginCodeSnippet
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char *argv[] )
{
  // Verify the number of parameters on the command line.
  if ( argc < 3 )
    {
      std::cerr << "Missing parameters. " << std::endl;
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0]
                << " inputImageFile outputImageFile"
                << std::endl;
      return -1;
    }

// Software Guide : BeginLatex
//
// The RGB Image and pixel types are defined as in the previous example.  The 
// \doxygen{ImageLinearIteratorWithIndex} class and its const version each have
// single template parameters, the image type.
//
// Software Guide : EndLatex

  const unsigned int Dimension = 2;
  
  typedef itk::RGBPixel< unsigned char > RGBPixelType;
  typedef itk::Image< RGBPixelType, Dimension >  ImageType;

// Software Guide : BeginCodeSnippet
  typedef itk::ImageLinearIteratorWithIndex< ImageType >       IteratorType;
  typedef itk::ImageLinearConstIteratorWithIndex< ImageType >  ConstIteratorType;
// Software Guide : EndCodeSnippet
  
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  ImageType::ConstPointer inputImage;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
      reader->Update();
      inputImage = reader->GetOutput();
    }
  catch ( itk::ExceptionObject &err)
    {
      std::cout << "ExceptionObject caught a !" << std::endl; 
      std::cout << err << std::endl; 
      return -1;
    }

// Software Guide : BeginLatex
//
// After reading the input image, we allocate an output image that of the same
// size, spacing, and origin.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType::Pointer outputImage = ImageType::New();
  outputImage->SetRegions( inputImage->GetRequestedRegion() );
  outputImage->CopyInformation( inputImage );
  outputImage->Allocate();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we create the two iterators.  The const iterator walks the input image,
// and the non-const iterator walks the output image.  The iterators are
// initialized over the same region.  The direction of iteration is set to 0,
// the x dimension.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ConstIteratorType inputIt(  inputImage,  inputImage->GetRequestedRegion() );
  IteratorType outputIt( outputImage, inputImage->GetRequestedRegion() );

  inputIt.SetDirection(0);
  outputIt.SetDirection(0);
// Software Guide : EndCodeSnippet

// Software Guide: BeginLatex
//
// Each line in the input is copied to the output.  The input iterator moves
// forward across columns while the output iterator moves backwards.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  for ( inputIt.GoToBegin(),  outputIt.GoToBegin(); ! inputIt.IsAtEnd();
        outputIt.NextLine(),  inputIt.NextLine())
    {
      inputIt.GoToBeginOfLine();
      outputIt.GoToEndOfLine();
      --outputIt;
      while ( ! inputIt.IsAtEndOfLine() )
        {
          outputIt.Set( inputIt.Get() );
          ++inputIt;
          --outputIt;
        }
    }
// Software Guide : EndCodeSnippet

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
// Running this example on \code{VisibleWomanEyeSlice.png} produces
// the same output image shown in
// figure~\ref{fig:ImageRegionIteratorWithIndexExample}.
//
// \index{itk::ImageLinearIteratorWithIndex!example of using|)}
// Software Guide : EndLatex
  
  return 0;
}
