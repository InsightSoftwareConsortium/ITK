/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageSliceIteratorWithIndex.cxx
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
// The \code{itk::ImageSliceIteratorWithIndex} is an extension of the
// \code{itk::ImageLinearIteratorWithIndex} from movement along lines to
// movement along lines \emph{and planes} in an image.  A \emph{slice} is a 2D
// plane spanned by two vectors pointing along coordinate axes.  The two
// coordinate directions which define the slice plane are set as member
// variables.
// 
// \begin{itemize}
// \item \textbf{\code{SetFirstDirection()}} Specifies the first coordinate axis
// direction of the slice plane.
//
// \item \textbf{\code{SetSecondDirection()}} Specifies the second coordinate axis
// direction of the slice plane.
// \end{itemize}
//
// Several new methods control movement from slice to slice.
//
// \begin{itemize}
// 
// \item \textbf{\code{NextSlice()}} Moves the iterator to the beginning pixel
// location of the next slice in the image.  The origin of the next slice is
// calculated by incrementing the current origin index along the fastest
// increasing dimension of the image subspace which excludes the first and
// second dimensions of the iterator.
//
// \item \textbf{\code{PreviousSlice()}} Moves the iterator to the \emph{last
// valid pixel location} in the previous slice.  The origin of the previous
// slice is calculated by decrementing the current origin index along the
// fastest increasing dimension of the image subspace which excludes the first
// and second dimensions of the iterator.
//
// \item \textbf{\code{(IsAtBeginOfSlice())}} Returns true if the iterator
// points to the beginning pixel of the current slice.
//
// \item \textbf{\code{(IsAtEndOfSlice())}} Returns true if the iterator points
// to \emph{one position past the last valid} pixel of the current slice.
// 
// \end{itemize}
//
// The slice iterator moves line by line using \code{NextLine()} and
// \code{PreviousLine()}.  The line direction is parallel to the \emph{second}
// coordinate axis direction of the slice plane (see also
// section~\ref{sec:itkImageLinearIteratorWithIndex}).
//
// The next code example calculates the maximum intensity projection along one
// of the coordinate axes of an image volume.  The algorithm is straightforward
// using \code{itk::ImageSliceIteratorWithIndex}: step slice by slice along the
// specified axis, keeping track of the maximum values at each slice index
// location in a second, 2D image.  The 2D image is the projection image.
//
// We include a header for the const version of the slice iterator. For writing
// values to the 2D projection image, we use the linear iterator from the
// previous section.  The linear iterator is chosen because it can be set to
// follow the same path in its underlying 2D image that the slice iterator
// follows over each slice of the 3D image.
//
// Software Guide : EndLatex

#include "itkImage.h"
#include "vnl/vnl_math.h"

// Software Guide : BeginCodeSnippet
#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char ** argv )
{
  //   Verify the number of parameters on the command line.
  if ( argc < 4 )
    {
      std::cerr << "Missing parameters. " << std::endl;
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0]
                << " inputImageFile outputImageFile projectionDirection"
                << std::endl;
      return -1;
    }
  
// Software Guide : BeginLatex
//
// The pixel type is defined as unsigned short.  For this application, we need
// two image types, a 3D image for the input, and a 2D image for the intensity
// projection.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef unsigned short PixelType;
  typedef itk::Image< PixelType, 2 >  ImageType2D;
  typedef itk::Image< PixelType, 3 >  ImageType3D;
// Software Guide : EndCodeSnippet  

// Software Guide : BeginLatex
//
//  A slice iterator type is defined to walk the input image.
  
//
// Software Guide : EndLatex
  
// Software Guide : BeginCodeSnippet
  typedef itk::ImageLinearIteratorWithIndex< ImageType2D > LinearIteratorType;
  typedef itk::ImageSliceConstIteratorWithIndex< ImageType3D >  SliceIteratorType;
// Software Guide : EndCodeSnippet
  
  typedef itk::ImageFileReader< ImageType3D > ReaderType;
  typedef itk::ImageFileWriter< ImageType2D > WriterType;

  ImageType3D::ConstPointer inputImage;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
      reader->Update();
      inputImage = reader->GetOutput();
    }
  catch ( itk::ExceptionObject &err)
    {
      std::cout << "ExceptionObject caught !" << std::endl; 
      std::cout << err << std::endl; 
      return -1;
    }
  
// Software Guide : BeginLatex
//
// The projection direction is read from the command line. The projection image
// will be the size of the 2D plane orthogonal to the projection direction.
// Its spanning vectors are the two remaining coordinate axes in the volume.
// These axes are recorded in the \code{direction} array.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  unsigned int projectionDirection = static_cast<unsigned int>( ::atoi( argv[3] ) );

  unsigned int i, j;
  unsigned int direction[2];
  for (i = 0, j = 0; i < 3; ++i )
    {
      if (i != projectionDirection)
        {
          direction[j] = i;
          j++;
        }
    }
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The \code{direction} array is now used to define the projection image size
// based on the input image size.  The output image is created so that its
// common dimension(s) with the input image are the same size.  For example, if
// we project along the X axis of the input, the size and origin of the Y axes
// of the input and output will match.  This makes the code slightly more
// complicated, but prevents a counter-intuitive rotation of the output.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType2D::RegionType region;
  ImageType2D::RegionType::SizeType size;
  ImageType2D::RegionType::IndexType index;
  
  index[ direction[0] ] = inputImage->GetRequestedRegion().GetIndex()[ direction[0] ];
  index[ 1- direction[0] ] = inputImage->GetRequestedRegion().GetIndex()[ direction[1] ];
  size[ direction[0] ] = inputImage->GetRequestedRegion().GetSize()[ direction[0] ];
  size[ 1- direction[0] ] = inputImage->GetRequestedRegion().GetSize()[ direction[1] ];

  region.SetSize( size );
  region.SetIndex( index );

  ImageType2D::Pointer outputImage = ImageType2D::New();
  outputImage->SetRegions( region );
  outputImage->Allocate();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we create the necesary iterators.  The const slice iterator walks the 3D input image,
// and the non-const linear iterator walks the 2D output image. The iterators are
// initialized to walk the same linear path through a slice.  Remember that the
// \emph{second} direction of the slice iterator defines the direction linear
// iteration walks within a slice.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  SliceIteratorType  inputIt(  inputImage,  inputImage->GetRequestedRegion() );
  LinearIteratorType outputIt( outputImage, outputImage->GetRequestedRegion() );

  inputIt.SetFirstDirection(  direction[1] );
  inputIt.SetSecondDirection( direction[0] );
  
  outputIt.SetDirection( 1 - direction[0] );
// Software Guide : EndCodeSnippet

// Software Guide: BeginLatex
//
// Now we are ready to compute the projection.  The first step is to initialize
// all of the projection values to zero.  The projection values are then updated row
// by row from the first slice of the input.  At the end of the first slice, the input
// iterator steps to the first row in the next slice, while the output iterator,
// whose underlying image consists of only one slice, rewinds to its first row.
// The process repeats until the last slice of the input is processed.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  outputIt.GoToBegin();
  while ( ! outputIt.IsAtEnd() )
    {
      while ( ! outputIt.IsAtEndOfLine() )
        {
          outputIt.Set( 0 );
          ++outputIt;
        }
      outputIt.NextLine();
    }

  inputIt.GoToBegin();
  outputIt.GoToBegin();
  
  while( !inputIt.IsAtEnd() )
    {
      while ( !inputIt.IsAtEndOfSlice() )
        {
          while ( !inputIt.IsAtEndOfLine() )
            {
              outputIt.Set( vnl_math_max( outputIt.Get(), inputIt.Get() ));
              ++inputIt;
              ++outputIt;
            }
          outputIt.NextLine();
          inputIt.NextLine();

        }
      outputIt.GoToBegin();
      inputIt.NextSlice();
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

  return 0;
}
