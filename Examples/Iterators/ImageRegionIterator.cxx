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
// The \code{itk::ImageRegionIterator} is the most commonly used and least
// specialized of the ITK image iterator classes.  It has all of the
// functionality described in the preceding section.
//
// The following example illustrates the use of
// \code{itk::ImageRegionConstIterator} and \code{itk::ImageRegionIterator}. It
// is an application which crops a region from an image.  The iterators are
// used to copy pixel values from a sub-region in an input image to a smaller output
// image.
//
// We begin by including the appropriate header files.
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkRGBPixel.h"
// Software Guide : BeginCodeSnippet
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


// Iterators1 /home/cates/Insight/Examples/Data/VisibleWomanEyeSlice.png out.png 20 10 100 100
//%In fact, with iterators,
//%algorithms often generalize to higher dimensions automatically.  Note how the
//%code in figure~\ref{fig:ImageIterators} is equally correct for images of two,
//%three, or even ten dimensions.  The alternative to using iterators in this
//%example would be a set of nested \code{for} loops.  Such nested loops would
//%also require knowing the size of the image.  

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
// Next we define types for the ITK objects used.  This application will
// process color images, so we use an RGB pixel type. The iterator classes
// have a single template parameter, the image type.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;
  
  typedef itk::RGBPixel< unsigned char > RGBPixelType;
  typedef itk::Image< RGBPixelType, Dimension >  ImageType;
  
  typedef itk::ImageRegionConstIterator< ImageType > ConstIteratorType;
  typedef itk::ImageRegionIterator< ImageType>       IteratorType;
  // Software Guide : EndCodeSnippet
  
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

// Software Guide : BeginLatex
// 
// Information about the region to copy to the output is read from the
// command line.  Notice that the region object type can be defined by the
// image type.  A region definition needs a size and a starting index
// (section~\ref{}).
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
// After reading the input image and checking that the desired region is, in
// fact, contained in the input, we allocate an output image using the region
// the that will be copied.  This might be an important detail later if the
// cropped region requires registering against the original data.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType::Pointer outputImage = ImageType::New();
  outputImage->SetRegions( region );
  outputImage->Allocate();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// All the necessary images and region definitions are now in place.  All that
// is left is to create the iterators and perform the copy.  Note that image
// iterators are not smart pointers, their constructors are called directly.
// Also notice how the input and output iterators are defined over the
// \emph{same region} even though they walk images of different sizes.
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
// The \code{for} loop above is a very common construct in ITK.  The beauty of
// those four simple lines of code is that they are equally valid for one, two,
// three, or even ten dimensional data.  Consider the ugly alternative of ten
// nested \code{for} loops for traversing an image.
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

  return 0;
}


