/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    NeighborhoodIterators2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"

// Software Guide : BeginLatex
//
// In this example, the Sobel edge-detection routine is reimplemented using
// convolution filtering.  Convolution filtering is a standard image processing
// technique that is implemented numerically by successive inner product
// operations between an image neighborhood and a convolution kernel
// \cite{Gonzalez1993} \cite{Castleman1993}.
//
// ITK has an implementation of the Sobel convolution kernel in 2D and 3D
// called the \doxygen{itk::SobelOperator}, which is a member of a general
// class of objects known as neighborhood operators.  Other neighborhood operators
// include derivative and Gaussian convolution kernels as well as morphological
// operators.
// 
// In this example, convolution filtering is done by taking the inner product
// of the Sobel operator with the neighborhood iterator at each image pixel
// index. The resulting values are written to an output image buffer as before.
// Many image processing algorithms in ITK are implemented using the basic
// procedure illustrated here.  \code{Itk::NeighborhoodOperatorImageFilter} is
// a generalization of this example to ND and arbitrary convolution kernels.
//
//. A few additional header files are necessary for this example.  The
// \doxygen{NeighborhoodInnerProduct} object is a function object that takes the
// inner product between a neighborhood operator and an image neighborhood.
// 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSobelOperator.h"
#include "itkNeighborhoodInnerProduct.h"
// Software Guide : EndCodeSnippet

int main( int argc, char ** argv )
{
  if ( argc < 4 )
    {
      std::cerr << "Missing parameters. " << std::endl;
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0]
                << " inputImageFile outputImageFile direction"
                << std::endl;
      return -1;
    }

  typedef float PixelType;
  typedef itk::Image< PixelType, 2 >  ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;

  typedef itk::ConstNeighborhoodIterator< ImageType > NeighborhoodIteratorType;
  typedef itk::ImageRegionIterator< ImageType>        IteratorType;

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
  
  ImageType::Pointer output = ImageType::New();
  output->SetRegions(reader->GetOutput()->GetRequestedRegion());
  output->Allocate();
  
  IteratorType out(output, reader->GetOutput()->GetRequestedRegion());
  
// Software Guide : BeginLatex
//
// \index{convolution!kernels}
// \index{convolution!operators}
// \index{iterators!neighborhood!and convolution}
//
// Refer to the previous example for a description of reading the input image and
// setting up the output image and iterator.
//
// The following code creates a Sobel operator.  Our Sobel operator requires a
// direction that corresponds to the derivative direcion.  This direction is
// read from the command line.  Changing the direction of the derivatives
// changes the bias of the edge detection, i.e. maximally vertical or maximally
// horizontal.  A complete Sobel edge detection routine such as
// \doxygen{SobelEdgeDetectionImageFilter} uses the square-root sum-of-squares
// of all of the partial derivatives.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  itk::SobelOperator<PixelType, 2> sobelOperator;
  sobelOperator.SetDirection( ::atoi(argv[3]) );
  sobelOperator.CreateDirectional();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The neighborhood iterator is initialized as before, except that now it takes
// its radius directly from the radius of the Sobel operator.  The inner
// product function object is templated over image type and requires no initialization.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::RadiusType radius = sobelOperator.GetRadius();
  NeighborhoodIteratorType it( radius, reader->GetOutput(), reader->GetOutput()->GetRequestedRegion() );
  
  itk::NeighborhoodInnerProduct<ImageType> innerProduct;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Using the Sobel operator, inner product, and neighborhood iterator objects,
// we can now write a very simple \code{for} loop for performing convolution
// filtering.  As before, out-of-bounds pixel values are supplied automatically
// by the iterator.
//
// Software Guide : EndLatex
    
// Software Guide : BeginCodeSnippet
  for (it.GoToBegin(), out.GoToBegin(); !it.IsAtEnd(); ++it, ++out)
    {
    out.Set( innerProduct( it, sobelOperator ) );
    }
  
// Software Guide : EndCodeSnippet
  
// Software Guide : BeginLatex
//
// The output is rescaled and written as in the previous example.  Filtering in
// the $x$-direction gives the same results as in figure~\ref{fig:NeighborhoodExample1}a.
// The results from filtering in the $y$ direction are given by
// figure~\ref{fig:NeighborhoodExample1}b.
// 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef unsigned char WritePixelType;
  typedef itk::Image< WritePixelType, 2 > WriteImageType;
  typedef itk::ImageFileWriter< WriteImageType > WriterType;
  
  typedef itk::RescaleIntensityImageFilter< 
               ImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput(output);
  
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput(rescaler->GetOutput());
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
// Software Guide : EndCodeSnippet

  return 0;
}
