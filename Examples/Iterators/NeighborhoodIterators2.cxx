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
// convolution filtering with the \code{itk::SobelOperator}.  Convolution is
// done by taking the inner product between the Sobel operator (convolution
// kernel) and the neighborhood around each image pixel, which is accessed
// through the image iterator.  The result at each pixel is written to an
// output image buffer as before.  Many basic image processing algorithms in
// ITK were implemented using the basic procedure illustrated here.
// \code{Itk::NeighborhoodOperatorImageFilter} is a generalization of
// this example to ND and arbitrary convolution kernels.
//
//.A few additional header files are necessary for this example.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSobelOperator.h"
#include "itkNeighborhoodInnerProduct.h"
// Software Guide : EndCodeSnippet

int main( int argc, char ** argv )
{
  // Verify the number of parameters on the command line.
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
  
  NeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborhoodIteratorType it( radius, reader->GetOutput(), reader->GetOutput()->GetRequestedRegion() );
  
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
// Refer to the previous example for the steps of reading the input image and
// setting up the input and output iterators.  In this example, instead of
// performing the finite-difference operations on the neighborhood explicitly,
// we will perform convolution of the neighborhood with the Sobel operator
// (kernel) at each point.  The first step is to create a Sobel operator.  The
// direction in which the derivatives are taken is read from the command line.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  itk::SobelOperator<PixelType, 2> sobelOperator;
  sobelOperator.SetDirection( ::atoi(argv[3]) );
  sobelOperator.CreateDirectional();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// There are many other ITK neighborhood operator objects that can be used for
// convolution filtering.  Note that there may be slight differences in the way
// each one is constructed and initialized and each may have a different set of
// parameters appropriate to its functionality.
//
// Now we create a function object to use for taking the inner products.  The inner
// product object, like the neighborhood iterators, is templated over an image type.
//
// Software Guide : EndLatex
  
// Software Guide : BeginCodeSnippet
  itk::NeighborhoodInnerProduct<ImageType> innerProduct;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Using the Sobel operator, inner product, and neighborhood iterator objects,
// we can now write a very simple \code{for} loop for performing convolution
// filtering.  As before, values outside the bounds of the image are supplied
// automatically according to the default boundary condition of the
// \code{NeighborhoodIterator} object--in this case a Neumann condition where
// the first derivative across the boundary is zero. 
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
// The output is rescaled and written as in the previous example.  Filter the
// BLAH BLAH image in the $X$ direction give the same result as in Figure~BLAH BLAH
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
