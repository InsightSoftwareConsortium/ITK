/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    NeighborhoodIterators5.cxx
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
#include "itkNeighborhoodAlgorithm.h"
#include "itkGaussianOperator.h"
#include "itkNeighborhoodInnerProduct.h"

// Software Guide : BeginLatex
//
// This example introduces slice-based neighborhood processing.  An
// \code{std::slice} is a simple object that defines a starting position, a
// step size, and an ending position.  Using slices, we can walk through a
// neighborhood in a variety of ways.  For example, suppose we wanted to take
// derivatives along the $y$-axis, but offset those derivatives by one pixel
// along the positive $x$-axis.  For a $3x3$, 2D neighborhood iterator, we can
// construct a slice as \code{(start = 2, stride = 3, end = 8)}, where
// \code{start} and \code{end} are given as neighborhood array positions.  This
// slice can be passed along with an appropriate 1D
// \doxygen{DerviativeOperator} to the \doxygen{NeighborhoodInnerProduct}
// function, which will then use only the pixels specified by the slice,
// i.e. those pixels at neighborhood offsets $(1, -1)$, $(1, 0)$, $(1, 1)$ (see
// figure~\ref{????????????????????????}) for reference.
//
// The previous separable Gaussian filtering example can be rewritten using
// slices and slice-based inner products.  In general, slice-based processing
// is most useful when doing many different calculations on the same
// neighborhood, so that defining multiple iterators as in 
// section~\ref{?????????????????} becomes impractical or inefficient.  Good
// examples of slice-based neighborhood processing are any of the ND
// anisotropic diffusion function objects, such as
// \doxygen{CurvatureNDAnisotropicDiffusionFunction}.
//
// Software Guide : EndLatex

int main( int argc, char ** argv )
{
  if ( argc < 4 )
    {
    std::cerr << "Missing parameters. " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << " inputImageFile outputImageFile sigma"
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
  
  itk::NeighborhoodInnerProduct<ImageType> innerProduct;
   
  typedef itk::NeighborhoodAlgorithm
    ::ImageBoundaryFacesCalculator< ImageType > FaceCalculatorType;
  
  FaceCalculatorType faceCalculator;
  FaceCalculatorType::FaceListType faceList;
  FaceCalculatorType::FaceListType::iterator fit;

  IteratorType out;
  NeighborhoodIteratorType it;

// Software Guide: BeginLatex
//
// The first difference between this example and the previous example is that
// the Gaussian operator is only initialized once.  Its direction is not
// important because it is only a 1D array of coefficients.
//
// Software Guide: EndLatex

// Software Guide : BeginCodeSnippet
  itk::GaussianOperator< PixelType, 2 > gaussianOperator;
  gaussianOperator.SetDirection(0);
  gaussianOperator.SetVariance( ::atof(argv[3]) * ::atof(argv[3]) );
  gaussianOperator.CreateDirectional();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we need to define a radius for the iterator.  The radius in all
// directions matches that of the single extent of the Gaussian operator,
// defining a square neighborhood.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( gaussianOperator.GetRadius()[0] );
// Software Guide EndCodeSnippet

// Software Guide : BeginLatex
//
// Now we have arrived at the main loop.  The inner product and face calculator are
// defined as before, but now the iterator is reinitialized each iteration with the
// square \code{radius} instead of the radius of the operator, and the inner
// product is taken using a slice along the axial direction corresponding to
// the current iteration.  Note the use of \code{GetSlice} to return the proper
// slice from the iterator itself.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType::Pointer input = reader->GetOutput();

  faceList = faceCalculator(input, output->GetRequestedRegion(),
                            radius);
   
  for (unsigned int i = 0; i < ImageType::ImageDimension; ++i)
    {
    for ( fit=faceList.begin(); fit != faceList.end(); ++fit )
      {
      it = NeighborhoodIteratorType( radius, input, *fit );

      out = IteratorType( output, *fit );
      
      for (it.GoToBegin(), out.GoToBegin(); ! it.IsAtEnd(); ++it, ++out)
        {
        out.Set( innerProduct(it.GetSlice(i), it, gaussianOperator) );
        }
      }
    
    // Swap the input and output buffers
    if (i != ImageType::ImageDimension - 1)
      {
      ImageType::Pointer tmp = input;
      input = output;
      output = tmp;
      }
    }
// Software Guide : EndCodeSnippet

  
// Software Guide : BeginLatex
//
// This technique produces exactly the same results as the previous example.  A
// little experimentation, however, will reveal that it is less efficient since
// the neighborhood iterator is keeping track of extra, unused pixel locations
// for each iteration, while the previous example only references those pixels
// that it needs.  In cases, however, where an algorithm takes multiple
// derivatives or convolution products over the same neighborhood, slice-based
// processing can increase efficiency and simplify the implementation.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet

  typedef unsigned char WritePixelType;
  typedef itk::Image< WritePixelType, 2 > WriteImageType;
  typedef itk::ImageFileWriter< WriteImageType > WriterType;
  
  typedef itk::RescaleIntensityImageFilter< ImageType,
    WriteImageType > RescaleFilterType;
  
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput(output);
  
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );
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
