/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    NeighborhoodIterators3.cxx
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
// 
//
// algorithm to the previous example by 
// The previous two examples rely on of
// the \code{itk::NeighborhoodIterator} objec not explicitly handled
// calculations at image boundaries
//
// The next example illustrates a technique for improving the efficiency of
// neighborhood calculations by eliminating unnecessary calculations at
// boundaries.  Correctly handling out-of-bounds pixels i computationally
// expensive procedure because it requires that the iterator be aware of its
// position in the image and compare it with the image boundaries at each
// calculation.  an out of bounds condition requires that the iterator supply
// an appropriate value.
//
// previous examples handle boundary conditions but are not optimally
// efficient.  image region structure of images gives us capability of only
// doing bounds conditions when required.  can break an ND image into separate
// regions.  the largest region is guaranteed to contain only in-bounds
// pixels.  other N-1 dimensional regions are guaranteed on the boundary.  turn
// off boundary checking in the first region and always do checking in other
// regions. 
//
// 
// Software Guide : EndLatex

#include "itkSobelOperator.h"
#include "itkNeighborhoodInnerProduct.h"

// Software Guide : BeginCodeSnippet
#include "itkNeighborhoodAlgorithm.h"
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
  
  itk::SobelOperator<PixelType, 2> sobelOperator;
  sobelOperator.SetDirection( ::atoi(argv[3]) );
  sobelOperator.CreateDirectional();

  itk::NeighborhoodInnerProduct<ImageType> innerProduct;
  
// Software Guide : BeginLatex
//
// after loading in the input image, we create the output as in previous
// examples.  the sobel operator and inner product function object is created
// as in the previous example.
//
// The next step is to analyze the image regions to determine where the image
// boundaries are.  the \code{ImageBoundaryFacesCalculator} object is designed
// for this purpose.  it takes as arguments an image pointer, an image region,
// and a neighborhood radius.  the pointer argument 
// 
// Software Guide : EndLatex
  
  // Break the input into a series of regions.  The first region is free
  // of boundary conditions, the rest with boundary conditions. Note,
  // we pass in the input image and the OUTPUT requested region. We are
  // only concerned with centering the neighborhood operator at the
  // pixels that correspond to output pixels. if the input and output regions
  // did not match, for example
  
  
// Software Guide : BeginCodeSnippet   
  typedef itk::NeighborhoodAlgorithm
    ::ImageBoundaryFacesCalculator< ImageType > FaceCalculatorType;
  
  FaceCalculatorType faceCalculator;
  FaceCalculatorType::FaceListType faceList;
  FaceCalculatorType::FaceListType::iterator fit;
  
  faceList = faceCalculator(reader->GetOutput(), output->GetRequestedRegion(),
                            sobelOperator.GetRadius());
// Software Guide : EndCodeSnippet 

// Software Guide : BeginLatex
//
// 
//
// Software Guide : EndLatex

  
// Software Guide : BeginCodeSnippet
  IteratorType out;
  NeighborhoodIteratorType it;
  
  for ( fit=faceList.begin(); fit != faceList.end(); ++fit)
    {
    it = NeighborhoodIteratorType( sobelOperator.GetRadius(),
                                  reader->GetOutput(), *fit );
    out = IteratorType( output, *fit );
    
    for (it.GoToBegin(), out.GoToBegin(); ! it.IsAtEnd(); ++it, ++out)
      {
      out.Set( innerProduct(it, sobelOperator) );
      }
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
