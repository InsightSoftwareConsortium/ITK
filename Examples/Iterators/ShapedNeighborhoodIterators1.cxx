/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ShapedNeighborhoodIterators1.cxx
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
#include "itkNeighborhoodAlgorithm.h"
#include <math.h>

// Software Guide : BeginLatex
//
// This example uses \doxygen{ShapedNeighborhoodIterator} to implement a binary
// erosion algorithm.  If we think of an image $I$ as a set of pixel indicies,
// then erosion of $I$ by a smaller set $E$, called the \emph{structuring
// element}, is the set of all indicies at locations $x$ in $I$ such that when
// $E$ is positioned at $x$, every element in $E$ is also contained in $I$.
//
// This type of algorithm is easy to implement with shaped neighborhood
// iterators because we can use the iterator itself as the structuring element
// $E$ and move it sequentially through all positions $x$.  The result at $x$
// is obtained by a simple iteration through the neighborhood stencil.
//
// We need two iterators, a shaped iterator for the input image and a regular
// image iterator for writing results to the output image. 
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
// Software Guide : EndCodeSnippet

int main( int argc, char ** argv )
{
  if ( argc < 4 )
    {
    std::cerr << "Missing parameters. " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << " inputImageFile outputImageFile element_radius"
              << std::endl;
    return -1;
    }

  typedef unsigned char PixelType;
  typedef itk::Image< PixelType, 2 >  ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;
  
  typedef itk::ConstShapedNeighborhoodIterator< ImageType > ShapedNeighborhoodIteratorType;
  typedef itk::ImageRegionIterator< ImageType> IteratorType;
  
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  unsigned int element_radius = ::atoi( argv[3] );
  
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

// Software Guide : BeginLatex
//
// Software Guide : EndLatex
  
// Software Guide : BeginCodeSnippet   
  typedef itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageType> FaceCalculatorType;
  
  FaceCalculatorType faceCalculator;
  FaceCalculatorType::FaceListType faceList;
  FaceCalculatorType::FaceListType::iterator fit;

  ShapedNeighborhoodIteratorType::RadiusType radius;
  radius.Fill(element_radius);
  
  faceList = faceCalculator(reader->GetOutput(), output->GetRequestedRegion(), radius);
// Software Guide : EndCodeSnippet 

// Software Guide : BeginLatex
//
// 
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  IteratorType out;
  ShapedNeighborhoodIteratorType it;
  const float rad = static_cast<float>(element_radius);

  const PixelType background_value = 0;
  const PixelType foreground_value = 255;
  
  for ( fit=faceList.begin(); fit != faceList.end(); ++fit)
    {
    it = ShapedNeighborhoodIteratorType( radius, reader->GetOutput(), *fit );
    out = IteratorType( output, *fit );

    // Creates a circular structuring element by activating all the pixels less
    // than radius distance from the center of the neighborhood.
    for (float y = -rad; y <= rad; y++)
      {
      for (float x = -rad; x <= rad; x++)
        {     
        ShapedNeighborhoodIteratorType::OffsetType off;
        
        float dis = ::sqrt( x*x + y*y );
        if (dis <= rad)
          {
          off[0] = static_cast<int>(x);
          off[1] = static_cast<int>(y);
          it.ActivateOffset(off);
          }

        // Deactivate the center element
        off[0] = off[1] = 0;
        it.DeactivateOffset(off);
        }
      }

    // Implements erosion
    for (it.GoToBegin(), out.GoToBegin(); !it.IsAtEnd(); ++it, ++out)
      {
      ShapedNeighborhoodIteratorType::ConstIterator ci;

      bool flag = true;
      for (ci = it.Begin(); ci != it.End(); ci++)
        {
        if (ci.Get() == background_value)
          {
          flag = false;
          }
        }
      if (flag == true)
        {
        out.Set(foreground_value);
        }
      else
        {
        out.Set(background_value);
        }
      }
    }
  
  //  ShapedNeighborhoodIteratorType::IndexType idx = {{100, 100}};
  //  it.SetLocation( idx );
  //  itk::ShapedNeighborhoodIterator<ImageType>::Iterator ci;

  //  for (ci = it.Begin(); ci != it.End(); ci++)
  //    {
  //    ci.Set(255);
  //    }
    
// Software Guide : EndCodeSnippet

  
// Software Guide : BeginLatex
//
// The output is rescaled and written as in the previous example.  Filter the
// BLAH BLAH image in the $X$ direction give the same result as in Figure~BLAH BLAH
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet

  typedef itk::ImageFileWriter< ImageType > WriterType;
  
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( output );
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
