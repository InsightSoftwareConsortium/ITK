/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMask2DImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPolylineMask2DImageFilter.h"
#include "itkPolyLineParametricPath.h"
#include "itkImageFileWriter.h"

int itkPolylineMask2DImageFilterTest(int argc, char * argv [] ) 
{

  if( argc < 2 )
    {
    std::cerr << "Error: missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " outputFilename " << std::endl;
    return EXIT_FAILURE;
    }

  // Declare the types of the images
  const unsigned int Dimension = 2;
  typedef unsigned char PixelType;

  typedef itk::Image<PixelType, Dimension>        InputImageType;
  typedef itk::Image<PixelType, Dimension>        OutputImageType;
  typedef itk::PolyLineParametricPath<Dimension>  InputPolylineType;

  // Declare the type of the index to access images
  typedef InputImageType::IndexType         InputIndexType;

  // Declare the type of the size 
  typedef InputImageType::SizeType          InputSizeType;

  // Declare the type of the Region
  typedef InputImageType::RegionType         InputRegionType;

  // Create images
  InputImageType::Pointer inputImage    = InputImageType::New();
 
  // Create polyline
  InputPolylineType::Pointer inputPolyline   = InputPolylineType::New();

  // Define their size, and start index
  InputSizeType size;
  size[0] = 512;
  size[1] = 512;

  InputIndexType start;
  start[0] = 0;
  start[1] = 0;

  InputRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize input image
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();
  inputImage->FillBuffer(0);

  InputImageType::SpacingType spacing; spacing.Fill(.1);
  inputImage->SetSpacing(spacing);

  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIteratorWithIndex<InputImageType>  inputIteratorType;

  // Create one iterator for Image A (this is a light object)
  inputIteratorType it( inputImage, inputImage->GetBufferedRegion() );
  it.GoToBegin();
  while( !it.IsAtEnd() ) 
    {
    /* fill in only the upper part of the image */
    if(it.GetIndex()[1] > 256)
      {
      it.Set( 255 );
      }
    ++it;
    }

  // Initialize the polyline 
  typedef InputPolylineType::VertexType VertexType;
    
  // Add vertices to the polyline
  VertexType v0;
  v0[0] = 12.8;
  v0[1] = 25.6;
  inputPolyline->AddVertex(v0);
  
  VertexType v1;
  v1[0] = 25.6;
  v1[1] = 39.4;
  inputPolyline->AddVertex(v1);
  
  VertexType v2;
  v2[0] = 39.4;
  v2[1] = 25.6;
  inputPolyline->AddVertex(v2);

  VertexType v3;
  v3[0] = 25.6;
  v3[1] = 12.8;
  inputPolyline->AddVertex(v3);
  
  // Close the polygon
  inputPolyline->AddVertex(v0);

  // Declare the type for the Mask image filter
  typedef itk::PolylineMask2DImageFilter<
                           InputImageType, InputPolylineType,   
                           OutputImageType  >     InputFilterType;
            

  // Create a mask  Filter                                
  InputFilterType::Pointer filter = InputFilterType::New();

  // Connect the input image
  filter->SetInput1    ( inputImage ); 
 
  // Connect the Polyline 
  filter->SetInput2    ( inputPolyline ); 

  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  std::cout << "Output filename = " << argv[1] << std::endl;

  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[1] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an unexpected exception. " << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }
  
  // Now cause and exception
  VertexType ve;
  ve[0] = 256.0;
  ve[1] = 12.8;
  inputPolyline->AddVertex(ve);
  
  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an expected exception. " << std::endl;
    std::cout << err << std::endl;
    return EXIT_SUCCESS;
    }
  return EXIT_FAILURE;

}

