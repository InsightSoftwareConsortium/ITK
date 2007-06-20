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

#include <itkImage.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkPolylineMask2DImageFilter.h>
#include <itkPolyLineParametricPath.h>
#include <itkImageFileWriter.h>

int itkPolylineMask2DImageFilterTest(int , char* [] ) 
{

 // Declare the types of the images
  typedef itk::Image<unsigned char, 2>     inputImageType;
  typedef itk::Image<unsigned char, 2>     outputImageType;
  typedef itk::PolyLineParametricPath<2>     inputPolylineType;

  // Declare the type of the index to access images
  typedef inputImageType::IndexType         inputIndexType;

  // Declare the type of the size 
  typedef inputImageType::SizeType          inputSizeType;

  // Declare the type of the Region
  typedef inputImageType::RegionType         inputRegionType;

  // Create images
  inputImageType::Pointer inputImage    = inputImageType::New();
 
  // Create polyline
  inputPolylineType::Pointer inputPolyline   = inputPolylineType::New();

  // Define their size, and start index
  inputSizeType size;
  size[0] = 512;
  size[1] = 512;

  inputIndexType start;
  start[0] = 0;
  start[1] = 0;

  inputRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize input image
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();
  inputImage->FillBuffer(0);

  inputImageType::SpacingType spacing; spacing.Fill(.1);
  inputImage->SetSpacing(spacing);

  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIteratorWithIndex<inputImageType>  inputIteratorType;

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
  typedef inputPolylineType::VertexType VertexType;
    
  // Add vertices to the polyline
  VertexType v;
  v[0] = 12.8;
  v[1] = 25.6;
  inputPolyline->AddVertex(v);
  
  v[0] = 25.6;
  v[1] = 39.4;
  inputPolyline->AddVertex(v);
  
  v[0] = 39.4;
  v[1] = 25.6;
  inputPolyline->AddVertex(v);

  v[0] = 25.6;
  v[1] = 12.8;
  inputPolyline->AddVertex(v);
  
  // Declare the type for the Mask image filter
  typedef itk::PolylineMask2DImageFilter<
                           inputImageType, inputPolylineType,   
                           outputImageType  >     inputFilterType;
            

  // Create a mask  Filter                                
  inputFilterType::Pointer filter = inputFilterType::New();

  // Connect the input image
  filter->SetInput1    ( inputImage ); 
 
  // Connect the Polyline 
  filter->SetInput2    ( inputPolyline ); 
  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an unexpected exception. " << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }
  
  // Now cause and exception
  v[0] = 256.0;
  v[1] = 12.8;
  inputPolyline->AddVertex(v);
  
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




