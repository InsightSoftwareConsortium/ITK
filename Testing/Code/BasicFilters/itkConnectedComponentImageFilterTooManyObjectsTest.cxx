/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedComponentImageFilterTooManyObjectsTest.cxx
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
#include "itkConnectedComponentImageFilter.h"
#include "itkSimpleFilterWatcher.h"

int itkConnectedComponentImageFilterTooManyObjectsTest(int itkNotUsed(argc), char*[] itkNotUsed(argv))
{

  typedef   unsigned char  PixelType;
  const     unsigned int   Dimension = 2;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  // create a test input image with more objects in it than what the output type
  // can handle - 255
  ImageType::Pointer img = ImageType::New();
  ImageType::SizeType size;
  size.Fill( 512 );
  img->SetRegions( size );
  img->Allocate();
  img->FillBuffer( 0 );
  for( int x=0; x<512; x+=2 )
    {
    ImageType::IndexType idx;
    idx[0] = x;
    for( int y=0; y<512; y+=2 )
      {
      idx[1] = y;
      img->SetPixel( idx, 255 );
      }
    }
  
  typedef itk::ConnectedComponentImageFilter< ImageType, ImageType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( img );
  itk::SimpleFilterWatcher watcher(filter);
    
  try
    {
    filter->Update();
    // no exception - that's not normal
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "exception caught:" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_SUCCESS;
    }

  return EXIT_FAILURE;
}
