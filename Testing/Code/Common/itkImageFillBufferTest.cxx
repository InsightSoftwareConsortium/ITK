/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFillBufferTest.cxx
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

#include <iostream>
#include "itkImage.h"

int itkImageFillBufferTest(int argc, char * argv[])
{
  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " imageSize (GB). It can be a decimal value.";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned char,3> ImageType;
  ImageType::Pointer image = ImageType::New();

  ImageType::SizeType size;
  size[0] = (ImageType::SizeValueType) (atof(argv[1])*1024);
  size[1] = 1024;
  size[2] = 1024;
 
  // // do the math with a very large type to be sure to be able to store very
  // // large numbers on 32 bit systems
  // vxl_uint_64 total = ((vxl_uint_64)size[0]) * ((vxl_uint_64)size[1]) * ((vxl_uint_64)size[2]);
  // std::cout << "Expected memory usage is: " << total << std::endl;
  // if( total > itk::NumericTraits<ImageType::SizeValueType>::max() )
  //   {
  // // we should get an error on allocation
  //  try
  //    {
  //    image->SetRegions( size );
  //    }
  //   catch(itk::ExceptionObject e)
  //     {
  //     std::cout << e << std::endl;
  //     std::cout << "Can't allocate memory - that's nice. Don't go further." << std::endl;
  //     return (EXIT_SUCCESS);
  //     }
  //   return (EXIT_FAILURE); 
  //   }
  // else
  //   {
  //   image->SetRegions( size );
  //   }

  image->SetRegions( size );
  image->Allocate();
  image->FillBuffer( 128 );
  image->Print(std::cout);

  // try to access a value in the image
  ImageType::IndexType idx;
  idx[0] = 100;
  idx[1] = 100;
  idx[2] = 100;
  std::cout << "ComputeOffset(): " << image->ComputeOffset(idx) << std::endl;
  // we may have a segfault here on 32 bit systems if 4 GB is requested and 0 effectively allocated
  if( image->GetPixel( idx ) != 128 )
    {
    std::cerr << "Value is not 128!" << std::endl;
    return (EXIT_FAILURE);  
    }

  return (EXIT_SUCCESS);
}
