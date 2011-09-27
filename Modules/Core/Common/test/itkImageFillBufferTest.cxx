/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

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
