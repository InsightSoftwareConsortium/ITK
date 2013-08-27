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
#include "itkBMPImageIO.h"
#include "itkRGBPixel.h"
#include "itkImageFileReader.h"
#include "itkImageRegionConstIterator.h"
#include <fstream>


#define SPECIFIC_IMAGEIO_MODULE_TEST

/* This test check that a lower-left bitmap and an upper-left bitmap
 * representing the same RGB image contains the same data.
 */
int itkBMPImageIOTest4( int ac, char* av[] )
{

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " LowerLeftImage UpperLeftImage" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::RGBPixel <unsigned char >             PixelType;
  typedef itk::Image<PixelType, 2>                   ImageType;
  typedef itk::ImageFileReader<ImageType>            ReaderType;
  typedef itk::ImageRegionConstIterator< ImageType > IteratorType;


  ReaderType::Pointer reader1 = ReaderType::New();
  itk::BMPImageIO::Pointer io1 = itk::BMPImageIO::New();
  reader1->SetImageIO(io1);
  reader1->SetFileName(av[1]);

  ReaderType::Pointer reader2 = ReaderType::New();
  itk::BMPImageIO::Pointer io2 = itk::BMPImageIO::New();
  reader2->SetImageIO(io2);
  reader2->SetFileName(av[2]);


  try
    {
    reader1->Update();
    reader2->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in image file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }


  if(!io1->GetFileLowerLeft())
    {
      std::cout << "Expecting a lower-left bitmap, got an upper-left." << std::endl;
      return EXIT_FAILURE;
    }

  if(io2->GetFileLowerLeft())
    {
      std::cout << "Expecting an upper-left bitmap, got a lower-left." << std::endl;
      return EXIT_FAILURE;
    }


  ImageType::RegionType r1 = reader1->GetOutput()->GetLargestPossibleRegion(),
                        r2 = reader2->GetOutput()->GetLargestPossibleRegion();

  if(r1 != r2)
    {
      std::cout << "The images must have the same size." << std::endl;
      return EXIT_FAILURE;
    }

  IteratorType it1( reader1->GetOutput(), r1 );
  IteratorType it2( reader2->GetOutput(), r2 );

  it1.GoToBegin();
  it2.GoToBegin();
  while ( !it1.IsAtEnd() )
    {
    if ( !(it1.Value() == it2.Value()) )
      {
      std::cout << "An image stored in a lower-left bitmap is different than the same image stored in a upper-left bitmap." << std::endl;
      return EXIT_FAILURE;
      }

    ++it1;
    ++it2;
    }

  return EXIT_SUCCESS;
}
