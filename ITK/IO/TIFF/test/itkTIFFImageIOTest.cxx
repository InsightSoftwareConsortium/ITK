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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include <fstream>
#include "itkTIFFImageIO.h"

#if defined(ITK_USE_MODULAR_BUILD)
  #define SPECIFIC_IMAGEIO_MODULE_TEST
#endif

template<class T> int DoIt( int, char * argv[], typename T::Pointer)
{
  typename itk::ImageFileReader<T>::Pointer reader
    = itk::ImageFileReader<T>::New();

  typename itk::ImageFileWriter<T>::Pointer writer
    = itk::ImageFileWriter<T>::New();

  itk::TIFFImageIO::Pointer io = itk::TIFFImageIO::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }


  typename T::Pointer image = reader->GetOutput();
  image->Print(std::cout );

  typename T::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  // Generate test image
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName(argv[2]);
  writer->SetImageIO(io);
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}

int itkTIFFImageIOTest( int argc, char* argv[] )
{

  unsigned int dimension = 2;
  unsigned int pixelType = 1;

  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0]
              << " Input Output [dimensionality:default 2]"
              << "[pixeltype: 1:uchar(default) 2:ushort 3: short]\n";
    return EXIT_FAILURE;
    }
  else if (argc == 4)
    {
    dimension = atoi(argv[3]);
    }
  else if (argc == 5)
    {
    dimension = atoi(argv[3]);
    pixelType = atoi(argv[4]);
    }

  if (dimension == 2 && pixelType == 1)
    {
    typedef itk::RGBPixel<unsigned char> PixelType;
    itk::Image<PixelType, 2>::Pointer dummy;
    return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy);

    }
  else if (dimension == 2 && pixelType == 2)
    {
    typedef itk::RGBPixel<unsigned short> PixelType;
    itk::Image<PixelType, 2>::Pointer dummy;
    return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy);
    }
  else if (dimension == 2 && pixelType == 3)
    {
    typedef itk::RGBPixel<short> PixelType;
    itk::Image<PixelType, 2>::Pointer dummy;
    return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy);
    }
  else if (dimension == 3 && pixelType == 1)
    {
    itk::Image<unsigned char, 3>::Pointer dummy;
    return DoIt<itk::Image<unsigned char, 3> >( argc, argv, dummy);
    }
  else if (dimension == 3 && pixelType == 2)
    {
    itk::Image<unsigned short, 3>::Pointer dummy;
    return DoIt<itk::Image<unsigned short, 3> >( argc, argv, dummy);
    }
  else if (dimension == 3 && pixelType == 3)
    {
    itk::Image<short, 3>::Pointer dummy;
    return DoIt<itk::Image<short, 3> >( argc, argv, dummy);
    }
  else if (dimension == 4 && pixelType == 1)
    {
    itk::Image<unsigned char, 4>::Pointer dummy;
    return DoIt<itk::Image<unsigned char, 4> >( argc, argv, dummy);
    }
  else if (dimension == 4 && pixelType == 2)
    {
    itk::Image<unsigned short, 4>::Pointer dummy;
    return DoIt<itk::Image<unsigned short, 4> >( argc, argv, dummy);
    }
  else if (dimension == 4 && pixelType == 3)
    {
    itk::Image<short, 4>::Pointer dummy;
    return DoIt<itk::Image<short, 4> >( argc, argv, dummy);
    }
  else
    {
    std::cerr << "Usage: " << argv[0]
              << " Input Output [dimensionality:default 2]"
              << "[pixeltype: 1:uchar(default) 2:ushort 3: short]\n";
    return EXIT_FAILURE;
    }
}
