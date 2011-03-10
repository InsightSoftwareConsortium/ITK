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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkImageRegionIterator.h"
#include "itkTimeProbesCollectorBase.h"

int itkLargeImageWriteConvertReadTest(int ac, char* av[])
{

  if (ac < 3)
    {
    std::cout << "usage: itkIOTests itkLargeImageWriteConvertReadTest outputFileName numberOfPixelsInOneDimension" << std::endl;
    return EXIT_FAILURE;
    }
  typedef unsigned char OutputPixelType;
  typedef itk::Image<OutputPixelType,2> OutputImageType;
  typedef itk::Image<unsigned short,2> InputImageType;

  typedef itk::ImageFileWriter< OutputImageType >   WriterType;
  typedef itk::ImageFileReader< InputImageType >   ReaderType;


  itk::TimeProbesCollectorBase chronometer;

  { // begin write block
  OutputImageType::Pointer image = OutputImageType::New();
  OutputImageType::RegionType region;
  OutputImageType::IndexType index;
  OutputImageType::SizeType size;


  const size_t numberOfPixelsInOneDimension = atol( av[2] );

  size.Fill( numberOfPixelsInOneDimension );
  index.Fill(0);
  region.SetSize(size);
  region.SetIndex(index);

  image->SetRegions(region);

  chronometer.Start("Allocate");
  image->Allocate();
  chronometer.Stop("Allocate");

  std::cout << "Initializing pixel values " << std::endl;
  typedef itk::ImageRegionIterator< OutputImageType >  IteratorType;

  IteratorType itr( image, region );
  itr.GoToBegin();

  OutputPixelType pixelValue = itk::NumericTraits< OutputPixelType >::Zero;

  chronometer.Start("Initializing");
  while( !itr.IsAtEnd() )
    {
    itr.Set( pixelValue );
    ++pixelValue;
    ++itr;
    }
  chronometer.Stop("Initializing");

  std::cout << "Trying to write the image to disk" << std::endl;
  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(image);
    writer->SetFileName(av[1]);
    chronometer.Start("Write");
    writer->Update();
    chronometer.Stop("Write");
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }

  } // end writing block so data is freed

  std::cout << "Trying to read the image back from disk" << std::endl;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(av[1]);

  try
    {
    chronometer.Start("Read");
    reader->Update();
    chronometer.Stop("Read");
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }

  InputImageType::ConstPointer readImage = reader->GetOutput();

#if 0
  ConstIteratorType ritr( readImage, region );
  IteratorType oitr( image, region );

  ritr.GoToBegin();
  oitr.GoToBegin();

  std::cout << "Comparing the pixel values.. :" << std::endl;

  pixelValue = itk::NumericTraits< PixelType >::Zero;

  chronometer.Start("Compare");
  while( !ritr.IsAtEnd() )
    {
    if( ( oitr.Get() != ritr.Get() ) || ( oitr.Get() != pixelValue ) )
      {
      std::cerr << "Pixel comparison failed at index = " << oitr.GetIndex() << std::endl;
      std::cerr << "Expected pixel value " << pixelValue << std::endl;
      std::cerr << "Original Image pixel value " << oitr.Get() << std::endl;
      std::cerr << "Read Image pixel value " << ritr.Get() << std::endl;
      return EXIT_FAILURE;
      }

    ++pixelValue;
    ++oitr;
    ++ritr;
    }
  chronometer.Stop("Compare");
#endif
  chronometer.Report( std::cout );

  std::cout << std::endl;
  std::cout << "Test PASSED !" << std::endl;

  return EXIT_SUCCESS;

}
