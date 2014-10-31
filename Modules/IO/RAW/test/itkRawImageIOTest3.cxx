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

#include <fstream>
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkRawImageIO.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkRawImageIOTest3(int argc, char*argv[])
{
  typedef itk::Image<unsigned short,2>    ImageType;
  typedef ImageType::PixelType            PixelType;

  typedef itk::ImageRegionIterator<
                                  ImageType > ImageIteratorType;

  typedef itk::ImageRegionConstIterator<
                                  ImageType > ImageConstIteratorType;

  typedef itk::RawImageIO<PixelType,
                          ImageType::ImageDimension> RawImageIOType;


  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " Output1 Output2\n";
    return EXIT_FAILURE;
    }

  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  ImageType::SizeType size;
  size[0]=517;  // prime numbers are good bug testers...
  size[1]=293;

  ImageType::RegionType region;
  ImageType::IndexType  index;
  index.Fill(0);

  region.SetIndex( index );
  region.SetSize(size);

  ImageType::Pointer image = ImageType::New();
  image->SetRegions( region );
  image->Allocate();

  ImageIteratorType ii( image, region );

  PixelType value = itk::NumericTraits< PixelType >::ZeroValue();
  ii.GoToBegin();
  while( !ii.IsAtEnd() )
    {
    ii.Set( value );
    ++value;
    ++ii;
    }

  RawImageIOType::Pointer io = RawImageIOType::New();
  io->SetByteOrderToBigEndian();

  // Write out the image
  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput( image );
  writer->SetFileName(argv[1]);
  writer->SetImageIO(io);
  writer->Write();

  // Create a source object (in this case a reader)
  itk::ImageFileReader<ImageType>::Pointer reader;
  reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(io);
  reader->SetFileName(argv[1]);
  reader->Update();

  // Compare pixel by pixel in memory


  ImageConstIteratorType it( reader->GetOutput(),
                             reader->GetOutput()->GetBufferedRegion() );

  ImageConstIteratorType ot( image,
                             image->GetBufferedRegion() );

  it.GoToBegin();
  ot.GoToBegin();
  while( !it.IsAtEnd() )
    {
    const PixelType iv = it.Get();
    const PixelType ov = ot.Get();
    if( iv != ov )
      {
      std::cerr << "Error in read/write of pixel " << it.GetIndex() << std::endl;
      std::cerr << "Read value  is : " << iv << std::endl;
      std::cerr << "it should be   : " << ov << std::endl;
      std::cerr << "Test FAILED ! " << std::endl;
      return EXIT_FAILURE;
      }
    ++it;
    ++ot;
    }

  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);
  writer->SetInput(reader->GetOutput());
  writer->Write();

  std::cerr << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
