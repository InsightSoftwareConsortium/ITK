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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMetaDataObject.h"
#include "itkMetaImageIO.h"
#include "itkTestingHashImageFilter.h"
#include "itkTestingMacros.h"

#define SPECIFIC_IMAGEIO_MODULE_TEST

namespace
{

int TestUnknowMetaDataBug( const std::string &fname )
{
  std::cout << "Testing for unknow meta data entry bug." << std::endl;

  try
    {
    typedef unsigned short           PixelType;
    typedef itk::Image<PixelType, 2> ImageType;

    ImageType::RegionType region;
    ImageType::SizeType size = {{32,32}};
    region.SetSize( size );

    ImageType::Pointer image = ImageType::New();
    image->SetRegions( region );
    image->Allocate();
    image->FillBuffer( 0 );

    itk::MetaDataDictionary &dict = image->GetMetaDataDictionary();

    itk::EncapsulateMetaData<float>(dict,"ASimpleFloatInitalized",static_cast<float>(1.234560F));
    itk::EncapsulateMetaData<std::complex<float> >(dict,"AnUnsuportedComplexInitalized",std::complex<float>(1.234560F));

    typedef itk::Testing::HashImageFilter<ImageType> Hasher;
    Hasher::Pointer hasher = Hasher::New();
    hasher->SetInput( image );
    hasher->InPlaceOff();
    hasher->Update();

    std::string originalHash = hasher->GetHash();
    std::cout << "\tOriginal image hash: " << originalHash << std::endl;


    // Write image out
    itk::ImageFileWriter<ImageType>::Pointer writer;
    writer = itk::ImageFileWriter<ImageType>::New();
    writer->SetInput( image );
    writer->SetFileName( fname );
    writer->Update();

    itk::ImageFileReader<ImageType>::Pointer reader;
    reader = itk::ImageFileReader<ImageType>::New();
    reader->SetFileName( fname );

    hasher->SetInput( reader->GetOutput() );
    hasher->Update();

    std::string readHash = hasher->GetHash();
    std::cout << "\tRead hash: " << readHash << std::endl;

    TEST_EXPECT_EQUAL( originalHash, readHash );
    }
  catch ( std::exception &e )
    {
    std::cerr << "Exception: " << e.what() << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}

}

int itkMetaImageIOTest2(int argc, char* argv[])
{
  if(argc < 2)
    {
    std::cerr << "Usage: " << argv[0] << " Output\n";
    return EXIT_FAILURE;
    }

  const bool pass = ( TestUnknowMetaDataBug( argv[1] ) == EXIT_SUCCESS );
  return (pass ? EXIT_SUCCESS : EXIT_FAILURE);
}
