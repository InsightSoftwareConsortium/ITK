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

#include "itkNiftiImageIOTest.h"
#include "itkTestingHashImageFilter.h"
#include "itkTestingMacros.h"

int itkNiftiImageIOTest12(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
  }
  if(ac != 2)
    return EXIT_FAILURE;


  char *arg1 = av[1];

  // make large RGB Image

  typedef itk::VectorImage<unsigned char, 3 > ImageType;

  ImageType::RegionType region;
  ImageType::SizeType size = {{2024, 1024, 1024}};
  region.SetSize( size );

  ImageType::Pointer image = ImageType::New();
  image->SetRegions( region );
  image->SetNumberOfComponentsPerPixel(3);
  image->Allocate();

  { //Fill in entire image

  ImageType::PixelType value(3);

  itk::ImageRegionIterator<ImageType> ri(image,region);
  while(!ri.IsAtEnd())
    {
    ImageType::IndexType idx = ri.GetIndex();

    value[0] = idx[0]%256;
    value[1] = idx[1]%256;
    value[2] = idx[2]%256;

    ri.Set( value );
    ++ri;
    }

  }

  typedef itk::Testing::HashImageFilter<ImageType> Hasher;
  Hasher::Pointer hasher = Hasher::New();
  hasher->SetInput( image );
  hasher->InPlaceOff();
  hasher->Update();

  std::string originalHash = hasher->GetHash();

  std::cout << "Original image hash: " << originalHash << std::endl;

  try
    {
    itk::IOTestHelper::WriteImage<ImageType,itk::NiftiImageIO>(image,arg1);

    image = itk::IOTestHelper::ReadImage<ImageType>( arg1 );
    hasher->SetInput( image );
    hasher->Update();

    std::string readHash = hasher->GetHash();
    std::cout << "Read hash: " << readHash << std::endl;

    TEST_EXPECT_EQUAL( originalHash, readHash );

    }
  catch (itk::ExceptionObject &)
    {
    }

  return EXIT_SUCCESS;

}
