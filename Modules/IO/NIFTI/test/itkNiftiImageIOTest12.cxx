/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

int
itkNiftiImageIOTest12(int argc, char * argv[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if (argc != 3)
  {
    std::cerr << "Incorrect command line usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " <TempOutputDirectory> <filename>" << std::endl;
    return EXIT_FAILURE;
  }
  constexpr unsigned int CmdLineTestDirPos = 1;
  const std::string      testdir{ argv[CmdLineTestDirPos] };
  itksys::SystemTools::ChangeDirectory(testdir);
  constexpr unsigned int CmdLineFilenamePos = 2;
  const std::string      imgfilename{ argv[CmdLineFilenamePos] };

  using ImageType = itk::VectorImage<unsigned char, 3>;

  ImageType::RegionType region;

  // make a large RGB Image
  // test is too slow with large image ImageType::SizeType   size = { { 2024, 1024, 1024 } };

  // make small RGB Image
  ImageType::SizeType size = { { 24, 10, 11 } };
  region.SetSize(size);
#if 0 // using non-zero start index exposes bug in ITK IO physical space preservation
  ImageType::IndexType startIndex = { { 200, 300, 400 } };
#else
  ImageType::IndexType startIndex = { { 0, 0, 0 } };
#endif
  region.SetIndex(startIndex);

  auto image = ImageType::New();
  image->SetRegions(region);
  image->SetNumberOfComponentsPerPixel(3);
  image->SetOrigin(itk::MakePoint(-7.0, -13.0, -19.0));
  image->Allocate();

  { // Fill in entire image
    ImageType::PixelType value(3);

    itk::ImageRegionIterator<ImageType> ri(image, region);
    while (!ri.IsAtEnd())
    {
      ImageType::IndexType idx = ri.GetIndex();

      value[0] = idx[0] % 256;
      value[1] = idx[1] % 256;
      value[2] = idx[2] % 256;

      ri.Set(value);
      ++ri;
    }
  }

  using Hasher = itk::Testing::HashImageFilter<ImageType>;

  auto myHasher = [&](ImageType::Pointer imptr) -> std::string {
    auto originalHasher = Hasher::New();
    originalHasher->InPlaceOff();
    originalHasher->SetInput(imptr);
    originalHasher->Update();
    return originalHasher->GetHash();
  };

  const std::string originalHash = myHasher(image);
  std::cout << "Original image hash: " << originalHash << std::endl;

  try
  {
    itk::IOTestHelper::WriteImage<ImageType, itk::NiftiImageIO>(image, imgfilename);

    ImageType::Pointer readImage = itk::IOTestHelper::ReadImage<ImageType>(imgfilename);

    const std::string readHash = myHasher(readImage);
    std::cout << "Read hash: " << readHash << std::endl;

    ITK_TEST_EXPECT_EQUAL(originalHash, readHash);

    ImageType::IndexType threeIndex = { { 3, 3, 3 } };
    ImageType::PointType origPhysLocationIndexThree;
    image->TransformIndexToPhysicalPoint<double>(threeIndex, origPhysLocationIndexThree);
    ImageType::PointType readPhysLocationIndexThree;
    readImage->TransformIndexToPhysicalPoint<double>(threeIndex, readPhysLocationIndexThree);

    // If the origins, and the spacings, and the direction cosines are the smae,
    // then index locations should all represent the same physical locations as well.
    if (origPhysLocationIndexThree.EuclideanDistanceTo(readPhysLocationIndexThree) > 1e-5)
    {
      std::cerr << "ERROR: Physical space not preserved in nifti writer" << std::endl;
      std::cerr << origPhysLocationIndexThree << " != " << readPhysLocationIndexThree << std::endl;
      return EXIT_FAILURE;
    }
    else
    {
      std::cout << "Index location [3,3,3] represents same physical space:" << std::endl;
      std::cerr << origPhysLocationIndexThree << " != " << readPhysLocationIndexThree << std::endl;
    }
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception occurred: " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
