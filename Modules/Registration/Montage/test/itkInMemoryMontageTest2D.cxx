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

#include "itkInMemoryMontageTestHelper.hxx"
#include "itkTileConfiguration.h"
#include "itkRGBPixel.h"

int
itkInMemoryMontageTest2D(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0] << " <directoryWithInputData> <montageTSV>";
    std::cerr << " [ variation streamSubdivisions ]" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned streamSubdivisions = 1;
  if (argc > 4)
  {
    streamSubdivisions = std::stoul(argv[4]);
  }

  std::string inputPath = argv[1];
  if (inputPath.back() != '/' && inputPath.back() != '\\')
  {
    inputPath += '/';
  }

  itk::TileConfiguration<2> stageTiles;
  stageTiles.Parse(inputPath + "TileConfiguration.registered.txt");

  itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(
    (inputPath + stageTiles.Tiles[0].FileName).c_str(), itk::IOFileModeEnum::ReadMode);
  imageIO->SetFileName(inputPath + stageTiles.Tiles[0].FileName);
  imageIO->ReadImageInformation();
  const itk::IOPixelEnum pixelType = imageIO->GetPixelType();

  std::string outFileName = std::string(argv[2]);

  if (pixelType == itk::IOPixelEnum::RGB)
  {
    using TestTransformType = InMemoryMontageTest<itk::RGBPixel<unsigned char>, itk::RGBPixel<unsigned int>>;

    TestTransformType::TestVariation variation = TestTransformType::TestVariation::UOrigin_USpacing_UTransform;
    if (argc > 3)
    {
      variation = static_cast<TestTransformType::TestVariation>(std::stoul(argv[3]));
    }

    TestTransformType::Pointer testObject = TestTransformType::New();
    return testObject->execute(stageTiles, inputPath, outFileName, variation, streamSubdivisions);
  }
  else
  {
    using TestTransformType = InMemoryMontageTest<unsigned short, double>;

    TestTransformType::TestVariation variation = TestTransformType::TestVariation::UOrigin_USpacing_UTransform;
    if (argc > 3)
    {
      variation = static_cast<TestTransformType::TestVariation>(std::stoul(argv[3]));
    }

    TestTransformType::Pointer testObject = TestTransformType::New();
    return testObject->execute(stageTiles, inputPath, outFileName, variation, streamSubdivisions);
  }
}
