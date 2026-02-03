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
#include "itkArray.h"
#include "itkHDF5ImageIO.h"
#include "itkHDF5ImageIOFactory.h"
#include "itkIOTestHelper.h"
#include "itkMetaDataObject.h"
#include "itkMath.h"
#include "itkTestingMacros.h"
#include <type_traits>

// DUPLICATE CODE IN itkMetaDictionaryGTest.cxx
namespace
{
template <typename T>
void
AddMetaData(itk::MetaDataDictionary & metaDict, const std::string & key, const T & knownValue)
{
  itk::EncapsulateMetaData<T>(metaDict, key, knownValue);
}

template <typename T>
int
VerifyMetaData(const itk::MetaDataDictionary & metaDict, const std::string & key, const T & knownValue)
{
  int status = EXIT_SUCCESS;
  T   exposedValue{};
  if (!itk::ExposeMetaData<T>(metaDict, key, exposedValue))
  {
    std::cerr << "Failure ExposeMetaData '" << key << "'" << std::endl;
    status = EXIT_FAILURE;
  }
  if constexpr (std::is_floating_point_v<T>)
  {
    if (itk::Math::NotAlmostEquals(exposedValue, knownValue))
    {
      std::cerr << "Incorrect meta value read in for " << key << " '" << exposedValue << "' != '" << knownValue << "'"
                << std::endl;
      status = EXIT_FAILURE;
    }
  }
  else
  {
    if (exposedValue != knownValue)
    {
      std::cerr << "Incorrect meta value read in for " << key << " '" << exposedValue << "' != '" << knownValue << "'"
                << std::endl;
      status = EXIT_FAILURE;
    }
  }
  if (status == EXIT_FAILURE)
  {
    std::cerr << "========================================" << std::endl;
    metaDict.Print(std::cerr);
    std::cerr << "========================================" << std::endl;
  }
  return status;
}
} // namespace

template <typename TPixel>
int
HDF5ReadWriteTest(const char * fileName)
{
  std::cout << fileName << std::endl;
  int success(EXIT_SUCCESS);
  using ImageType = typename itk::Image<TPixel, 3>;
  typename ImageType::SpacingType spacing;
  typename ImageType::PointType   origin;
  for (unsigned int i = 0; i < 3; ++i)
  {
    spacing[i] = 1.0 + static_cast<double>(i);
    origin[i] = static_cast<double>(i) * 5.0;
  }
  constexpr typename ImageType::SizeType size{ 5, 5, 5 };
  typename ImageType::RegionType         imageRegion{ size };
  const typename ImageType::Pointer      im =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<ImageType>(imageRegion, spacing);

  itk::Matrix<itk::SpacePrecisionType> mat;
  mat.SetIdentity();
  // 30deg rotation
  mat[1][1] = mat[0][0] = 0.866025403784439;
  mat[0][1] = -0.5;
  mat[1][0] = 0.5;
  im->SetDirection(mat);

  // set origin
  im->SetOrigin(origin);
  //
  // add some unique metadata
  itk::MetaDataDictionary & metaDict(im->GetMetaDataDictionary());

  AddMetaData<bool>(metaDict, "TestBool", false);
  AddMetaData<char>(metaDict, "TestChar", 'c');
  AddMetaData<unsigned char>(metaDict, "TestUChar", 'u');
  AddMetaData<short>(metaDict, "TestShort", 1);
  AddMetaData<unsigned short>(metaDict, "TestUShort", 3);
  AddMetaData<int>(metaDict, "TestInt", 5);
  AddMetaData<unsigned int>(metaDict, "TestUInt", 7);
  AddMetaData<long>(metaDict, "TestLong", 5);
  AddMetaData<unsigned long>(metaDict, "TestULong", 7);
  AddMetaData<long long>(metaDict, "TestLLong", -5);
  AddMetaData<unsigned long long>(metaDict, "TestULLong", 7ull);
  AddMetaData<float>(metaDict, "TestFloat", 1.23456f);
  AddMetaData<double>(metaDict, "TestDouble", 1.23456);

  itk::Array<char> metaDataCharArray(5);
  metaDataCharArray[0] = 'h';
  metaDataCharArray[1] = 'e';
  metaDataCharArray[2] = 'l';
  metaDataCharArray[3] = 'l';
  metaDataCharArray[4] = 'o';
  AddMetaData<itk::Array<char>>(metaDict, "TestCharArray", metaDataCharArray);

  itk::Array<double> metaDataDoubleArray(5);
  metaDataDoubleArray[0] = 3.0;
  metaDataDoubleArray[1] = 1.0;
  metaDataDoubleArray[2] = 4.0;
  metaDataDoubleArray[3] = 5.0;
  metaDataDoubleArray[4] = 2.0;
  AddMetaData<itk::Array<double>>(metaDict, "TestDoubleArray", metaDataDoubleArray);

  AddMetaData<std::string>(metaDict, "StdString", "Test std::string");

  //
  // fill image buffer
  vnl_random                          randgen(12345678);
  itk::ImageRegionIterator<ImageType> it(im, im->GetLargestPossibleRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    TPixel pix;
    itk::IOTestHelper::RandomPix(randgen, pix);
    it.Set(pix);
  }
  typename ImageType::Pointer im2;
  try
  {
    itk::IOTestHelper::WriteImage<ImageType, itk::HDF5ImageIO>(im, std::string(fileName));
    im2 = itk::IOTestHelper::ReadImage<ImageType>(std::string(fileName));
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "itkHDF5ImageIOTest" << std::endl << "Exception Object caught: " << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  if (im->GetOrigin() != im2->GetOrigin())
  {
    std::cout << "Origin read " << im2->GetOrigin() << " doesn't match origin written" << im->GetOrigin() << std::endl;
    return EXIT_FAILURE;
  }
  if (im->GetSpacing() != im2->GetSpacing())
  {
    std::cout << "Spacing read " << im2->GetSpacing() << " doesn't match spacing written" << im->GetSpacing()
              << std::endl;
    return EXIT_FAILURE;
  }
  if (im->GetDirection() != im2->GetDirection())
  {
    std::cout << "Direction read " << im2->GetDirection() << " doesn't match direction written" << im->GetDirection()
              << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "Checking MetaDataDictionary Values:" << std::endl;
  //
  // Check MetaData
  const itk::MetaDataDictionary & metaDict2(im2->GetMetaDataDictionary());

  if (VerifyMetaData<bool>(metaDict2, "TestBool", false) != EXIT_SUCCESS)
  {
    metaDict2.Print(std::cerr);
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<char>(metaDict2, "TestChar", 'c') != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<unsigned char>(metaDict2, "TestUChar", 'u') != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<short>(metaDict2, "TestShort", 1) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<unsigned short>(metaDict2, "TestUShort", 3) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<int>(metaDict2, "TestInt", 5) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<unsigned int>(metaDict2, "TestUInt", 7) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<long>(metaDict2, "TestLong", 5) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<unsigned long>(metaDict2, "TestULong", 7) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<long long>(metaDict2, "TestLLong", -5) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<unsigned long long>(metaDict2, "TestULLong", 7ull) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<float>(metaDict2, "TestFloat", 1.23456f) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<double>(metaDict2, "TestDouble", 1.23456) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }

  if (VerifyMetaData<itk::Array<char>>(metaDict2, "TestCharArray", metaDataCharArray) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<itk::Array<double>>(metaDict2, "TestDoubleArray", metaDataDoubleArray) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }
  if (VerifyMetaData<std::string>(metaDict2, "StdString", std::string("Test std::string")) != EXIT_SUCCESS)
  {
    success = EXIT_FAILURE;
  }

  itk::ImageRegionIterator<ImageType> it2(im2, im2->GetLargestPossibleRegion());
  for (it.GoToBegin(), it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it, ++it2)
  {
    if (itk::Math::NotAlmostEquals(it.Get(), it2.Get()))
    {
      std::cout << "Original Pixel (" << it.Get() << ") doesn't match read-in Pixel (" << it2.Get() << std::endl;
      success = EXIT_FAILURE;
      break;
    }
  }

  return success;
}

int
HDF5ReuseReadWriteTest(const char * fileName)
{
  constexpr int success{ EXIT_SUCCESS };

  const itk::HDF5ImageIO::Pointer io = itk::HDF5ImageIO::New();
  io->SetFileName(fileName);

  // Is writing first an image should produce an error?
  // io->WriteImageInformation();

  io->ReadImageInformation();
  // Ensure there are no memory leaks
  io->ReadImageInformation();

  io->WriteImageInformation();
  // Ensure there are no memory leaks
  io->WriteImageInformation();

  // Reading after Writing shouldn't produce an error
  io->ReadImageInformation();

  return success;
}

int
itkHDF5ImageIOTest(int argc, char * argv[])
{
  std::string prefix("");
  if (argc > 1)
  {
    prefix = *++argv;
    --argc;
    itksys::SystemTools::ChangeDirectory(prefix);
  }
  itk::ObjectFactoryBase::RegisterFactory(itk::HDF5ImageIOFactory::New());

  const itk::HDF5ImageIO::Pointer imageio = itk::HDF5ImageIO::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(imageio, HDF5ImageIO, StreamingImageIOBase);

  int result(0);

  result += HDF5ReadWriteTest<unsigned char>("UCharImage.hdf5");
  result += HDF5ReadWriteTest<float>("FloatImage.hdf5");
  result += HDF5ReadWriteTest<unsigned long long>("ULongLongImage.hdf5");
  result += HDF5ReadWriteTest<itk::RGBPixel<unsigned char>>("RGBImage.hdf5");

  result += HDF5ReuseReadWriteTest("UCharImage.hdf5");

  return result != 0;
}
