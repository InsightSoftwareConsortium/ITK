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

#include "itkMetaImageIO.h"
#include "itkTestingMacros.h"
#include "itkImage.h"
#include "itkVectorImage.h"

#include "itkImageIOFactory.h" // required to instantiate an instance of ImageIOBase

// Specific ImageIO test

// Macro to check that two arrays have the same size at compile time. It doesn't compile if they don't
// as it tries to create an array of size(-1)
// https://scaryreasoner.wordpress.com/2009/02/28/checking-sizeof-at-compile-time/
#define CHECK_ARRAYS_HAVE_SAME_SIZE_AT_COMPILE_TIME(array1, array2) \
  ((void)sizeof(char[1 - 2 * !!(sizeof(array1) / sizeof(*array1) - sizeof(array2) / sizeof(*array2))]))

int
itkImageIOBaseTest(int, char *[])
{
  itk::MetaImageIO::Pointer reader = itk::MetaImageIO::New();
  reader->SetNumberOfDimensions(3);

  bool gotException = false;
  try
  {
    reader->SetDimensions(3, 1);
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
  }
  if (!gotException)
  {
    std::cerr << "Failed to catch expected exception in method SetDimensions" << std::endl;
    return EXIT_FAILURE;
  }

  gotException = false;
  try
  {
    reader->SetOrigin(3, 1.0);
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
  }
  if (!gotException)
  {
    std::cerr << "Failed to catch expected exception in method SetOrigin" << std::endl;
    return EXIT_FAILURE;
  }

  gotException = false;
  try
  {
    reader->SetSpacing(3, 1.0);
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
  }
  if (!gotException)
  {
    std::cerr << "Failed to catch expected exception in method SetSpacing" << std::endl;
    return EXIT_FAILURE;
  }

  gotException = false;
  try
  {
    std::vector<double> direction(3);
    direction[0] = 1.0;
    direction[1] = 1.0;
    direction[2] = 1.0;
    reader->SetDirection(3, direction);
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
  }
  if (!gotException)
  {
    std::cerr << "Failed to catch expected exception in method SetDirection" << std::endl;
    return EXIT_FAILURE;
  }

  gotException = false;
  try
  {
    vnl_vector<double> direction(3);
    direction[0] = 1.0;
    direction[1] = 1.0;
    direction[2] = 1.0;
    reader->SetDirection(3, direction);
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
  }
  if (!gotException)
  {
    std::cerr << "Failed to catch expected exception in method SetDirection" << std::endl;
    return EXIT_FAILURE;
  }
  { // Test string <-> type conversions
    itk::IOComponentEnum listComponentType[] = { itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE,
                                                 itk::IOComponentEnum::UCHAR,
                                                 itk::IOComponentEnum::CHAR,
                                                 itk::IOComponentEnum::USHORT,
                                                 itk::IOComponentEnum::SHORT,
                                                 itk::IOComponentEnum::UINT,
                                                 itk::IOComponentEnum::INT,
                                                 itk::IOComponentEnum::ULONG,
                                                 itk::IOComponentEnum::LONG,
                                                 itk::IOComponentEnum::ULONGLONG,
                                                 itk::IOComponentEnum::LONGLONG,
                                                 itk::IOComponentEnum::FLOAT,
                                                 itk::IOComponentEnum::DOUBLE };
    const char *         listComponentTypeString[] = { "unknown", "unsigned_char",      "char",      "unsigned_short",
                                               "short",   "unsigned_int",       "int",       "unsigned_long",
                                               "long",    "unsigned_long_long", "long_long", "float",
                                               "double" };
    itk::IOPixelEnum     listIOPixelType[] = { itk::IOPixelEnum::UNKNOWNPIXELTYPE,
                                           itk::IOPixelEnum::SCALAR,
                                           itk::IOPixelEnum::RGB,
                                           itk::IOPixelEnum::RGBA,
                                           itk::IOPixelEnum::OFFSET,
                                           itk::IOPixelEnum::VECTOR,
                                           itk::IOPixelEnum::POINT,
                                           itk::IOPixelEnum::COVARIANTVECTOR,
                                           itk::IOPixelEnum::SYMMETRICSECONDRANKTENSOR,
                                           itk::IOPixelEnum::DIFFUSIONTENSOR3D,
                                           itk::IOPixelEnum::COMPLEX,
                                           itk::IOPixelEnum::FIXEDARRAY,
                                           itk::IOPixelEnum::MATRIX };
    const char *         listIOPixelTypeString[] = { "unknown",
                                             "scalar",
                                             "rgb",
                                             "rgba",
                                             "offset",
                                             "vector",
                                             "point",
                                             "covariant_vector",
                                             "symmetric_second_rank_tensor",
                                             "diffusion_tensor_3D",
                                             "complex",
                                             "fixed_array",
                                             "matrix" };
    CHECK_ARRAYS_HAVE_SAME_SIZE_AT_COMPILE_TIME(listComponentTypeString, listComponentType);
    CHECK_ARRAYS_HAVE_SAME_SIZE_AT_COMPILE_TIME(listIOPixelType, listIOPixelTypeString);
    size_t listComponentSize = sizeof(listComponentType) / sizeof(*listComponentType);
    size_t listPixelSize = sizeof(listIOPixelType) / sizeof(*listIOPixelType);
    { // Test the static version of the string <-> type conversions
      for (size_t i = 0; i < listComponentSize; ++i)
      {
        std::string componentTypeString = itk::ImageIOBase::GetComponentTypeAsString(listComponentType[i]);
        if (componentTypeString.compare(listComponentTypeString[i]) != 0)
        {
          std::cerr << "GetComponentTypeAsString(" << listComponentType[i] << ") should return '"
                    << listComponentTypeString[i] << "'" << std::endl;
          return EXIT_FAILURE;
        }
      }
      for (size_t i = 0; i < listPixelSize; ++i)
      {
        std::string pixelTypeString = itk::ImageIOBase::GetPixelTypeAsString(listIOPixelType[i]);
        if (pixelTypeString.compare(listIOPixelTypeString[i]) != 0)
        {
          std::cerr << "GetPixelTypeAsString(" << listIOPixelType[i] << ") should return '" << listIOPixelTypeString[i]
                    << "'" << std::endl;
          return EXIT_FAILURE;
        }
      }
      for (size_t i = 0; i < listComponentSize; ++i)
      {
        itk::IOComponentEnum componentType = itk::ImageIOBase::GetComponentTypeFromString(listComponentTypeString[i]);
        if (componentType != listComponentType[i])
        {
          std::cerr << "GetComponentTypeFromString('" << listComponentTypeString[i] << "') should return "
                    << listComponentType[i] << std::endl;
          return EXIT_FAILURE;
        }
      }
      for (size_t i = 0; i < listPixelSize; ++i)
      {
        itk::IOPixelEnum pixelType = itk::ImageIOBase::GetPixelTypeFromString(listIOPixelTypeString[i]);
        if (pixelType != listIOPixelType[i])
        {
          std::cerr << "GetPixelTypeFromString('" << listIOPixelTypeString[i] << "') should return "
                    << listIOPixelType[i] << std::endl;
          return EXIT_FAILURE;
        }
      }
    } // end Test the static version of the string <-> type conversions

    // Test the non-static version of the string <-> type conversions
    {
      // Create an instance of ImageIOBase. It does not matter that 'test' is not a valid image to read,
      // we just want the ImageIOBase object.
      itk::ImageIOBase::Pointer imageIOBase =
        itk::ImageIOFactory::CreateImageIO("test", itk::ImageIOFactory::IOFileModeEnum::ReadMode);
      for (size_t i = 0; i < listComponentSize; ++i)
      {
        std::string componentTypeString = imageIOBase->GetComponentTypeAsString(listComponentType[i]);
        if (componentTypeString.compare(listComponentTypeString[i]) != 0)
        {
          std::cerr << "GetComponentTypeAsString(" << listComponentType[i] << ") should return '"
                    << listComponentTypeString[i] << "'" << std::endl;
          return EXIT_FAILURE;
        }
      }
      for (size_t i = 0; i < listPixelSize; ++i)
      {
        std::string pixelTypeString = imageIOBase->GetPixelTypeAsString(listIOPixelType[i]);
        if (pixelTypeString.compare(listIOPixelTypeString[i]) != 0)
        {
          std::cerr << "GetPixelTypeAsString(" << listIOPixelType[i] << ") should return " << listIOPixelTypeString[i]
                    << std::endl;
          return EXIT_FAILURE;
        }
      }
      for (size_t i = 0; i < listComponentSize; ++i)
      {
        itk::IOComponentEnum componentType = imageIOBase->GetComponentTypeFromString(listComponentTypeString[i]);
        if (componentType != listComponentType[i])
        {
          std::cerr << "GetComponentTypeFromString('" << listComponentTypeString[i] << "') should return "
                    << listComponentType[i] << std::endl;
          return EXIT_FAILURE;
        }
      }
      for (size_t i = 0; i < listPixelSize; ++i)
      {
        itk::IOPixelEnum pixelType = imageIOBase->GetPixelTypeFromString(listIOPixelTypeString[i]);
        if (pixelType != listIOPixelType[i])
        {
          std::cerr << "GetPixelTypeFromString('" << listIOPixelTypeString[i] << "') should return "
                    << listIOPixelType[i] << std::endl;
          return EXIT_FAILURE;
        }
      }
    } // end Test the non-static version of the string <-> type conversions
  }

  { // Test SetPixelTypeInfo
    using IOPixelEnum = itk::CommonEnums::IOPixel;
    using IOComponentEnum = itk::CommonEnums::IOComponent;

    itk::MetaImageIO::Pointer imageIO = itk::MetaImageIO::New();

    imageIO->SetPixelTypeInfo(static_cast<const unsigned char *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 1);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::SCALAR);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::UCHAR);

    imageIO->SetPixelTypeInfo(static_cast<const float *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 1);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::SCALAR);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::FLOAT);

    imageIO->SetPixelTypeInfo(static_cast<const itk::RGBPixel<unsigned char> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 3);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::RGB);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::UCHAR);

    imageIO->SetPixelTypeInfo(static_cast<const itk::RGBAPixel<unsigned char> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 4);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::RGBA);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::UCHAR);

    imageIO->SetPixelTypeInfo(static_cast<const itk::Vector<float, 3> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 3);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::VECTOR);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::FLOAT);

    imageIO->SetPixelTypeInfo(static_cast<const itk::VariableLengthVector<unsigned char> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 1);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::VARIABLELENGTHVECTOR);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::UCHAR);

    imageIO->SetPixelTypeInfo(static_cast<const itk::CovariantVector<float, 2> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 2);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::COVARIANTVECTOR);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::FLOAT);

    imageIO->SetPixelTypeInfo(static_cast<const itk::SymmetricSecondRankTensor<float, 2> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 3);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::SYMMETRICSECONDRANKTENSOR);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::FLOAT);

    imageIO->SetPixelTypeInfo(static_cast<const itk::DiffusionTensor3D<double> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 6);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::DIFFUSIONTENSOR3D);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::DOUBLE);

    imageIO->SetPixelTypeInfo(static_cast<const itk::Matrix<float, 2, 2> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 4);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::MATRIX);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::FLOAT);

    imageIO->SetPixelTypeInfo(static_cast<const std::complex<double> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 2);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::COMPLEX);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::DOUBLE);

    imageIO->SetPixelTypeInfo(static_cast<const itk::Offset<3> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 3);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::OFFSET);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::LONG);

    imageIO->SetPixelTypeInfo(static_cast<const itk::Array<float> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 1);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::ARRAY);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::FLOAT);

    imageIO->SetPixelTypeInfo(static_cast<const itk::VariableSizeMatrix<double> *>(nullptr));
    ITK_TEST_EXPECT_EQUAL(imageIO->GetNumberOfComponents(), 1);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetPixelType(), IOPixelEnum::VARIABLESIZEMATRIX);
    ITK_TEST_EXPECT_EQUAL(imageIO->GetComponentType(), IOComponentEnum::DOUBLE);
  }
  return EXIT_SUCCESS;
}
