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
#include "itkIntTypes.h"
#include "itkVectorImage.h"

#include "itkImageIOFactory.h" // required to instantiate an instance of ImageIOBase

namespace
{
// Tests `MapPixelType<TPixel>::CType` and the estimation of component type traits.
class TestMapPixelTypeAndComponentTypeTraits : private itk::ImageIOBase
{
  struct UnknownComponentType
  {};
  static_assert(MapPixelType<UnknownComponentType>::CType == IOComponentEnum::UNKNOWNCOMPONENTTYPE);

  // Test built-in types:
  static_assert(MapPixelType<unsigned char>::CType == IOComponentEnum::UCHAR);
  static_assert(MapPixelType<signed char>::CType == IOComponentEnum::CHAR);
  static_assert(MapPixelType<char>::CType == (std::is_signed_v<char> ? IOComponentEnum::CHAR : IOComponentEnum::UCHAR));
  static_assert(MapPixelType<unsigned short>::CType == IOComponentEnum::USHORT);
  static_assert(MapPixelType<short>::CType == IOComponentEnum::SHORT);
  static_assert(MapPixelType<unsigned int>::CType == IOComponentEnum::UINT);
  static_assert(MapPixelType<int>::CType == IOComponentEnum::INT);
  static_assert(MapPixelType<unsigned long>::CType == IOComponentEnum::ULONG);
  static_assert(MapPixelType<long>::CType == IOComponentEnum::LONG);
  static_assert(MapPixelType<unsigned long long>::CType == IOComponentEnum::ULONGLONG);
  static_assert(MapPixelType<long long>::CType == IOComponentEnum::LONGLONG);
  static_assert(MapPixelType<float>::CType == IOComponentEnum::FLOAT);
  static_assert(MapPixelType<double>::CType == IOComponentEnum::DOUBLE);

  // Test fixed width types:
  static_assert(MapPixelType<uint8_t>::CType == IOComponentEnum::UINT8);
  static_assert(MapPixelType<int8_t>::CType == IOComponentEnum::INT8);
  static_assert(MapPixelType<uint16_t>::CType == IOComponentEnum::UINT16);
  static_assert(MapPixelType<int16_t>::CType == IOComponentEnum::INT16);
  static_assert(MapPixelType<uint32_t>::CType == IOComponentEnum::UINT32);
  static_assert(MapPixelType<int32_t>::CType == IOComponentEnum::INT32);
  static_assert(MapPixelType<uint64_t>::CType == IOComponentEnum::UINT64);
  static_assert(MapPixelType<int64_t>::CType == IOComponentEnum::INT64);
  static_assert(MapPixelType<float>::CType == IOComponentEnum::FLOAT32);
  static_assert(MapPixelType<double>::CType == IOComponentEnum::FLOAT64);

  // Tests IsComponentTypeFloatingPoint:
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::UNKNOWNCOMPONENTTYPE));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::UCHAR));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::CHAR));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::USHORT));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::SHORT));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::UINT));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::INT));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::ULONG));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::LONG));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::LONGLONG));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::ULONGLONG));
  static_assert(IsComponentTypeFloatingPoint(IOComponentEnum::FLOAT));
  static_assert(IsComponentTypeFloatingPoint(IOComponentEnum::DOUBLE));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::UINT8));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::INT8));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::UINT16));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::INT16));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::UINT32));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::INT32));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::UINT64));
  static_assert(!IsComponentTypeFloatingPoint(IOComponentEnum::INT64));
  static_assert(IsComponentTypeFloatingPoint(IOComponentEnum::FLOAT32));
  static_assert(IsComponentTypeFloatingPoint(IOComponentEnum::FLOAT64));

  // Tests IsComponentTypeUnsigned:
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::UNKNOWNCOMPONENTTYPE));
  static_assert(IsComponentTypeUnsigned(IOComponentEnum::UCHAR));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::CHAR));
  static_assert(IsComponentTypeUnsigned(IOComponentEnum::USHORT));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::SHORT));
  static_assert(IsComponentTypeUnsigned(IOComponentEnum::UINT));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::INT));
  static_assert(IsComponentTypeUnsigned(IOComponentEnum::ULONG));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::LONG));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::LONGLONG));
  static_assert(IsComponentTypeUnsigned(IOComponentEnum::ULONGLONG));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::FLOAT));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::DOUBLE));
  static_assert(IsComponentTypeUnsigned(IOComponentEnum::UINT8));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::INT8));
  static_assert(IsComponentTypeUnsigned(IOComponentEnum::UINT16));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::INT16));
  static_assert(IsComponentTypeUnsigned(IOComponentEnum::UINT32));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::INT32));
  static_assert(IsComponentTypeUnsigned(IOComponentEnum::UINT64));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::INT64));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::FLOAT32));
  static_assert(!IsComponentTypeUnsigned(IOComponentEnum::FLOAT64));

  // Tests GetNumberOfBitsOfComponentType:
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::UINT8) == 8);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::INT8) == 8);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::UINT16) == 16);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::INT16) == 16);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::UINT32) == 32);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::INT32) == 32);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::UINT64) == 64);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::INT64) == 64);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::FLOAT32) == 32);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::FLOAT64) == 64);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::UNKNOWNCOMPONENTTYPE) == 0);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::UCHAR) == sizeof(char) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::CHAR) == sizeof(char) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::USHORT) == sizeof(short) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::SHORT) == sizeof(short) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::UINT) == sizeof(int) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::INT) == sizeof(int) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::ULONG) == sizeof(long) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::LONG) == sizeof(long) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::LONGLONG) == sizeof(long long) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::ULONGLONG) == sizeof(long long) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::FLOAT) == sizeof(float) * CHAR_BIT);
  static_assert(GetNumberOfBitsOfComponentType(IOComponentEnum::DOUBLE) == sizeof(double) * CHAR_BIT);

  // Tests GetComponentTypeFromTypeTraits:
  static_assert(GetComponentTypeFromTypeTraits(false, false, 0) == IOComponentEnum::UNKNOWNCOMPONENTTYPE);
  static_assert(GetComponentTypeFromTypeTraits(false, true, 8) == IOComponentEnum::UINT8);
  static_assert(GetComponentTypeFromTypeTraits(false, false, 8) == IOComponentEnum::INT8);
  static_assert(GetComponentTypeFromTypeTraits(false, true, 16) == IOComponentEnum::UINT16);
  static_assert(GetComponentTypeFromTypeTraits(false, false, 16) == IOComponentEnum::INT16);
  static_assert(GetComponentTypeFromTypeTraits(false, true, 32) == IOComponentEnum::UINT32);
  static_assert(GetComponentTypeFromTypeTraits(false, false, 32) == IOComponentEnum::INT32);
  static_assert(GetComponentTypeFromTypeTraits(false, true, 64) == IOComponentEnum::UINT64);
  static_assert(GetComponentTypeFromTypeTraits(false, false, 64) == IOComponentEnum::INT64);
  static_assert(GetComponentTypeFromTypeTraits(true, false, 32) == IOComponentEnum::FLOAT32);
  static_assert(GetComponentTypeFromTypeTraits(true, false, 64) == IOComponentEnum::FLOAT64);
};
} // namespace


// Specific ImageIO test

int
itkImageIOBaseTest(int, char *[])
{
  const itk::MetaImageIO::Pointer reader = itk::MetaImageIO::New();
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
    constexpr itk::IOComponentEnum listComponentType[] = { itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE,
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
    const char * listComponentTypeString[] = { "unknown", "unsigned_char",      "char",      "unsigned_short",
                                               "short",   "unsigned_int",       "int",       "unsigned_long",
                                               "long",    "unsigned_long_long", "long_long", "float",
                                               "double" };
    constexpr itk::IOPixelEnum listIOPixelType[] = { itk::IOPixelEnum::UNKNOWNPIXELTYPE,
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
    const char *               listIOPixelTypeString[] = { "unknown",
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
    // Compile time verification that the array lengths are correct for type and name
    static_assert(std::size(listComponentType) == std::size(listComponentTypeString),
                  "listComponentType and listComponentTypeString must be same length");
    static_assert(std::size(listIOPixelType) == std::size(listIOPixelTypeString),
                  "listIOPixelType and listIOPixelTypeString must be same length");

    constexpr size_t listComponentSize{ std::size(listComponentType) };
    constexpr size_t listPixelSize{ std::size(listIOPixelType) };

    for (size_t i = 0; i < listComponentSize; ++i)
    {
      const std::string componentTypeString = itk::ImageIOBase::GetComponentTypeAsString(listComponentType[i]);
      if (componentTypeString.compare(listComponentTypeString[i]) != 0)
      {
        std::cerr << "GetComponentTypeAsString(" << listComponentType[i] << ") should return '"
                  << listComponentTypeString[i] << '\'' << std::endl;
        return EXIT_FAILURE;
      }
    }
    for (size_t i = 0; i < listPixelSize; ++i)
    {
      const std::string pixelTypeString = itk::ImageIOBase::GetPixelTypeAsString(listIOPixelType[i]);
      if (pixelTypeString.compare(listIOPixelTypeString[i]) != 0)
      {
        std::cerr << "GetPixelTypeAsString(" << listIOPixelType[i] << ") should return '" << listIOPixelTypeString[i]
                  << '\'' << std::endl;
        return EXIT_FAILURE;
      }
    }
    for (size_t i = 0; i < listComponentSize; ++i)
    {
      const itk::IOComponentEnum componentType =
        itk::ImageIOBase::GetComponentTypeFromString(listComponentTypeString[i]);
      if (componentType != listComponentType[i])
      {
        std::cerr << "GetComponentTypeFromString('" << listComponentTypeString[i] << "') should return "
                  << listComponentType[i] << std::endl;
        return EXIT_FAILURE;
      }
    }
    for (size_t i = 0; i < listPixelSize; ++i)
    {
      const itk::IOPixelEnum pixelType = itk::ImageIOBase::GetPixelTypeFromString(listIOPixelTypeString[i]);
      if (pixelType != listIOPixelType[i])
      {
        std::cerr << "GetPixelTypeFromString('" << listIOPixelTypeString[i] << "') should return " << listIOPixelType[i]
                  << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  { // Test SetPixelTypeInfo
    using IOPixelEnum = itk::CommonEnums::IOPixel;
    using IOComponentEnum = itk::CommonEnums::IOComponent;

    const itk::MetaImageIO::Pointer imageIO = itk::MetaImageIO::New();

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
