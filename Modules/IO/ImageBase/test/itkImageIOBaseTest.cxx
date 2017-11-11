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

#include "itkMetaImageIO.h"

#include "itkImageIOFactory.h" // required to instantiate an instance of ImageIOBase

#define SPECIFIC_IMAGEIO_MODULE_TEST

// Macro to check that two arrays have the same size at compile time. It doesn't compile if they don't
// as it tries to create an array of size(-1)
// https://scaryreasoner.wordpress.com/2009/02/28/checking-sizeof-at-compile-time/
#define CHECK_ARRAYS_HAVE_SAME_SIZE_AT_COMPILE_TIME(array1, array2)\
 ((void)sizeof(char[1 - 2*!!( sizeof(array1) / sizeof(*array1) - sizeof(array2) / sizeof(*array2) )]))

int itkImageIOBaseTest( int , char * [] )
{
  itk::MetaImageIO::Pointer reader = itk::MetaImageIO::New();
  reader->SetNumberOfDimensions(3);

  bool gotException = false;
  try
    {
    reader->SetDimensions(3,1);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetDimensions"
              << std::endl;
    return EXIT_FAILURE;
    }

  gotException = false;
  try
    {
    reader->SetOrigin(3,1.0);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetOrigin"
              << std::endl;
    return EXIT_FAILURE;
    }

  gotException = false;
  try
    {
    reader->SetSpacing(3,1.0);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetSpacing"
              << std::endl;
    return EXIT_FAILURE;
    }

  gotException = false;
  try
    {
    std::vector<double> direction(3);
    direction[0] = 1.0;
    direction[1] = 1.0;
    direction[2] = 1.0;
    reader->SetDirection(3,direction);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetDirection"
              << std::endl;
    return EXIT_FAILURE;
    }

  gotException = false;
  try
    {
    vnl_vector<double> direction(3);
    direction[0] = 1.0;
    direction[1] = 1.0;
    direction[2] = 1.0;
    reader->SetDirection(3,direction);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetDirection"
              << std::endl;
    return EXIT_FAILURE;
    }
  { // Test string <-> type conversions
  itk::ImageIOBase::IOComponentType listComponentType[] = {itk::ImageIOBase::UNKNOWNCOMPONENTTYPE,
                                                           itk::ImageIOBase::UCHAR, itk::ImageIOBase::CHAR,
                                                           itk::ImageIOBase::USHORT, itk::ImageIOBase::SHORT,
                                                           itk::ImageIOBase::UINT, itk::ImageIOBase::INT,
                                                           itk::ImageIOBase::ULONG, itk::ImageIOBase::LONG,
                                                           itk::ImageIOBase::ULONGLONG, itk::ImageIOBase::LONGLONG,
                                                           itk::ImageIOBase::FLOAT, itk::ImageIOBase::DOUBLE};
  const char* listComponentTypeString[] = {"unknown", "unsigned_char", "char", "unsigned_short", "short", "unsigned_int", "int",
                                           "unsigned_long", "long", "unsigned_long_long", "long_long", "float", "double"};
  itk::ImageIOBase::IOPixelType listIOPixelType[] = {itk::ImageIOBase::UNKNOWNPIXELTYPE,itk::ImageIOBase::SCALAR,
                                                     itk::ImageIOBase::RGB, itk::ImageIOBase::RGBA,
                                                     itk::ImageIOBase::OFFSET, itk::ImageIOBase::VECTOR,
                                                     itk::ImageIOBase::POINT, itk::ImageIOBase::COVARIANTVECTOR,
                                                     itk::ImageIOBase::SYMMETRICSECONDRANKTENSOR,
                                                     itk::ImageIOBase::DIFFUSIONTENSOR3D, itk::ImageIOBase::COMPLEX,
                                                     itk::ImageIOBase::FIXEDARRAY, itk::ImageIOBase::MATRIX};
  const char* listIOPixelTypeString[] = {"unknown", "scalar", "rgb", "rgba", "offset", "vector", "point", "covariant_vector",
                                         "symmetric_second_rank_tensor", "diffusion_tensor_3D", "complex",
                                         "fixed_array", "matrix"};
  CHECK_ARRAYS_HAVE_SAME_SIZE_AT_COMPILE_TIME(listComponentTypeString, listComponentType);
  CHECK_ARRAYS_HAVE_SAME_SIZE_AT_COMPILE_TIME(listIOPixelType, listIOPixelTypeString);
  size_t listComponentSize = sizeof(listComponentType) / sizeof(*listComponentType);
  size_t listPixelSize = sizeof(listIOPixelType) / sizeof(*listIOPixelType);
    {  // Test the static version of the string <-> type conversions
    for(size_t i = 0; i < listComponentSize; ++i)
      {
      std::string componentTypeString = itk::ImageIOBase::GetComponentTypeAsString(listComponentType[i]);
      if(componentTypeString.compare(listComponentTypeString[i]) != 0)
        {
        std::cerr << "GetComponentTypeAsString("<<listComponentType[i]<<") should return '"<<listComponentTypeString[i]<<"'"
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    for(size_t i = 0; i < listPixelSize; ++i)
      {
      std::string pixelTypeString = itk::ImageIOBase::GetPixelTypeAsString(listIOPixelType[i]);
      if(pixelTypeString.compare(listIOPixelTypeString[i]) != 0)
        {
        std::cerr << "GetPixelTypeAsString("<< listIOPixelType[i] <<") should return '"<< listIOPixelTypeString[i] <<"'"
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    for(size_t i = 0; i < listComponentSize; ++i)
      {
      itk::ImageIOBase::IOComponentType componentType = itk::ImageIOBase::GetComponentTypeFromString(listComponentTypeString[i]);
      if(componentType != listComponentType[i])
        {
        std::cerr << "GetComponentTypeFromString('"<< listComponentTypeString[i] <<"') should return " << listComponentType[i]
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    for(size_t i = 0; i < listPixelSize; ++i)
      {
      itk::ImageIOBase::IOPixelType pixelType = itk::ImageIOBase::GetPixelTypeFromString(listIOPixelTypeString[i]);
      if(pixelType != listIOPixelType[i])
        {
        std::cerr << "GetPixelTypeFromString('"<< listIOPixelTypeString[i] << "') should return " << listIOPixelType[i]
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    }// end Test the static version of the string <-> type conversions

    // Test the non-static version of the string <-> type conversions
    {
    // Create an instance of ImageIOBase. It does not matter that 'test' is not a valid image to read,
    // we just want the ImageIOBase object.
    itk::ImageIOBase::Pointer imageIOBase = itk::ImageIOFactory::CreateImageIO(
      "test", itk::ImageIOFactory::ReadMode);
    for(size_t i = 0; i < listComponentSize; ++i)
      {
      std::string componentTypeString = imageIOBase->GetComponentTypeAsString(listComponentType[i]);
      if(componentTypeString.compare(listComponentTypeString[i]) != 0)
        {
        std::cerr << "GetComponentTypeAsString("<< listComponentType[i] <<") should return '" << listComponentTypeString[i] <<"'"
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    for(size_t i = 0; i < listPixelSize; ++i)
      {
      std::string pixelTypeString = imageIOBase->GetPixelTypeAsString(listIOPixelType[i]);
      if(pixelTypeString.compare(listIOPixelTypeString[i]) != 0)
        {
        std::cerr << "GetPixelTypeAsString("<< listIOPixelType[i] <<") should return " << listIOPixelTypeString[i]
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    for(size_t i = 0; i < listComponentSize; ++i)
      {
      itk::ImageIOBase::IOComponentType componentType = imageIOBase->GetComponentTypeFromString(listComponentTypeString[i]);
      if(componentType != listComponentType[i])
        {
        std::cerr << "GetComponentTypeFromString('" << listComponentTypeString[i] << "') should return " << listComponentType[i]
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    for(size_t i = 0; i < listPixelSize; ++i)
      {
      itk::ImageIOBase::IOPixelType pixelType = imageIOBase->GetPixelTypeFromString(listIOPixelTypeString[i]);
      if(pixelType != listIOPixelType[i])
        {
        std::cerr << "GetPixelTypeFromString('"<< listIOPixelTypeString[i] <<"') should return " << listIOPixelType[i]
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    }// end Test the non-static version of the string <-> type conversions
  }
  return EXIT_SUCCESS;
}
