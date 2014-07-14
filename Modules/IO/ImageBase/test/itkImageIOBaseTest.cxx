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

  // Test the static version of the string <-> type conversions
  {
  std::string componentTypeString = itk::ImageIOBase::GetComponentTypeAsString(itk::ImageIOBase::UCHAR);
  if(componentTypeString.compare("unsigned_char") != 0)
    {
    std::cerr << "GetComponentTypeAsString(UCHAR) should return 'unsigned_char'"
              << std::endl;
    return EXIT_FAILURE;
    }

  std::string pixelTypeString = itk::ImageIOBase::GetPixelTypeAsString(itk::ImageIOBase::SCALAR);
  if(pixelTypeString.compare("scalar") != 0)
    {
    std::cerr << "GetPixelTypeAsString(SCALAR) should return 'scalar'"
              << std::endl;
    return EXIT_FAILURE;
    }

  itk::ImageIOBase::IOComponentType componentType = itk::ImageIOBase::GetComponentTypeFromString("unsigned_char");
  if(componentType != itk::ImageIOBase::UCHAR)
    {
    std::cerr << "GetComponentTypeFromString('unsigned_char') should return UCHAR"
              << std::endl;
    return EXIT_FAILURE;
    }

  itk::ImageIOBase::IOPixelType pixelType = itk::ImageIOBase::GetPixelTypeFromString("scalar");
  if(pixelType != itk::ImageIOBase::SCALAR)
    {
    std::cerr << "GetPixelTypeFromString('scalar') should return SCALAR"
              << std::endl;
    return EXIT_FAILURE;
    }
  }// end Test the static version of the string <-> type conversions

  // Test the non-static version of the string <-> type conversions
  {
  // Create an instance of ImageIOBase. It does not matter that 'test' is not a valid image to read,
  // we just want the ImageIOBase object.
  itk::ImageIOBase::Pointer imageIOBase = itk::ImageIOFactory::CreateImageIO(
    "test", itk::ImageIOFactory::ReadMode);

  std::string componentTypeString = imageIOBase->GetComponentTypeAsString(itk::ImageIOBase::UCHAR);
  if(componentTypeString.compare("unsigned_char") != 0)
    {
    std::cerr << "GetComponentTypeAsString(UCHAR) should return 'unsigned_char'"
              << std::endl;
    return EXIT_FAILURE;
    }

  std::string pixelTypeString = imageIOBase->GetPixelTypeAsString(itk::ImageIOBase::SCALAR);
  if(pixelTypeString.compare("scalar") != 0)
    {
    std::cerr << "GetPixelTypeAsString(SCALAR) should return 'scalar'"
              << std::endl;
    return EXIT_FAILURE;
    }

  itk::ImageIOBase::IOComponentType componentType = imageIOBase->GetComponentTypeFromString("unsigned_char");
  if(componentType != itk::ImageIOBase::UCHAR)
    {
    std::cerr << "GetComponentTypeFromString('unsigned_char') should return UCHAR"
              << std::endl;
    return EXIT_FAILURE;
    }

  itk::ImageIOBase::IOPixelType pixelType = imageIOBase->GetPixelTypeFromString("scalar");
  if(pixelType != itk::ImageIOBase::SCALAR)
    {
    std::cerr << "GetPixelTypeFromString('scalar') should return SCALAR"
              << std::endl;
    return EXIT_FAILURE;
    }
  }// end Test the non-static version of the string <-> type conversions

  return EXIT_SUCCESS;
}
