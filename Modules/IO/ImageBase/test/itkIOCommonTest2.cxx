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

#include "itkIOCommon.h"

int itkIOCommonTest2(int, char *[])
{
// Test the atomic pixel type to string conversions
    {
    std::string unsignedCharPixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_UCHAR);
    if ( unsignedCharPixelType.compare("unsigned char") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_UCHAR) should return 'unsigned char'";
      return EXIT_FAILURE;
      }

    std::string charPixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_CHAR);
    if ( charPixelType.compare("char") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_CHAR) should return 'char";
      return EXIT_FAILURE;
      }

    std::string unsignedShortPixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_USHORT);
    if ( unsignedShortPixelType.compare("unsigned short") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_USHORT) should return 'unsigned short";
      return EXIT_FAILURE;
      }

    std::string shortPixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_SHORT);
    if ( shortPixelType.compare("short") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_SHORT) should return 'short'";
      return EXIT_FAILURE;
      }

    std::string unsignedIntPixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_UINT);
    if ( unsignedIntPixelType.compare("unsigned int") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_UINT) should return 'unsigned int'";
      return EXIT_FAILURE;
      }

    std::string intPixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_INT);
    if ( intPixelType.compare("int") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_INT) should return 'int'";
      return EXIT_FAILURE;
      }

    std::string unsignedLongPixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_ULONG);
    if ( unsignedLongPixelType.compare("unsigned long") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_ULONG) should return 'unsigned long'";
      return EXIT_FAILURE;
      }

    std::string longPixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_LONG);
    if ( longPixelType.compare("long") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_LONG) should return 'long'";
      return EXIT_FAILURE;
      }

    std::string floatPixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_FLOAT);
    if ( floatPixelType.compare("float") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_FLOAT) should return 'float'";
      return EXIT_FAILURE;
      }

    std::string doublePixelType = itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::ITK_DOUBLE);
    if ( doublePixelType.compare("double") != 0 )
      {
      std::cerr << "AtomicPixelTypeToString(ITK_DOUBLE) should return 'double'";
      return EXIT_FAILURE;
      }
    }

// Test the atomic pixel type size computation
    {
    unsigned int unsignedCharPixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_UCHAR);
    if ( unsignedCharPixelTypeSize !=  static_cast< unsigned char >( sizeof( unsigned char ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_UCHAR) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }

    unsigned int charPixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_CHAR);
    if ( charPixelTypeSize !=  static_cast< unsigned int >( sizeof( unsigned char ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_CHAR) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }

    unsigned int unsignedShortPixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_USHORT);
    if ( unsignedShortPixelTypeSize !=  static_cast< unsigned int >( sizeof( unsigned short ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_USHORT) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }
    unsigned int shortPixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_SHORT);
    if ( shortPixelTypeSize !=  static_cast< unsigned int >( sizeof( short ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_SHORT) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }
    unsigned int unsignedIntPixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_UINT);
    if ( unsignedIntPixelTypeSize !=  static_cast< unsigned int >( sizeof( unsigned int ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_UINT) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }

    unsigned int intPixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_INT);
    if ( intPixelTypeSize !=  static_cast< unsigned int >( sizeof( int ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_INT) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }

    unsigned int unsignedLongPixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_ULONG);
    if ( unsignedLongPixelTypeSize !=  static_cast< unsigned int >( sizeof( unsigned long ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_ULONG) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }

    unsigned int longPixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_LONG);
    if ( longPixelTypeSize !=  static_cast< unsigned int >( sizeof( long ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_ULONG) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }

    unsigned int floatPixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_FLOAT);
    if ( floatPixelTypeSize !=  static_cast< unsigned int >( sizeof( float ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_FLOAT) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }

    unsigned int doublePixelTypeSize = itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::ITK_DOUBLE);
    if ( doublePixelTypeSize !=  static_cast< unsigned int >( sizeof( double ) ) )
      {
      std::cerr << "ComputeSizeOfAtomicPixelType(ITK_DOUBLE) is not returning the correct pixel size ";
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
