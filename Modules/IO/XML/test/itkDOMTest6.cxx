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

/*
This program tests operations of itk::StringTools.
*/

#include "itkStringTools.h"

#include <iostream>
#include "itkMacro.h"
#include "itkMath.h"

void testStringToolsWithBasicType();

void testStringToolsWithStdVector();

void testStringToolsWithItkArray();

void testStringToolsForStringOperations();

int itkDOMTest6( int, char*[] )
{
  try
    {
    testStringToolsWithBasicType();

    testStringToolsWithStdVector();

    testStringToolsWithItkArray();

    testStringToolsForStringOperations();
    }
  catch ( const itk::ExceptionObject& eo )
    {
    eo.Print( std::cerr );
    return EXIT_FAILURE;
    }
  catch ( const char* emsg )
    {
    std::cerr << emsg << std::endl;
    return EXIT_FAILURE;
    }
  catch ( ... )
    {
    std::cerr << "Unknown exception caught!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

// test for basic data type
void testStringToolsWithBasicType()
{
  // for unsigned char
    {
    typedef unsigned char DataType;

    std::string s;

    // write out
    DataType dataIn = '*';
    itk::StringTools::FromData( s, dataIn );

    // read back
    DataType dataOut = ' ';
    itk::StringTools::ToData( s, dataOut );

    // check result
    if ( dataIn != dataOut )
      {
      throw "uchar: input and output data do not match";
      }

    std::cout << "testStringToolsWithBasicType: uchar OK!" << std::endl;
    }

  // for short
    {
    typedef short DataType;

    std::string s;

    // write out
    DataType dataIn = -1024;
    itk::StringTools::FromData( s, dataIn );

    // read back
    DataType dataOut = 0;
    itk::StringTools::ToData( s, dataOut );

    // check result
    if ( dataIn != dataOut )
      {
      throw "short: input and output data do not match";
      }

    std::cout << "testStringToolsWithBasicType: short OK!" << std::endl;
    }

  // for double
    {
    typedef double DataType;

    std::string s;

    // write out
    DataType dataIn = -0.1;
    itk::StringTools::FromData( s, dataIn );

    // read back
    DataType dataOut = 0.0;
    itk::StringTools::ToData( s, dataOut );

    // check result
    if ( itk::Math::NotAlmostEquals( dataIn, dataOut ) )
      {
      throw "double: input and output data do not match";
      }

    std::cout << "testStringToolsWithBasicType: double OK!" << std::endl;
    }

  std::cout << "testStringToolsWithBasicType: Passed!" << std::endl;
}

// test for std::vector<T>
void testStringToolsWithStdVector()
{
  std::string svalue;
  std::string s;

  std::vector<float> dataIn( 10, -0.1f );
  itk::StringTools::FromData( svalue, dataIn );
  // add one more data element to the end of the string
  svalue.append( " 10 " );

  // read all data elements in the string
  std::vector<float> dataOut1;
  s = svalue;
  itk::StringTools::ToData( s, dataOut1 );
  // check successful or not
  if ( dataOut1.size() != (dataIn.size()+1) && dataOut1.back() != 10.0f )
    {
    throw "testStringToolsWithStdVector: failed reading all elements in the string (1)";
    }
  for ( size_t i = 0; i < dataIn.size(); i++ )
    {
    if ( itk::Math::NotAlmostEquals( dataIn[i], dataOut1[i] ) )
      {
      throw "testStringToolsWithStdVector: failed reading all elements in the string (2)";
      }
    }
  std::cout << "testStringToolsWithStdVector: dataOut1 OK!" << std::endl;

  // read all data elements for the output vector
  std::vector<float> dataOut2( 5, 0.0f );
  s = svalue;
  itk::StringTools::ToData( s, dataOut2, 0 );
  // check successful or not
  if ( dataOut2.size() != 5 )
    {
    throw "testStringToolsWithStdVector: failed reading all elements for the output vector (1)";
    }
  for ( size_t i = 0; i < dataOut2.size(); i++ )
    {
    if ( itk::Math::NotAlmostEquals( dataIn[i], dataOut2[i] ) )
      {
      throw "testStringToolsWithStdVector: failed reading all elements for the output vector (2)";
      }
    }
  std::cout << "testStringToolsWithStdVector: dataOut2 OK!" << std::endl;

  // read user-specified number of data elements (output data exist)
  std::vector<float> dataOut3( 10, 0.0f );
  s = svalue;
  itk::StringTools::ToData( s, dataOut3, 5 );
  // check successful or not
  if ( dataOut3.size() != 10 && dataOut3[5] != 0.0f )
    {
    throw "testStringToolsWithStdVector: failed reading user-specified number of elements (1.1)";
    }
  for ( size_t i = 0; i < 5; i++ )
    {
    if ( itk::Math::NotAlmostEquals( dataIn[i], dataOut3[i] ) )
      {
      throw "testStringToolsWithStdVector: failed reading user-specified number of elements (1.2)";
      }
    }
  std::cout << "testStringToolsWithStdVector: dataOut3 OK!" << std::endl;

  // read user-specified number of data elements (output data do not exist)
  std::vector<float> dataOut4;
  s = svalue;
  itk::StringTools::ToData( s, dataOut4, 5 );
  // check successful or not
  if ( dataOut4.size() != 5 )
    {
    throw "testStringToolsWithStdVector: failed reading user-specified number of elements (2.1)";
    }
  for ( size_t i = 0; i < 5; i++ )
    {
    if ( itk::Math::NotAlmostEquals( dataIn[i], dataOut4[i] ) )
      {
      throw "testStringToolsWithStdVector: failed reading user-specified number of elements (2.2)";
      }
    }
  std::cout << "testStringToolsWithStdVector: dataOut4 OK!" << std::endl;

  std::cout << "testStringToolsWithStdVector: Passed!" << std::endl;
}

// test for itk::Array<T>
void testStringToolsWithItkArray()
{
  typedef itk::Array<double> DataType;

  std::string svalue;
  std::string s;

  DataType dataIn( 10 );
  dataIn.Fill( -0.1 );
  itk::StringTools::FromData( svalue, dataIn );
  // add one more data element to the end of the string
  svalue.append( " 10 " );

  // read all data elements in the string
  DataType dataOut1;
  s = svalue;
  itk::StringTools::ToData( s, dataOut1 );
  // check successful or not
  if ( dataOut1.GetSize() != (dataIn.GetSize()+1) && dataOut1[10] != 10.0 )
    {
    throw "testStringToolsWithItkArray: failed reading all elements in the string (1)";
    }
  for ( unsigned int i = 0; i < dataIn.GetSize(); i++ )
    {
    if ( itk::Math::NotAlmostEquals( dataIn[i], dataOut1[i] ) )
      {
      throw "testStringToolsWithItkArray: failed reading all elements in the string (2)";
      }
    }
  std::cout << "testStringToolsWithItkArray: dataOut1 OK!" << std::endl;

  // read all data elements for the output vector
  DataType dataOut2( 5 );
  dataOut2.Fill( 0.0 );
  s = svalue;
  itk::StringTools::ToData( s, dataOut2, 0 );
  // check successful or not
  if ( dataOut2.GetSize() != 5 )
    {
    throw "testStringToolsWithItkArray: failed reading all elements for the output vector (1)";
    }
  for ( unsigned int i = 0; i < dataOut2.GetSize(); i++ )
    {
    if ( itk::Math::NotAlmostEquals( dataIn[i], dataOut2[i] ) )
      {
      throw "testStringToolsWithItkArray: failed reading all elements for the output vector (2)";
      }
    }
  std::cout << "testStringToolsWithItkArray: dataOut2 OK!" << std::endl;

  // read user-specified number of data elements (output data exist)
  DataType dataOut3( 10 );
  dataOut3.Fill( 0.0 );
  s = svalue;
  itk::StringTools::ToData( s, dataOut3, 5 );
  // check successful or not
  if ( dataOut3.GetSize() != 10 && dataOut3[5] != 0.0 )
    {
    throw "testStringToolsWithItkArray: failed reading user-specified number of elements (1.1)";
    }
  for ( unsigned int i = 0; i < 5; i++ )
    {
    if ( itk::Math::NotAlmostEquals( dataIn[i], dataOut3[i] ) )
      {
      throw "testStringToolsWithItkArray: failed reading user-specified number of elements (1.2)";
      }
    }
  std::cout << "testStringToolsWithItkArray: dataOut3 OK!" << std::endl;

  // read user-specified number of data elements (output data do not exist)
  DataType dataOut4;
  s = svalue;
  itk::StringTools::ToData( s, dataOut4, 5 );
  // check successful or not
  if ( dataOut4.GetSize() != 5 )
    {
    throw "testStringToolsWithItkArray: failed reading user-specified number of elements (2.1)";
    }
  for ( unsigned int i = 0; i < 5; i++ )
    {
    if ( itk::Math::NotAlmostEquals( dataIn[i], dataOut4[i] ) )
      {
      throw "testStringToolsWithItkArray: failed reading user-specified number of elements (2.2)";
      }
    }
  std::cout << "testStringToolsWithItkArray: dataOut4 OK!" << std::endl;

  std::cout << "testStringToolsWithItkArray: Passed!" << std::endl;
}

void testStringToolsForStringOperations()
{
  std::string s;

  s = " Hello World! ";
  if ( itk::StringTools::TrimLeft(s) != "Hello World! " )
    {
    throw "testStringToolsForStringOperations: failed trimming left";
    }
  std::cout << "testStringToolsForStringOperations: TrimLeft(-) OK!" << std::endl;

  s = " Hello World! ";
  if ( itk::StringTools::TrimRight(s) != " Hello World!" )
    {
    throw "testStringToolsForStringOperations: failed trimming right";
    }
  std::cout << "testStringToolsForStringOperations: TrimRight(-) OK!" << std::endl;

  s = " Hello World! ";
  if ( itk::StringTools::Trim(s) != "Hello World!" )
    {
    throw "testStringToolsForStringOperations: failed trimming both sides";
    }
  s = "Hello World!";
  if ( itk::StringTools::Trim(s) != "Hello World!" )
    {
      throw "testStringToolsForStringOperations: failed [not] trimming both sides";
    }
  s = "    ";
  if ( itk::StringTools::Trim(s) != "" )
    {
      throw "testStringToolsForStringOperations: failed trimming entire string";
    }
  std::cout << "testStringToolsForStringOperations: Trim(-) OK!" << std::endl;

  s = "Hello World!";
  if ( itk::StringTools::ToUpperCase(s) != "HELLO WORLD!" )
    {
    throw "testStringToolsForStringOperations: failed converting to upper case";
    }
  std::cout << "testStringToolsForStringOperations: ToUpperCase(-) OK!" << std::endl;

  s = "Hello World!";
  if ( itk::StringTools::ToLowerCase(s) != "hello world!" )
    {
    throw "testStringToolsForStringOperations: failed converting to lower case";
    }
  std::cout << "testStringToolsForStringOperations: ToLowerCase(-) OK!" << std::endl;

  s = " origin = 0 0 0 ";
  std::string lpart;
  std::string rpart;
  itk::StringTools::Split( s, lpart, rpart );
  if ( lpart != "origin" || rpart != "0 0 0" )
    {
    throw "testStringToolsForStringOperations: failed splitting into two parts";
    }
  std::cout << "testStringToolsForStringOperations: Split(-,-,-) OK!" << std::endl;

  s = " size = 256 256 100; spacing = 0.3 0.3 0.7; origin = * ";
  std::vector<std::string> parts;
  itk::StringTools::Split( s, parts );
  if ( parts[0] != "size = 256 256 100" || parts[1] != "spacing = 0.3 0.3 0.7" || parts[2] != "origin = *" )
    {
    throw "testStringToolsForStringOperations: failed splitting into a sequence of strings";
    }
  std::cout << "testStringToolsForStringOperations: Split(-,vector) OK!" << std::endl;

  s = " size = 256 256 100; spacing = 0.3 0.3 0.7; origin = * ";
  std::map<std::string,std::string> items;
  itk::StringTools::Split( s, items );
  if ( items["size"] != "256 256 100" || items["spacing"] != "0.3 0.3 0.7" || items["origin"] != "*" )
    {
    throw "testStringToolsForStringOperations: failed splitting into a map or dictionary";
    }
  std::cout << "testStringToolsForStringOperations: Split(-,map) OK!" << std::endl;

  s = "Hello World!";
  if ( !itk::StringTools::MatchWith(s,"hello world!") )
    {
    throw "testStringToolsForStringOperations: failed MatchWith testing (1)";
    }
  if ( itk::StringTools::MatchWith(s,"hello world!",false) )
    {
    throw "testStringToolsForStringOperations: failed MatchWith testing (2)";
    }
  if ( itk::StringTools::MatchWith(s," hello world! ") )
    {
    throw "testStringToolsForStringOperations: failed MatchWith testing (3)";
    }
  std::cout << "testStringToolsForStringOperations: MatchWith(-,-) OK!" << std::endl;

  s = "Hello World!";
  if ( !itk::StringTools::StartWith(s,"hello") )
    {
    throw "testStringToolsForStringOperations: failed StartWith testing (1)";
    }
  if ( itk::StringTools::StartWith(s,"hello",false) )
    {
    throw "testStringToolsForStringOperations: failed StartWith testing (2)";
    }
  std::cout << "testStringToolsForStringOperations: StartWith(-,-) OK!" << std::endl;

  s = "Hello World!";
  if ( !itk::StringTools::EndWith(s,"world!") )
    {
    throw "testStringToolsForStringOperations: failed EndWith testing (1)";
    }
  if ( itk::StringTools::EndWith(s,"world!",false) )
    {
    throw "testStringToolsForStringOperations: failed EndWith testing (2)";
    }
  std::cout << "testStringToolsForStringOperations: EndWith(-,-) OK!" << std::endl;

  s = "Hello World!";
  if ( !itk::StringTools::ContainSub(s,"Lo wo") )
    {
    throw "testStringToolsForStringOperations: failed ContainSub testing (1)";
    }
  if ( itk::StringTools::ContainSub(s,"Lo wo",false) )
    {
    throw "testStringToolsForStringOperations: failed ContainSub testing (2)";
    }
  std::cout << "testStringToolsForStringOperations: ContainSub(-,-) OK!" << std::endl;

  std::cout << "testStringToolsForStringOperations: Passed!" << std::endl;
}
