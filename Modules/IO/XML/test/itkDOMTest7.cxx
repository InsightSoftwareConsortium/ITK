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
This program tests operations of itk::FancyString.
*/

#include "itkFancyString.h"

#include <iostream>
#include "itkMacro.h"
#include "itkMath.h"

void testFancyStringWithBasicType();

void testFancyStringWithStdVector();

void testFancyStringWithItkArray();

void testFancyStringForStringOperations();

int itkDOMTest7( int, char*[] )
{
  try
    {
    testFancyStringWithBasicType();

    testFancyStringWithStdVector();

    testFancyStringWithItkArray();

    testFancyStringForStringOperations();
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
void testFancyStringWithBasicType()
{
  // for unsigned char
    {
    typedef unsigned char DataType;

    itk::FancyString s;

    // write out
    DataType dataIn = '*';
    s << dataIn;

    // read back
    DataType dataOut = ' ';
    s >> dataOut;

    // check result
    if ( dataIn != dataOut )
      {
      throw "uchar: input and output data do not match";
      }

    std::cout << "testFancyStringWithBasicType: uchar OK!" << std::endl;
    }

  // for short
    {
    typedef short DataType;

    itk::FancyString s;

    // write out
    DataType dataIn = -1024;
    s << dataIn;

    // read back
    DataType dataOut = 0;
    s >> dataOut;

    // check result
    if ( dataIn != dataOut )
      {
      throw "short: input and output data do not match";
      }

    std::cout << "testFancyStringWithBasicType: short OK!" << std::endl;
    }

  // for double
    {
    typedef double DataType;

    itk::FancyString s;

    // write out
    DataType dataIn = -0.1;
    s << dataIn;

    // read back
    DataType dataOut = 0.0;
    s >> dataOut;

    // check result
    if ( itk::Math::NotExactlyEquals(dataIn, dataOut) )
      {
      throw "double: input and output data do not match";
      }

    std::cout << "testFancyStringWithBasicType: double OK!" << std::endl;
    }

  // all testings were successful if reached here
}

// test for std::vector<T>
void testFancyStringWithStdVector()
{
  itk::FancyString svalue;
  itk::FancyString s;

  std::vector<float> dataIn( 10, -0.1f );
  svalue << dataIn;
  // add one more data element to the end of the string
  svalue.Append( " 10 " );

  // read all data elements in the string
  std::vector<float> dataOut1;
  s = svalue;
  s.ToData( dataOut1 );
  // check successful or not
  if ( dataOut1.size() != (dataIn.size()+1) && dataOut1.back() != 10.0f )
    {
    throw "testFancyStringWithStdVector: failed reading all elements in the string (1)";
    }
  for ( size_t i = 0; i < dataIn.size(); i++ )
    {
    if ( itk::Math::NotExactlyEquals(dataIn[i], dataOut1[i]) )
      {
      throw "testFancyStringWithStdVector: failed reading all elements in the string (2)";
      }
    }
  std::cout << "testFancyStringWithStdVector: dataOut1 OK!" << std::endl;

  // read all data elements for the output vector
  std::vector<float> dataOut2( 5, 0.0f );
  s = svalue;
  s >> dataOut2;
  // check successful or not
  if ( dataOut2.size() != 5 )
    {
    throw "testFancyStringWithStdVector: failed reading all elements for the output vector (1)";
    }
  for ( size_t i = 0; i < dataOut2.size(); i++ )
    {
    if ( itk::Math::NotExactlyEquals(dataIn[i], dataOut2[i]) )
      {
      throw "testFancyStringWithStdVector: failed reading all elements for the output vector (2)";
      }
    }
  std::cout << "testFancyStringWithStdVector: dataOut2 OK!" << std::endl;

  // read user-specified number of data elements (output data exist)
  std::vector<float> dataOut3( 10, 0.0f );
  s = svalue;
  s.ToData( dataOut3, 5 );
  // check successful or not
  if ( dataOut3.size() != 10 && dataOut3[5] != 0.0f )
    {
    throw "testFancyStringWithStdVector: failed reading user-specified number of elements (1.1)";
    }
  for ( size_t i = 0; i < 5; i++ )
    {
    if ( itk::Math::NotExactlyEquals(dataIn[i], dataOut3[i]) )
      {
      throw "testFancyStringWithStdVector: failed reading user-specified number of elements (1.2)";
      }
    }
  std::cout << "testFancyStringWithStdVector: dataOut3 OK!" << std::endl;

  // read user-specified number of data elements (output data do not exist)
  std::vector<float> dataOut4;
  s = svalue;
  s.ToData( dataOut4, 5 );
  // check successful or not
  if ( dataOut4.size() != 5 )
    {
    throw "testFancyStringWithStdVector: failed reading user-specified number of elements (2.1)";
    }
  for ( size_t i = 0; i < 5; i++ )
    {
    if ( itk::Math::NotExactlyEquals(dataIn[i], dataOut4[i]) )
      {
      throw "testFancyStringWithStdVector: failed reading user-specified number of elements (2.2)";
      }
    }
  std::cout << "testFancyStringWithStdVector: dataOut4 OK!" << std::endl;

  // all testings were successful if reached here
}

// test for itk::Array<T>
void testFancyStringWithItkArray()
{
  typedef itk::Array<double> DataType;

  itk::FancyString svalue;
  itk::FancyString s;

  DataType dataIn( 10 );
  dataIn.Fill( -0.1 );
  svalue << dataIn;
  // add one more data element to the end of the string
  svalue.Append( " 10 " );

  // read all data elements in the string
  DataType dataOut1;
  s = svalue;
  s.ToData( dataOut1 );
  // check successful or not
  if ( dataOut1.GetSize() != (dataIn.GetSize()+1) && dataOut1[10] != 10.0 )
    {
    throw "testFancyStringWithItkArray: failed reading all elements in the string (1)";
    }
  for ( unsigned int i = 0; i < dataIn.GetSize(); i++ )
    {
    if ( itk::Math::NotExactlyEquals(dataIn[i], dataOut1[i]) )
      {
      throw "testFancyStringWithItkArray: failed reading all elements in the string (2)";
      }
    }
  std::cout << "testFancyStringWithItkArray: dataOut1 OK!" << std::endl;

  // read all data elements for the output vector
  DataType dataOut2( 5 );
  dataOut2.Fill( 0.0 );
  s = svalue;
  s >> dataOut2;
  // check successful or not
  if ( dataOut2.GetSize() != 5 )
    {
    throw "testFancyStringWithItkArray: failed reading all elements for the output vector (1)";
    }
  for ( unsigned int i = 0; i < dataOut2.GetSize(); i++ )
    {
    if ( itk::Math::NotExactlyEquals(dataIn[i], dataOut2[i]) )
      {
      throw "testFancyStringWithItkArray: failed reading all elements for the output vector (2)";
      }
    }
  std::cout << "testFancyStringWithItkArray: dataOut2 OK!" << std::endl;

  // read user-specified number of data elements (output data exist)
  DataType dataOut3( 10 );
  dataOut3.Fill( 0.0 );
  s = svalue;
  s.ToData( dataOut3, 5 );
  // check successful or not
  if ( dataOut3.GetSize() != 10 && dataOut3[5] != 0.0 )
    {
    throw "testFancyStringWithItkArray: failed reading user-specified number of elements (1.1)";
    }
  for ( unsigned int i = 0; i < 5; i++ )
    {
    if ( itk::Math::NotExactlyEquals(dataIn[i], dataOut3[i]) )
      {
      throw "testFancyStringWithItkArray: failed reading user-specified number of elements (1.2)";
      }
    }
  std::cout << "testFancyStringWithItkArray: dataOut3 OK!" << std::endl;

  // read user-specified number of data elements (output data do not exist)
  DataType dataOut4;
  s = svalue;
  s.ToData( dataOut4, 5 );
  // check successful or not
  if ( dataOut4.GetSize() != 5 )
    {
    throw "testFancyStringWithItkArray: failed reading user-specified number of elements (2.1)";
    }
  for ( unsigned int i = 0; i < 5; i++ )
    {
    if ( itk::Math::NotExactlyEquals(dataIn[i], dataOut4[i]) )
      {
      throw "testFancyStringWithItkArray: failed reading user-specified number of elements (2.2)";
      }
    }
  std::cout << "testFancyStringWithItkArray: dataOut4 OK!" << std::endl;

  // all testings were successful if reached here
}

void testFancyStringForStringOperations()
{
  itk::FancyString s;

  s = " Hello World! ";
  if ( s.TrimLeft() != "Hello World! " )
    {
    throw "testFancyStringForStringOperations: failed trimming left";
    }
  std::cout << "testFancyStringForStringOperations: TrimLeft() OK!" << std::endl;

  s = " Hello World! ";
  if ( s.TrimRight() != " Hello World!" )
    {
    throw "testFancyStringForStringOperations: failed trimming right";
    }
  std::cout << "testFancyStringForStringOperations: TrimRight() OK!" << std::endl;

  s = " Hello World! ";
  if ( s.Trim() != "Hello World!" )
    {
    throw "testFancyStringForStringOperations: failed trimming both sides";
    }
  std::cout << "testFancyStringForStringOperations: Trim() OK!" << std::endl;

  s = "Hello World!";
  if ( s.ToUpperCase() != "HELLO WORLD!" )
    {
    throw "testFancyStringForStringOperations: failed converting to upper case";
    }
  std::cout << "testFancyStringForStringOperations: ToUpperCase() OK!" << std::endl;

  s = "Hello World!";
  if ( s.ToLowerCase() != "hello world!" )
    {
    throw "testFancyStringForStringOperations: failed converting to lower case";
    }
  std::cout << "testFancyStringForStringOperations: ToLowerCase() OK!" << std::endl;

  s = " origin = 0 0 0 ";
  std::string lpart;
  std::string rpart;
  s.Split( lpart, rpart );
  if ( lpart != "origin" || rpart != "0 0 0" )
    {
    throw "testFancyStringForStringOperations: failed splitting into two parts";
    }
  std::cout << "testFancyStringForStringOperations: Split(-,-) OK!" << std::endl;

  s = " size = 256 256 100; spacing = 0.3 0.3 0.7; origin = * ";
  std::vector<std::string> parts;
  s.Split( parts );
  if ( parts[0] != "size = 256 256 100" || parts[1] != "spacing = 0.3 0.3 0.7" || parts[2] != "origin = *" )
    {
    throw "testFancyStringForStringOperations: failed splitting into a sequence of strings";
    }
  std::cout << "testFancyStringForStringOperations: Split(vector) OK!" << std::endl;

  s = " size = 256 256 100; spacing = 0.3 0.3 0.7; origin = * ";
  std::map<std::string,std::string> items;
  s.Split( items );
  if ( items["size"] != "256 256 100" || items["spacing"] != "0.3 0.3 0.7" || items["origin"] != "*" )
    {
    throw "testFancyStringForStringOperations: failed splitting into a map or dictionary";
    }
  std::cout << "testFancyStringForStringOperations: Split(map) OK!" << std::endl;

  s = "Hello World!";
  if ( !s.MatchWith("hello world!") )
    {
    throw "testFancyStringForStringOperations: failed MatchWith testing (1)";
    }
  if ( s.MatchWith("hello world!",false) )
    {
    throw "testFancyStringForStringOperations: failed MatchWith testing (2)";
    }
  if ( s.MatchWith(" hello world! ") )
    {
    throw "testFancyStringForStringOperations: failed MatchWith testing (3)";
    }
  std::cout << "testFancyStringForStringOperations: MatchWith(-) OK!" << std::endl;

  s = "Hello World!";
  if ( !s.StartWith("hello") )
    {
    throw "testFancyStringForStringOperations: failed StartWith testing (1)";
    }
  if ( s.StartWith("hello",false) )
    {
    throw "testFancyStringForStringOperations: failed StartWith testing (2)";
    }
  std::cout << "testFancyStringForStringOperations: StartWith(-) OK!" << std::endl;

  s = "Hello World!";
  if ( !s.EndWith("world!") )
    {
    throw "testFancyStringForStringOperations: failed EndWith testing (1)";
    }
  if ( s.EndWith("world!",false) )
    {
    throw "testFancyStringForStringOperations: failed EndWith testing (2)";
    }
  std::cout << "testFancyStringForStringOperations: EndWith(-) OK!" << std::endl;

  s = "Hello World!";
  if ( !s.ContainSub("Lo wo") )
    {
    throw "testFancyStringForStringOperations: failed ContainSub testing (1)";
    }
  if ( s.ContainSub("Lo wo",false) )
    {
    throw "testFancyStringForStringOperations: failed ContainSub testing (2)";
    }
  std::cout << "testFancyStringForStringOperations: ContainSub(-) OK!" << std::endl;

  // all testings were successful if reached here
}
