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

    // test passed!
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

    // test passed!
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
    if ( dataIn != dataOut )
      {
      throw "double: input and output data do not match";
      }

    // test passed!
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
  svalue.append( " 10 " );

  // read all data elements in the string
  std::vector<float> dataOut1;
  s = svalue;
  s.ToData( dataOut1 );
  // check successful or not
  if ( dataOut1.size() != (dataIn.size()+1) && dataOut1.back() != 10.0f )
    {
    throw "testWithStdVector: failed reading all elements in the string (1)";
    }
  for ( size_t i = 0; i < dataIn.size(); i++ )
    {
    if ( dataIn[i] != dataOut1[i] )
      {
      throw "testWithStdVector: failed reading all elements in the string (2)";
      }
    }

  // read all data elements for the output vector
  std::vector<float> dataOut2( 5, 0.0f );
  s = svalue;
  s >> dataOut2;
  // check successful or not
  if ( dataOut2.size() != 5 )
    {
    throw "testWithStdVector: failed reading all elements for the output vector (1)";
    }
  for ( size_t i = 0; i < dataOut2.size(); i++ )
    {
    if ( dataIn[i] != dataOut2[i] )
      {
      throw "testWithStdVector: failed reading all elements for the output vector (2)";
      }
    }

  // read user-specified number of data elements (output data exist)
  std::vector<float> dataOut3( 10, 0.0f );
  s = svalue;
  s.ToData( dataOut3, 5 );
  // check successful or not
  if ( dataOut3.size() != 10 && dataOut3[5] != 0.0f )
    {
    throw "testWithStdVector: failed reading user-specified number of elements (1.1)";
    }
  for ( size_t i = 0; i < 5; i++ )
    {
    if ( dataIn[i] != dataOut3[i] )
      {
      throw "testWithStdVector: failed reading user-specified number of elements (1.2)";
      }
    }

  // read user-specified number of data elements (output data do not exist)
  std::vector<float> dataOut4;
  s = svalue;
  s.ToData( dataOut4, 5 );
  // check successful or not
  if ( dataOut4.size() != 5 )
    {
    throw "testWithStdVector: failed reading user-specified number of elements (2.1)";
    }
  for ( size_t i = 0; i < 5; i++ )
    {
    if ( dataIn[i] != dataOut4[i] )
      {
      throw "testWithStdVector: failed reading user-specified number of elements (2.2)";
      }
    }
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
  svalue.append( " 10 " );

  // read all data elements in the string
  DataType dataOut1;
  s = svalue;
  s.ToData( dataOut1 );
  // check successful or not
  if ( dataOut1.GetSize() != (dataIn.GetSize()+1) && dataOut1[10] != 10.0 )
    {
    throw "testWithItkArray: failed reading all elements in the string (1)";
    }
  for ( size_t i = 0; i < dataIn.GetSize(); i++ )
    {
    if ( dataIn[i] != dataOut1[i] )
      {
      throw "testWithItkArray: failed reading all elements in the string (2)";
      }
    }

  // read all data elements for the output vector
  DataType dataOut2( 5 );
  dataOut2.Fill( 0.0 );
  s = svalue;
  s >> dataOut2;
  // check successful or not
  if ( dataOut2.GetSize() != 5 )
    {
    throw "testWithItkArray: failed reading all elements for the output vector (1)";
    }
  for ( size_t i = 0; i < dataOut2.GetSize(); i++ )
    {
    if ( dataIn[i] != dataOut2[i] )
      {
      throw "testWithItkArray: failed reading all elements for the output vector (2)";
      }
    }

  // read user-specified number of data elements (output data exist)
  DataType dataOut3( 10 );
  dataOut3.Fill( 0.0 );
  s = svalue;
  s.ToData( dataOut3, 5 );
  // check successful or not
  if ( dataOut3.GetSize() != 10 && dataOut3[5] != 0.0 )
    {
    throw "testWithItkArray: failed reading user-specified number of elements (1.1)";
    }
  for ( size_t i = 0; i < 5; i++ )
    {
    if ( dataIn[i] != dataOut3[i] )
      {
      throw "testWithItkArray: failed reading user-specified number of elements (1.2)";
      }
    }

  // read user-specified number of data elements (output data do not exist)
  DataType dataOut4;
  s = svalue;
  s.ToData( dataOut4, 5 );
  // check successful or not
  if ( dataOut4.GetSize() != 5 )
    {
    throw "testWithItkArray: failed reading user-specified number of elements (2.1)";
    }
  for ( size_t i = 0; i < 5; i++ )
    {
    if ( dataIn[i] != dataOut4[i] )
      {
      throw "testWithItkArray: failed reading user-specified number of elements (2.2)";
      }
    }
}

void testFancyStringForStringOperations()
{
  itk::FancyString s;

  s = " Hello World! ";
  if ( s.TrimLeft() != "Hello World! " )
    {
    throw "testStringOperations: failed trimming left";
    }

  s = " Hello World! ";
  if ( s.TrimRight() != " Hello World!" )
    {
    throw "testStringOperations: failed trimming right";
    }

  s = " Hello World! ";
  if ( s.Trim() != "Hello World!" )
    {
    throw "testStringOperations: failed trimming both sides";
    }

  s = "Hello World!";
  if ( s.ToUpperCase() != "HELLO WORLD!" )
    {
    throw "testStringOperations: failed converting to upper case";
    }

  s = "Hello World!";
  if ( s.ToLowerCase() != "hello world!" )
    {
    throw "testStringOperations: failed converting to lower case";
    }

  s = " origin = 0 0 0 ";
  std::string lpart;
  std::string rpart;
  s.Split( lpart, rpart );
  if ( lpart != "origin" || rpart != "0 0 0" )
    {
    throw "testStringOperations: failed splitting into two parts";
    }

  s = " size = 256 256 100; spacing = 0.3 0.3 0.7; origin = * ";
  std::vector<std::string> parts;
  s.Split( parts );
  if ( parts[0] != "size = 256 256 100" || parts[1] != "spacing = 0.3 0.3 0.7" || parts[2] != "origin = *" )
    {
    throw "testStringOperations: failed splitting into a sequence of strings";
    }

  s = " size = 256 256 100; spacing = 0.3 0.3 0.7; origin = * ";
  std::map<std::string,std::string> items;
  s.Split( items );
  if ( items["size"] != "256 256 100" || items["spacing"] != "0.3 0.3 0.7" || items["origin"] != "*" )
    {
    throw "testStringOperations: failed splitting into a map or dictionary";
    }

  s = "Hello World!";
  if ( !s.MatchWith("hello world!") )
    {
    throw "testStringOperations: failed MatchWith testing (1)";
    }
  if ( s.MatchWith("hello world!",false) )
    {
    throw "testStringOperations: failed MatchWith testing (2)";
    }
  if ( s.MatchWith(" hello world! ") )
    {
    throw "testStringOperations: failed MatchWith testing (3)";
    }

  s = "Hello World!";
  if ( !s.StartWith("hello") )
    {
    throw "testStringOperations: failed StartWith testing (1)";
    }
  if ( s.StartWith("hello",false) )
    {
    throw "testStringOperations: failed StartWith testing (2)";
    }

  s = "Hello World!";
  if ( !s.EndWith("world!") )
    {
    throw "testStringOperations: failed EndWith testing (1)";
    }
  if ( s.EndWith("world!",false) )
    {
    throw "testStringOperations: failed EndWith testing (2)";
    }

  s = "Hello World!";
  if ( !s.ContainSub("Lo wo") )
    {
    throw "testStringOperations: failed ContainSub testing (1)";
    }
  if ( s.ContainSub("Lo wo",false) )
    {
    throw "testStringOperations: failed ContainSub testing (2)";
    }

  // all testings passed!
}
