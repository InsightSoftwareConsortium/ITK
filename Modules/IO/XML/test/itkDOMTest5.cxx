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
This program tests the itk::DOMReader and itk::DOMWriter update functions,
especially the change of working directory for reading/writing external files in XML from the correct directory.
*/

#include "itkDOMTestObjectDOMReader.h"
#include "itkDOMTestObjectDOMWriter.h"

#include <iostream>
#include "itkMacro.h"

int itkDOMTest5( int argc, char* argv[] )
{
  if ( argc < 2 )
    {
    std::cerr << "arguments expected: <output>.DOMTestObject.xml" << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    std::cout << "input file for w/r: " << argv[1] << std::endl;
    std::cout << std::endl;

    // create the test object
    itk::DOMTestObject::Pointer testobj1 = itk::DOMTestObject::New();
    // settings for foo
    testobj1->SetFooValue( "Hello!" );
    testobj1->SetFooFileName( "itkDOMTest5-output/foo.txt" );

    std::cout << "foo value: " << testobj1->GetFooValue() << std::endl;
    std::cout << "foo file : " << testobj1->GetFooFileName() << std::endl;
    std::cout << std::endl;

    // write the test object to an XML file
    itk::DOMTestObjectDOMWriter::Pointer writer = itk::DOMTestObjectDOMWriter::New();
    writer->SetInput( testobj1 );
    writer->SetFileName( argv[1] );
    writer->Update();

    itk::DOMTestObject::Pointer testobj2;

    // read the object back to memory from the disk
    itk::DOMTestObjectDOMReader::Pointer reader = itk::DOMTestObjectDOMReader::New();
    reader->SetFileName( argv[1] );
    reader->Update();
    testobj2 = reader->GetOutput();

    // check whether the two test objects have the same values
    std::cout << "returned foo value: " << testobj2->GetFooValue() << std::endl;
    std::cout << std::endl;
    if ( testobj1->GetFooValue() != testobj2->GetFooValue() )
      {
      throw "foo values mismatch";
      }

    // testing is successful if reached here
    }
  catch ( const itk::ExceptionObject& eo )
    {
    eo.Print( std::cerr );
    return EXIT_FAILURE;
    }
  catch ( const char* e )
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  catch ( ... )
    {
    std::cerr << "Unknown exception caught!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
