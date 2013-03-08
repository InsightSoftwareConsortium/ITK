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
This program tests the itk::DOMNode::Find() function that searches for a node using a query string.
Make sure that the testing of itk::DOMNode::GetPath() has been performed before doing this type of tests,
because it is used here to verify the correctness of the query output.
*/

#include "itkDOMNodeXMLReader.h"

#include <iostream>
#include "itkMacro.h"

int itkDOMTest4( int argc, char* argv[] )
{
  if ( argc < 4 )
    {
    std::cerr << "arguments expected: <test>.xml <QueryString> <GroundTruthPathString>" << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    // read the DOM object from the input XML file
    itk::DOMNodeXMLReader::Pointer reader = itk::DOMNodeXMLReader::New();
    reader->SetFileName( argv[1] );
    reader->Update();
    itk::DOMNode::Pointer dom = reader->GetModifiableOutput();

    std::string sQueryString = argv[2];
    // itk_add_test has problem to supply an empty string, so we use a special string
    if ( sQueryString == "[]" )
      {
      sQueryString = "";
      }

    std::string sGroundTruthPathString = argv[3];
    // itk_add_test has problem to supply an empty string, so we use a special string
    if ( sGroundTruthPathString == "[]" )
      {
      sGroundTruthPathString = "";
      }

    itk::DOMNode* node = dom->Find( sQueryString );
    if ( !node )
      {
      throw "testing failed";
      }
    std::cout << "node path: " << node->GetPath() << std::endl;
    if ( node->GetPath() != sGroundTruthPathString )
      {
      throw "testing failed";
      }

    // the testing is successful if you reached here
    }
  catch ( const itk::ExceptionObject& eo )
    {
    eo.Print( std::cerr );
    return EXIT_FAILURE;
    }
  catch ( ... )
    {
    std::cerr << "Unknown exception caught!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
