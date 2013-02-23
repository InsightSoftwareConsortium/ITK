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
itkDOMTest4 depends on function itk::DOMNode::GetPath() to check whether a returned
node is correct or not, so we need to make sure that GetPath() is working correctly. This

program validates the GetPath() function using the following XML file:
<ITK>/Testing/Data/Input/XML/test.xml,
so it is important to supply the correct input during the testing process.
*/

#include "itkDOMNodeXMLReader.h"

#include <iostream>
#include "itkMacro.h"

int itkDOMTest3( int argc, char* argv[] )
{
  if ( argc < 2 )
    {
    std::cerr << "arguments expected: test.xml" << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    // read the DOM object from the input XML file
    itk::DOMNodeXMLReader::Pointer reader = itk::DOMNodeXMLReader::New();
    reader->SetFileName( argv[1] );
    reader->Update();
    itk::DOMNode::Pointer dom = reader->GetModifiableOutput();

    itk::DOMNode* node = dom->GetChild(0);
    if ( !node )
      {
      throw "testing failed";
      }
    std::cout << "node path: " << node->GetPath() << std::endl;
    if ( node->GetPath() != "/0" )
      {
      throw "testing failed";
      }

    node = dom->GetChild(1);
    if ( !node )
      {
      throw "testing failed";
      }
    node = node->GetChild(5);
    if ( !node )
      {
      throw "testing failed";
      }
    std::cout << "node path: " << node->GetPath() << std::endl;
    if ( node->GetPath() != "/1/5" )
      {
      throw "testing failed";
      }

    node = dom->GetChild(2);
    if ( !node )
      {
      throw "testing failed";
      }
    std::cout << "node path: " << node->GetPath() << std::endl;
    if ( node->GetPath() != "/2" )
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
