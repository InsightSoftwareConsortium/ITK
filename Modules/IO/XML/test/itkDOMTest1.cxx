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
This program tests operations on a DOM node.
*/

#include "itkDOMNode.h"
#include "itkDOMNodeXMLReader.h"
#include "itkDOMNodeXMLWriter.h"

#include <iostream>
#include "itkMacro.h"

int itkDOMTest1( int, char*[] )
{
  try
    {
    // create a DOM object
    itk::DOMNode::Pointer dom = itk::DOMNode::New();
    dom->SetName( "SimpleTestObject" );

    // add some attributes
    dom->SetAttribute( "weight", "10 kg" );
    dom->SetAttribute( "owner", "ITK" );

    // add some children
    // 1st child
    itk::DOMNode::Pointer child1 = itk::DOMNode::New();
    child1->SetName( "city" );
    child1->SetAttribute( "name", "New York" );
    dom->AddChild( child1 );
    // 2nd child
    itk::DOMNode::Pointer child2 = itk::DOMNode::New();
    child2->SetName( "city" );
    child2->SetAttribute( "id", "dc" );
    child2->SetAttribute( "name", "District of Columbia" );
    dom->AddChild( child2 );
    // 3rd child
    itk::DOMNode::Pointer child3 = itk::DOMNode::New();
    child3->SetName( "country" );
    child3->SetAttribute( "id", "usa" );
    child3->AddTextChild( "United States of America" );
    dom->AddChildAtEnd( child3 );

    // display the created DOM object
    std::cout << "DOM object created: " << std::endl;
    std::cout << *dom << std::endl;

    // delete/modify some attributes from the children
    child2->RemoveAttribute( "name" );
    child3->SetAttribute( "id", "United States of America" );

    // delete some children
    child3->RemoveChild();
    dom->RemoveChild( 1 );

    // display the modified DOM object
    std::cout << "DOM object modified: " << std::endl;
    std::cout << *dom << std::endl;
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
