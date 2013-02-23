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
This program tests the functions of itk::DOMNodeXMLReader and itk::DOMNodeXMLWriter.
It also demonstrates
- how to read a DOM object from an XML file
- how to write a DOM object to an XML file
- how to read a DOM object from an XML stream
- how to write a DOM object to an XML stream
*/

#include "itkDOMNodeXMLReader.h"
#include "itkDOMNodeXMLWriter.h"

#include <iostream>
#include "itkMacro.h"

int itkDOMTest2( int argc, char* argv[] )
{
  if ( argc < 3 )
    {
    std::cerr << "arguments expected: input.xml output.xml" << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    // read a DOM object from an XML file
    itk::DOMNodeXMLReader::Pointer reader = itk::DOMNodeXMLReader::New();
    reader->SetFileName( argv[1] );
    reader->Update();
    itk::DOMNode::Pointer dom = reader->GetModifiableOutput();

    // write a DOM object to an XML file
    itk::DOMNodeXMLWriter::Pointer writer = itk::DOMNodeXMLWriter::New();
    writer->SetInput( dom );
    writer->SetFileName( argv[2] );
    writer->Update();

    // write a DOM object to an XML stream
    itk::DOMNode::Pointer dom1 = dom;
    std::ostringstream oss;
    oss << *dom1;
    std::string s = oss.str();
    std::cout << "Write DOM object to an output string stream: " << std::endl;
    std::cout << s << std::endl;

    // read a DOM object from an XML stream
    itk::DOMNode::Pointer dom2 = itk::DOMNode::New();
    std::istringstream iss( s );
    iss >> *dom2;
    std::cout << "Read DOM object from an input string stream: " << std::endl;
    std::cout << *dom2 << std::endl;
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
