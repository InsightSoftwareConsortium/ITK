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
 * This interactive example program demonstrates:
 * - how to read a DOM object from an XML file;
 * - how to write a DOM object to an XML file; and
 * - how to use the Find("QueryString") function to get the nodes of interest by interactively navigating through the XML document.
 *
 * The Find() function finds a child or sibling or relative using a query string or path.
 * A QueryString consists of multiple following items that are separated by '/':
 *     -[n]           : an older sibling by distance 1 (when omitted) or n
 *     +[n]           : a younger sibling by distance 1 (when omitted) or n
 *     n              : a child at index n
 *     <tag>[:n]      : a child at index 0 (when omitted) or n after filtering children with a tag name
 *     ![:n]          : a child at index 0 (when omitted) or n within all text children
 *     :<id>          : a child by id
 *     .              : current node
 *     ..             : parent node
 *     /<rpath>       : absolute path (denote apath), search from the root
 *
 * An example XML file has been provided for this testing, which can be found at [ITK_HOME]/Testing/Data/InputXML/test.xml.
 */

#include "itkDOMNodeXMLReader.h"
#include "itkDOMNodeXMLWriter.h"

#include <iostream>
#include "itkMacro.h"

int main( int argc, char* argv[] )
{
  if ( argc < 3 )
    {
    std::cerr << "arguments expected: test.xml test-slice.xml" << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    // read a DOM object from an XML file
    itk::DOMNodeXMLReader::Pointer reader = itk::DOMNodeXMLReader::New();
    reader->SetFileName( argv[1] );
    reader->Update();
    itk::DOMNode::Pointer dom = reader->GetModifiableOutput();

    // the following code demonstrates the DOM function Find("QueryString");
    // it navigates through the loaded XML document by typing a query string on the console and
    // displaying the returned result.
    itk::DOMNode::Pointer dom1 = dom;
    std::string query = "";
    do
      {
      std::cout << "query = \"" << query << "\"" << std::endl;
      itk::DOMNode::Pointer dom2 = dom1->Find( query );
      if ( (itk::DOMNode*)dom2 == 0 )
        {
        std::cout << "invalid query!" << std::endl;
        }
      else
        {
        dom1 = dom2;
        std::cout << "tag : " << dom1->GetName() << std::endl;
        std::cout << "path: " << dom1->GetPath() << std::endl;
        std::cout << *dom1 << std::endl;
        }
      std::cout << std::endl << "type a query (... to quit) > ";
      std::cin >> query;
      }
    while ( query != "..." );

    // write the sliced child DOM object to an XML file
    itk::DOMNodeXMLWriter::Pointer writer = itk::DOMNodeXMLWriter::New();
    writer->SetInput( dom1 );
    writer->SetFileName( argv[2] );
    writer->Update();
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
