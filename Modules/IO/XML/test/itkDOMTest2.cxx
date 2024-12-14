/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkTestingMacros.h"

int
itkDOMTest2(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " input.xml output.xml" << '\n';
    return EXIT_FAILURE;
  }

  try
  {
    // read a DOM object from an XML file
    const itk::DOMNodeXMLReader::Pointer reader = itk::DOMNodeXMLReader::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(reader, DOMNodeXMLReader, Object);


    const std::string inputFileName = argv[1];
    reader->SetFileName(inputFileName);
    ITK_TEST_SET_GET_VALUE(inputFileName, reader->GetFileName());

    reader->Update();
    const itk::DOMNode::Pointer dom = reader->GetOutput();

    // write a DOM object to an XML file
    const itk::DOMNodeXMLWriter::Pointer writer = itk::DOMNodeXMLWriter::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(writer, DOMNodeXMLWriter, Object);


    writer->SetInput(dom);
    ITK_TEST_SET_GET_VALUE(dom, writer->GetInput());

    const std::string outputFileName = argv[2];
    writer->SetFileName(outputFileName);
    ITK_TEST_SET_GET_VALUE(outputFileName, writer->GetFileName());

    writer->Update();

    // write a DOM object to an XML stream
    const itk::DOMNode::Pointer dom1 = dom;
    std::ostringstream          oss;
    oss << *dom1;
    const std::string s = oss.str();
    std::cout << "Write DOM object to an output string stream: " << '\n';
    std::cout << s << '\n';

    // read a DOM object from an XML stream
    const itk::DOMNode::Pointer dom2 = itk::DOMNode::New();
    std::istringstream          iss(s);
    iss >> *dom2;
    std::cout << "Read DOM object from an input string stream: " << '\n';
    std::cout << *dom2 << '\n';
  }
  catch (const itk::ExceptionObject & eo)
  {
    eo.Print(std::cerr);
    return EXIT_FAILURE;
  }
  catch (...)
  {
    std::cerr << "Unknown exception caught!" << '\n';
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
