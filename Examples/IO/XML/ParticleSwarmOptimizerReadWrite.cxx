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

/**
 * This example program shows how to reade/write the ITK object itk::ParticleSwarmOptimizer using
 * the traditional SAX-based API and the newly introduced DOM-based API.
 *
 * The program reads a PSO XML file (arg1) using both DOM-based and SAX-based readers, then
 * writes out the loaded PSO object using both DOM-based (arg2) and SAX-based (arg3) writers.
 *
 * Please see [ITK_HOME]/Testing/Data/InputXML/test.pso.xml for an example XML file for the PSO object.
 */

#include "itkParticleSwarmOptimizer.h"

#include "itkParticleSwarmOptimizerSAXReader.h"
#include "itkParticleSwarmOptimizerSAXWriter.h"

#include "itkParticleSwarmOptimizerDOMReader.h"
#include "itkParticleSwarmOptimizerDOMWriter.h"

#include <iostream>
#include "itkMacro.h"

int main( int argc, char* argv[] )
{
  if ( argc < 4 )
    {
    std::cerr << "arguments expected: test-input.pso.xml test-output-DOM.pso.xml test-output-SAX.pso.xml" << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    // use DOM reader/writer
    {
    itk::ParticleSwarmOptimizer::Pointer optimizer;
    // read the optimizer from an XML file
    itk::ParticleSwarmOptimizerDOMReader::Pointer reader = itk::ParticleSwarmOptimizerDOMReader::New();
    reader->SetFileName( argv[1] );
    reader->Update();
    optimizer = reader->GetOutput();
    // write a DOM object to an XML file
    itk::ParticleSwarmOptimizerDOMWriter::Pointer writer = itk::ParticleSwarmOptimizerDOMWriter::New();
    writer->SetInput( optimizer );
    writer->SetFileName( argv[2] );
    writer->Update();
    }

    // use SAX reader/writer
    {
    itk::ParticleSwarmOptimizer::Pointer optimizer = itk::ParticleSwarmOptimizer::New();
    // read the optimizer from an XML file
    itk::ParticleSwarmOptimizerSAXReader::Pointer reader = itk::ParticleSwarmOptimizerSAXReader::New();
    reader->SetOutputObject( optimizer ); // method defined in itk::XMLReader<T>
    reader->SetFilename( argv[1] ); // method defined in itk::XMLReaderBase
    reader->ReadFile();
    // write a DOM object to an XML file
    itk::ParticleSwarmOptimizerSAXWriter::Pointer writer = itk::ParticleSwarmOptimizerSAXWriter::New();
    writer->SetObject( optimizer ); // method defined in itk::XMLWriterBase
    writer->SetFilename( argv[3] ); // method defined in itk::XMLWriterBase
    writer->WriteFile();
    }
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
