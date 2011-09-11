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
#include <iostream>

#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
#include "itkCompositeTransformWriter.h"
#include "itkCompositeTransformReader.h"
#include "itkArray2D.h"


/* A simple test for CompositeTransformReader on its own. See
 * CompositeTransformWriterAndReaderTest for a test that checks
 * actual writing and then reading for errors. */

int itkCompositeTransformReaderTest(int , char *[] )
{
  const unsigned int NDimensions = 2;

  typedef itk::CompositeTransformReader<double, NDimensions> ReaderType;
  ReaderType::Pointer                   reader = ReaderType::New();

  /* Check for error if try to read w/out setting filename */
  bool caughtException = false;
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    caughtException = true;
    std::cout << "Exception caught as expected:" << std::endl;
    std::cout << ex << std::endl;
    }
  if( !caughtException )
    {
    std::cout << "No error generated for no filename." << std::endl;
    return EXIT_FAILURE;
    }

  /* Try with bad filename */
  try
    {
    reader->SetFileName( "junkfile!@#$%^&*()" );
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << "Unexpected Exception caught:" << std::endl;
    std::cout << ex << std::endl;
    }

  caughtException = false;
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    caughtException = true;
    std::cout << "Exception caught as expected:" << std::endl;
    std::cout << ex << std::endl;
    }
  if( !caughtException )
    {
    std::cout << "No error generated for bad filename." << std::endl;
    return EXIT_FAILURE;
    }

  //Print
  std::cout << reader;

  return EXIT_SUCCESS;
}
