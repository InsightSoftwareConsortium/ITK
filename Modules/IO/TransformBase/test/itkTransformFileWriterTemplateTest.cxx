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


#include "itkTransformFileWriter.h"

int itkTransformFileWriterTemplateTest( int argc, char *argv[] )
{
  if( argc < 1 )
    {
    std::cerr << "Usage:" << argv[0];
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::TransformFileWriterTemplate<double>      TransformWriterType;


  TransformWriterType::Pointer transformWriter = TransformWriterType::New();

  std::cout << "Writer class = "
            << transformWriter->GetNameOfClass()
            << "Writer base = "
            << dynamic_cast<TransformWriterType::Superclass *>(transformWriter.GetPointer())->GetNameOfClass()
            << std::endl;

  // trigger empty write exception
  TRY_EXPECT_EXCEPTION( transformWriter->Update() );

  transformWriter->SetFileName("transform.garbage");
  // trigger exception for transformio not found
  TRY_EXPECT_EXCEPTION( transformWriter->Update() );

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
