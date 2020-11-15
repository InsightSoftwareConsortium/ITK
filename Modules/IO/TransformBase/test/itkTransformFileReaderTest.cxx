/*=========================================================================
 *
 *  Copyright NumFOCUS
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


#include "itkTransformFileReader.h"
#include "itkTestingMacros.h"

int
itkTransformFileReaderTest(int, char *[])
{
  using TransformReaderType = itk::TransformFileReader;

  TransformReaderType::Pointer transformReader = TransformReaderType::New();

  std::cout << "Reader class = " << transformReader->GetNameOfClass() << "Reader base = "
            << dynamic_cast<TransformReaderType::Superclass *>(transformReader.GetPointer())->GetNameOfClass()
            << std::endl;

  try
  {
    // trigger empty read exception
    transformReader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Expected exception (no filename)" << std::endl << excp << std::endl;
  }
  transformReader->SetFileName("transform.garbage");
  try
  {
    // trigger exception for transformio not found
    transformReader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Expected exception (no transformio that can read garbage and no transformio should be registered)"
              << excp << std::endl;
  }

  std::cout << "Test PASSED!" << std::endl;

  return EXIT_SUCCESS;
}
