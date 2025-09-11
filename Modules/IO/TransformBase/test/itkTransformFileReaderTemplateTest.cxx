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


#include "itkTransformFileReader.h"
#include "itkTestingMacros.h"

int
itkTransformFileReaderTemplateTest(int, char *[])
{
  using TransformReaderType = itk::TransformFileReaderTemplate<double>;

  auto transformReader = TransformReaderType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(transformReader, TransformFileReaderTemplate, LightProcessObject);


  // trigger empty read exception
  ITK_TRY_EXPECT_EXCEPTION(transformReader->Update());


  const char * fileName = "transform.garbage";
  transformReader->SetFileName(fileName);
  ITK_TEST_SET_GET_VALUE(fileName, transformReader->GetFileName());

  // trigger exception for transformIO not found
  ITK_TRY_EXPECT_EXCEPTION(transformReader->Update());

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
