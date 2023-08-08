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


#include "itkTransformFileWriter.h"
#include "itkTestingMacros.h"

int
itkTransformFileWriterTemplateTest(int, char *[])
{
  using TransformWriterType = itk::TransformFileWriterTemplate<double>;
  auto transformWriter = TransformWriterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(transformWriter, TransformFileWriterTemplate, LightProcessObject);


  auto useCompression = false;
  ITK_SET_GET_BOOLEAN(transformWriter, UseCompression, useCompression);

  auto appendMode = false;
  ITK_SET_GET_BOOLEAN(transformWriter, AppendMode, appendMode);

#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  transformWriter->SetAppendOn();
  auto nonConstAppendMode = transformWriter->GetAppendMode();
  ITK_TEST_EXPECT_TRUE(nonConstAppendMode);

  transformWriter->SetAppendOff();
  nonConstAppendMode = transformWriter->GetAppendMode();
  ITK_TEST_EXPECT_TRUE(!nonConstAppendMode);
#endif

  // trigger empty write exception
  ITK_TRY_EXPECT_EXCEPTION(transformWriter->Update());

  const char * fileName = "transform.garbage";
  transformWriter->SetFileName(fileName);
  ITK_TEST_SET_GET_VALUE(fileName, transformWriter->GetFileName());

  // trigger exception for transformio not found
  ITK_TRY_EXPECT_EXCEPTION(transformWriter->Update());

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
