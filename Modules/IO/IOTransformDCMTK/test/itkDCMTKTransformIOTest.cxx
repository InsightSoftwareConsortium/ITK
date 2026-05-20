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

#include "itkDCMTKTransformIO.h"
#include "itkDCMTKTransformIOFactory.h"
#include "itkTransformFileReader.h"
#include "itkTestingMacros.h"
#include "itkCompositeTransform.h"

int
itkDCMTKTransformIOTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << argv[0] << " transform" << std::endl;
    return EXIT_FAILURE;
  }
  const char * transformFileName = argv[1];

  int testStatus = EXIT_SUCCESS;

  constexpr unsigned int Dimension = 3;
  using ScalarType = float;

  auto dcmtkTransformIOFactory = itk::DCMTKTransformIOFactory::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(dcmtkTransformIOFactory, DCMTKTransformIOFactory, ObjectFactoryBase);
  std::cout << std::endl;
  itk::ObjectFactoryBase::RegisterFactory(dcmtkTransformIOFactory);

  using TransformReaderType = itk::TransformFileReaderTemplate<ScalarType>;
  auto transformReader = TransformReaderType::New();
  transformReader->SetFileName(transformFileName);

  using TransformIOType = itk::DCMTKTransformIO<ScalarType>;
  auto transformIO = TransformIOType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(transformIO, DCMTKTransformIO, TransformIOBaseTemplate);
  transformReader->SetTransformIO(transformIO);

  ITK_TEST_SET_GET_VALUE(std::string{}, transformIO->GetFrameOfReferenceUID());

  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(!transformIO->CanWriteFile(transformFileName), testStatus);
  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(!transformIO->CanReadFile("fileThatDoesNotExist.dcm"), testStatus);
  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(transformIO->CanReadFile(transformFileName), testStatus);

  ITK_TRY_EXPECT_EXCEPTION(transformIO->Write());

  ITK_TRY_EXPECT_NO_EXCEPTION(transformReader->Update());

  using TransformListType = TransformReaderType::TransformListType;
  const TransformListType * const transformList = transformReader->GetTransformList();

  using ReadTransformType = itk::CompositeTransform<ScalarType, Dimension>;
  auto       transformIt = transformList->begin();
  const auto readTransform = dynamic_cast<ReadTransformType *>((*transformIt).GetPointer());
  if (readTransform == nullptr)
  {
    std::cerr << "Did not get the expected transform out." << std::endl;
    return EXIT_FAILURE;
  }
  readTransform->Print(std::cout);
  // Fixture rect-offset-sro-rigid/transform.dcm carries three MatrixRegistration
  // items across two Frame-of-Reference UIDs.
  ITK_TEST_EXPECT_EQUAL_STATUS_VALUE(transformIO->GetReadTransformList().size(), 3u, testStatus);

  const std::string firstFrameUID{ "1.2.826.0.1.3680043.8.274.1.1.8323328.22085.1372783046.709932" };
  transformIO->SetFrameOfReferenceUID(firstFrameUID);
  ITK_TEST_SET_GET_VALUE(firstFrameUID, transformIO->GetFrameOfReferenceUID());
  ITK_TRY_EXPECT_NO_EXCEPTION(transformReader->Update());
  ITK_TEST_EXPECT_EQUAL_STATUS_VALUE(transformIO->GetReadTransformList().size(), 2u, testStatus);

  std::cout << "Test finished." << std::endl;
  return testStatus;
}
