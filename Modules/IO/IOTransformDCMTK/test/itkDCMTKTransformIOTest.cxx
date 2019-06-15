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

  constexpr unsigned int Dimension = 3;
  using ScalarType = float;

  itk::DCMTKTransformIOFactory::Pointer dcmtkTransformIOFactory = itk::DCMTKTransformIOFactory::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(dcmtkTransformIOFactory, DCMTKTransformIOFactory, ObjectFactoryBase);
  std::cout << std::endl;
  itk::ObjectFactoryBase::RegisterFactory(dcmtkTransformIOFactory);

  using TransformReaderType = itk::TransformFileReaderTemplate<ScalarType>;
  TransformReaderType::Pointer transformReader = TransformReaderType::New();
  transformReader->SetFileName(transformFileName);

  using TransformIOType = itk::DCMTKTransformIO<ScalarType>;
  TransformIOType::Pointer transformIO = TransformIOType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(transformIO, DCMTKTransformIO, TransformIOBaseTemplate);
  transformReader->SetTransformIO(transformIO);

  ITK_TEST_EXPECT_TRUE(!transformIO->CanWriteFile(transformFileName));

  ITK_TEST_EXPECT_TRUE(!transformIO->CanReadFile("fileThatDoesNotExist.dcm"));

  ITK_TEST_EXPECT_TRUE(transformIO->CanReadFile(transformFileName));

  ITK_TRY_EXPECT_EXCEPTION(transformIO->Write());

  ITK_TRY_EXPECT_NO_EXCEPTION(transformReader->Update());

  using TransformListType = TransformReaderType::TransformListType;
  const TransformListType * const transformList = transformReader->GetTransformList();

  using ReadTransformType = itk::CompositeTransform<ScalarType, Dimension>;
  TransformListType::const_iterator transformIt = transformList->begin();
  ReadTransformType::Pointer        readTransform = dynamic_cast<ReadTransformType *>((*transformIt).GetPointer());
  if (readTransform.IsNull())
  {
    std::cerr << "Did not get the expected transform out." << std::endl;
    return EXIT_FAILURE;
  }
  readTransform->Print(std::cout);
  ITK_TEST_EXPECT_EQUAL(3, transformIO->GetReadTransformList().size());

  transformIO->SetFrameOfReferenceUID("1.2.826.0.1.3680043.8.274.1.1.8323328.22085.1372783046.709932");
  ITK_TRY_EXPECT_NO_EXCEPTION(transformReader->Update());
  ITK_TEST_EXPECT_EQUAL(2, transformIO->GetReadTransformList().size());

  return EXIT_SUCCESS;
}
