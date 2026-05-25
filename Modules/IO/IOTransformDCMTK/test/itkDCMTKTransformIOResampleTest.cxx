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
#include "itkImageSeriesReader.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkCompositeTransform.h"
#include "itkImageFileWriter.h"
#include "itkMetaDataObject.h"
#include "itkResampleImageFilter.h"
#include "itkTestingMacros.h"


int
itkDCMTKTransformIOResampleTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Usage: " << argv[0] << " fixedSeriesDirectory movingSeriesDirectory"
              << " transform fixedImageOutput resampledMovingOutput" << std::endl;
    return EXIT_FAILURE;
  }
  const char * fixedSeriesDirectory = argv[1];
  const char * movingSeriesDirectory = argv[2];
  const char * transformFileName = argv[3];
  const char * fixedImageOutputFileName = argv[4];
  const char * resampledMovingOutputFileName = argv[5];

  int testStatus = EXIT_SUCCESS;

  constexpr unsigned int Dimension = 3;
  using PixelType = short;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ScalarType = float;

  using ReaderType = itk::ImageSeriesReader<ImageType>;
  using ImageIOType = itk::GDCMImageIO;
  using SeriesFileNamesType = itk::GDCMSeriesFileNames;
  using FileNamesContainerType = SeriesFileNamesType::FileNamesContainerType;

  auto fixedReader = ReaderType::New();
  auto fixedIO = ImageIOType::New();
  fixedReader->SetImageIO(fixedIO);

  auto fixedSeriesFileNames = SeriesFileNamesType::New();
  fixedSeriesFileNames->SetInputDirectory(fixedSeriesDirectory);
  const FileNamesContainerType & fixedFileNames = fixedSeriesFileNames->GetInputFileNames();
  std::cout << "There are " << fixedFileNames.size() << " fixed image slices." << std::endl;
  std::cout << "First fixed images series UID: " << fixedSeriesFileNames->GetSeriesUIDs()[0] << '\n' << std::endl;
  fixedReader->SetFileNames(fixedFileNames);

  auto movingReader = ReaderType::New();
  auto movingIO = ImageIOType::New();
  movingReader->SetImageIO(movingIO);

  auto movingSeriesFileNames = SeriesFileNamesType::New();
  movingSeriesFileNames->SetInputDirectory(movingSeriesDirectory);
  const FileNamesContainerType & movingFileNames = movingSeriesFileNames->GetInputFileNames();
  std::cout << "There are " << movingFileNames.size() << " moving image slices." << std::endl;
  std::cout << "First moving images series UID: " << movingSeriesFileNames->GetSeriesUIDs()[0] << '\n' << std::endl;
  movingReader->SetFileNames(movingFileNames);

  ITK_TRY_EXPECT_NO_EXCEPTION(fixedReader->Update());
  ITK_TRY_EXPECT_NO_EXCEPTION(movingReader->Update());

  auto dcmtkTransformIOFactory = itk::DCMTKTransformIOFactory::New();
  itk::ObjectFactoryBase::RegisterFactory(dcmtkTransformIOFactory);

  using TransformReaderType = itk::TransformFileReaderTemplate<ScalarType>;
  auto transformReader = TransformReaderType::New();
  transformReader->SetFileName(transformFileName);

  using TransformIOType = itk::DCMTKTransformIO<ScalarType>;
  auto transformIO = TransformIOType::New();
  transformReader->SetTransformIO(transformIO);

  using TransformListType = TransformReaderType::TransformListType;
  using ReadTransformType = itk::CompositeTransform<ScalarType, Dimension>;

  // Fixed image transform
  const ReaderType::DictionaryType & fixedMetaDataDict = fixedIO->GetMetaDataDictionary();
  std::string                        fixedFrameOfReferenceUID;
  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(
    itk::ExposeMetaData<std::string>(fixedMetaDataDict, "0020|0052", fixedFrameOfReferenceUID), testStatus);
  std::cout << "Fixed image frame of reference UID: " << fixedFrameOfReferenceUID << std::endl;
  transformIO->SetFrameOfReferenceUID(fixedFrameOfReferenceUID);

  ITK_TRY_EXPECT_NO_EXCEPTION(transformReader->Update());
  const TransformListType *        transformList = transformReader->GetTransformList();
  auto                             transformIt = transformList->begin();
  const ReadTransformType::Pointer fixedTransform = dynamic_cast<ReadTransformType *>((*transformIt).GetPointer());
  if (fixedTransform.IsNull())
  {
    std::cerr << "Did not get the expected fixed transform out." << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "Fixed transform: " << fixedTransform << std::endl;

  // Moving image transform
  const ReaderType::DictionaryType & movingMetaDataDict = movingIO->GetMetaDataDictionary();
  std::string                        movingFrameOfReferenceUID;
  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(
    itk::ExposeMetaData<std::string>(movingMetaDataDict, "0020|0052", movingFrameOfReferenceUID), testStatus);
  std::cout << "Moving image frame of reference UID: " << movingFrameOfReferenceUID << std::endl;
  transformIO->SetFrameOfReferenceUID(movingFrameOfReferenceUID);

  ITK_TRY_EXPECT_NO_EXCEPTION(transformReader->Update());
  transformList = transformReader->GetTransformList();
  transformIt = transformList->begin();
  const ReadTransformType::Pointer movingTransform = dynamic_cast<ReadTransformType *>((*transformIt).GetPointer());
  if (movingTransform.IsNull())
  {
    std::cerr << "Did not get the expected moving transform out." << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "Moving transform: " << movingTransform << std::endl;

  // Compose the transform from the fixed to the moving image
  auto movingTransformInverse = ReadTransformType::New();
  movingTransform->GetInverse(movingTransformInverse);

  auto fixedToMovingTransform = ReadTransformType::New();
  fixedToMovingTransform->AddTransform(fixedTransform);
  fixedToMovingTransform->AddTransform(movingTransformInverse);
  fixedToMovingTransform->FlattenTransformQueue();

  using ResamplerType = itk::ResampleImageFilter<ImageType, ImageType, ScalarType, ScalarType>;
  auto resampler = ResamplerType::New();
  resampler->SetInput(movingReader->GetOutput());
  resampler->SetUseReferenceImage(true);
  resampler->SetReferenceImage(fixedReader->GetOutput());
  resampler->SetTransform(fixedToMovingTransform);
  resampler->SetDefaultPixelValue(-1000);

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(fixedImageOutputFileName);
  writer->SetInput(fixedReader->GetOutput());
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  writer->SetInput(resampler->GetOutput());
  writer->SetFileName(resampledMovingOutputFileName);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Test finished." << std::endl;
  return testStatus;
}
