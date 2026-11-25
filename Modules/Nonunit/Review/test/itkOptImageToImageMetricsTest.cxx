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
#include "itkTranslationTransform.h"
#include "itkRigid2DTransform.h"
#include "itkImageFileReader.h"

#include "itkLinearInterpolateImageFunction.h"

#include "itkMeanSquaresImageToImageMetric.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkMutualInformationImageToImageMetric.h"

#include "itkOptImageToImageMetricsTest.h"

int
itkOptImageToImageMetricsTest(int, char * argv[])
{
  std::cout << "OPTIMIZED ON" << std::endl;
  std::cout << "Default number of threads : " << itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads() << std::endl;

  using FixedImageType = itk::Image<unsigned int>;
  using MovingImageType = itk::Image<unsigned int>;

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);


  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  //  First run the experiments with the default number of threads,
  //  as set from the command line arguments, the system defaults
  //  or the ITK environment variable:
  //  ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << "Now Running tests with : " << std::endl;
  std::cout << "\t Global Default Number of Threads " << itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads()
            << std::endl;
  std::cout << "\t Global Maximum Number of Threads " << itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads()
            << std::endl;
  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << std::endl;


  itk::TranslationLinearTest<FixedImageReaderType, MovingImageReaderType>(fixedImageReader,
                                                                          movingImageReader.GetPointer());

  itk::RigidLinearTest<FixedImageReaderType, MovingImageReaderType>(fixedImageReader, movingImageReader.GetPointer());

  itk::AffineLinearTest<FixedImageReaderType, MovingImageReaderType>(fixedImageReader, movingImageReader.GetPointer());

  std::cout << "OPTIMIZED ON" << std::endl;

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  // Then, do experiments with number of threads set to 1!!!!!!!!!!
  // In this way we can compare the results with N threads versus 1.
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << "Now Running tests with : " << std::endl;
  std::cout << "\t itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(1); " << std::endl;
  std::cout << "\t itk::MultiThreaderBase::SetGlobalMaximumNumberOfThreads(1); " << std::endl;
  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << std::endl;

  itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(1);
  itk::MultiThreaderBase::SetGlobalMaximumNumberOfThreads(1);

  itk::TranslationLinearTest<FixedImageReaderType, MovingImageReaderType>(fixedImageReader,
                                                                          movingImageReader.GetPointer());

  itk::RigidLinearTest<FixedImageReaderType, MovingImageReaderType>(fixedImageReader, movingImageReader.GetPointer());

  itk::AffineLinearTest<FixedImageReaderType, MovingImageReaderType>(fixedImageReader, movingImageReader.GetPointer());

  std::cout << "OPTIMIZED ON" << std::endl;
  return EXIT_SUCCESS;
}
