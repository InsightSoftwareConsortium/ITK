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
#include "itkImageFileReader.h"

#include "itkLinearInterpolateImageFunction.h"

#include "itkMeanSquaresImageToImageMetric.h"

#include "itkOptImageToImageMetricsTest2.h"

int itkOptImageToImageMetricsTest2(int , char* argv[])
{
  std::cout << "OPTIMIZED ON" << std::endl;

  std::cout << "Default number of threads : "
            << itk::MultiThreader::GetGlobalDefaultNumberOfThreads()
            << std::endl;

  typedef itk::Image< unsigned int > FixedImageType;
  typedef itk::Image< unsigned int > MovingImageType;

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  //  First run the experiments with the default number of threads,
  //  as set from the command line arguments, the system defaults
  //  or the ITK environment variable:
  //  ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << "Now Running tests with : " << std::endl;
  std::cout << "\t Global Default Number of Threads " << itk::MultiThreader::GetGlobalDefaultNumberOfThreads() << std::endl;
  std::cout << "\t Global Maximum Number of Threads " << itk::MultiThreader::GetGlobalMaximumNumberOfThreads() << std::endl;
  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << std::endl;

  itk::BSplineLinearTest( fixedImageReader.GetPointer(),
                       movingImageReader.GetPointer() );

  return(EXIT_SUCCESS);
}
