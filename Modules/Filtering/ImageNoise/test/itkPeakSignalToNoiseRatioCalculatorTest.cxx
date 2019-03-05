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
#include "itkImageFileWriter.h"

#include "itkPeakSignalToNoiseRatioCalculator.h"
#include "itkTestingMacros.h"

int itkPeakSignalToNoiseRatioCalculatorTest(int argc, char * argv[])
{

  if( argc < 3 )
    {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv) << " intput noisy [expectedValue tolerance]" << std::endl;
    std::cerr << " input: the input image" << std::endl;
    std::cerr << " noisy: noise with the input image" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  constexpr int dim = 2;

  using PType = unsigned char;
  using IType = itk::Image< PType, dim >;

  using ReaderType = itk::ImageFileReader< IType >;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );
  reader2->Update();

  using CalculatorType = itk::PeakSignalToNoiseRatioCalculator< IType >;
  CalculatorType::Pointer psnr = CalculatorType::New();
  psnr->SetImage( reader->GetOutput() );
  psnr->SetNoisyImage( reader2->GetOutput() );
  psnr->Compute();

  if (argc >= 5)
    {
    double expectedValue = std::stod(argv[3]);
    double tolerance = std::stod(argv[4]);

    std::cout << "<DartMeasurement name=\"PSNR\" type=\"numeric/double\">";
    std::cout <<  psnr->GetOutput();
    std::cout <<  "</DartMeasurement>" << std::endl;

    if ( psnr->GetOutput() - expectedValue > tolerance )
      {
      return EXIT_FAILURE;
      }
    }

  return 0;
}
