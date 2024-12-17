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

#include "itkImage.h"
#include <iostream>

#include "itkMINCImageIO.h"
#include "itkMINCImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itksys/SystemTools.hxx"

#include "itkImageMomentsCalculator.h"
#include "itkStdStreamStateSave.h"
#include "itkTestingMacros.h"
template <typename ImageType>
int
test_image_moments(const char * input_image,
                   const char * output_image,
                   double       total,
                   double       mx,
                   double       my,
                   double       mz,
                   double       epsilon,
                   bool         ras_to_lps)
{
  if (ras_to_lps)
  { // need to flip expected moments to match flipped coordinate system
    mx = -mx;
    my = -my;
  }

  itk::MINCImageIO::Pointer mincIO1 = itk::MINCImageIO::New();
  mincIO1->SetRAStoLPS(ras_to_lps);

  using ReaderType = itk::ImageFileReader<ImageType>;

  using WriterType = itk::ImageFileWriter<ImageType>;

  using MomentsCalculatorType = itk::ImageMomentsCalculator<ImageType>;

  auto reader = ReaderType::New();

  auto calculator = MomentsCalculatorType::New();

  if (itksys::SystemTools::StringEndsWith(input_image, ".mnc"))
    reader->SetImageIO(mincIO1);

  reader->SetFileName(input_image);

  reader->Update();
  calculator->SetImage(reader->GetOutput());
  calculator->Compute();

  std::cout << "Image:" << input_image << " sum=" << calculator->GetTotalMass()
            << " COM=" << calculator->GetCenterOfGravity() << std::endl;

  if (total > 0.0) // assume that if no total was provided this test should not be performed
  {
    if (itk::Math::abs(calculator->GetTotalMass() - total) > epsilon)
    {
      std::cerr << "Total sum mismatch:" << calculator->GetTotalMass()
                << " difference=" << (calculator->GetTotalMass() - total) << std::endl;
      return EXIT_FAILURE;
    }
    if (itk::Math::abs(calculator->GetCenterOfGravity()[0] - mx) > epsilon)
    {
      std::cerr << "Total mx mismatch:" << calculator->GetCenterOfGravity()[0] << std::endl;
      return EXIT_FAILURE;
    }
    if (itk::Math::abs(calculator->GetCenterOfGravity()[1] - my) > epsilon)
    {
      std::cerr << "Total my mismatch:" << calculator->GetCenterOfGravity()[1] << std::endl;
      return EXIT_FAILURE;
    }
    if (itk::Math::abs(calculator->GetCenterOfGravity()[2] - mz) > epsilon)
    {
      std::cerr << "Total mz mismatch:" << calculator->GetCenterOfGravity()[2] << std::endl;
      return EXIT_FAILURE;
    }
  }

  if (output_image)
  {
    auto writer = WriterType::New();
    writer->SetFileName(output_image);
    // writer should use default RAS to LPS flag, to satisfy comparison after
    mincIO1->SetRAStoLPS(ras_to_lps);
    if (itksys::SystemTools::StringEndsWith(output_image, ".mnc")) // HACK to enable use .mhd files
      writer->SetImageIO(mincIO1);

    writer->SetInput(reader->GetOutput());
    writer->Update();
  }
  return EXIT_SUCCESS;
}


int
itkMINCImageIOTest4(int argc, char * argv[])
{
  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope
  // scope.
  const itk::StdStreamStateSave coutState(std::cout);

  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputfile outputfile <RAS->LPS> [sum mx my mz ]" << std::endl;
    return EXIT_FAILURE;
  }

  const char * input = argv[1];
  const char * output = argv[2];
  int          ras_to_lps_test = atoi(argv[3]);
  bool         ras_to_lps = ras_to_lps_test < 0 ? false : ras_to_lps_test == 1;

  double total = 0.0;
  double mx = 0.0;
  double my = 0.0;
  double mz = 0.0;

  itk::MINCImageIOFactory::RegisterOneFactory();

  if (argc > 4)
  {
    if (argc == 8)
    {
      total = std::stod(argv[4]);
      mx = std::stod(argv[5]);
      my = std::stod(argv[6]);
      mz = std::stod(argv[7]);
    }
    else
    {
      std::cerr << "Missing parameters." << std::endl;
      std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
      std::cerr << " inputfile outputfile [sum mx my mz ]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  constexpr double epsilon = 1e-3;

  try
  {
    int ret = EXIT_SUCCESS;

    std::cout.precision(10);
    if (test_image_moments<itk::Image<double, 3>>(input, nullptr, total, mx, my, mz, epsilon, ras_to_lps) !=
        EXIT_SUCCESS)
    {
      ret = EXIT_FAILURE;
    }
    // write out only float image
    if (test_image_moments<itk::Image<float, 3>>(input, output, total, mx, my, mz, epsilon, ras_to_lps) != EXIT_SUCCESS)
    {
      ret = EXIT_FAILURE;
    }
    return ret;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
