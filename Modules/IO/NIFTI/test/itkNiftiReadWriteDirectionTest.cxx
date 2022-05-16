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
#include <iostream>
#include <fstream>
#include "itkNiftiImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

// debug
#include <map>

int
itkNiftiReadWriteDirectionTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << "imageWithBothQAndSForms imageWithNoQform imageWithNoSform testOutputDir" << std::endl;
    std::cerr << "5 arguments required, received " << argc << std::endl;
    for (int i = 0; i < argc; ++i)
    {
      std::cerr << "\t" << i << " : " << argv[i] << std::endl;
    }
    return EXIT_FAILURE;
  }

  using TestImageType = itk::Image<float, 3>;
  TestImageType::Pointer inputImage = itk::ReadImage<TestImageType>(argv[1]);
  TestImageType::Pointer inputImageNoQform = itk::ReadImage<TestImageType>(argv[2]);
  TestImageType::Pointer inputImageNoSform = itk::ReadImage<TestImageType>(argv[3]);

  const auto inputImageDirection = inputImage->GetDirection();
  const auto inputImageNoQformDirection = inputImageNoQform->GetDirection();
  const auto inputImageNoSformDirection = inputImageNoSform->GetDirection();

  // Verify that the sform from InputImage being used when reading an image with both sform and qform
  // The sforms should be identical in both cases.
  if (inputImageDirection != inputImageNoQformDirection)
  {
    std::cerr << "Error: direction matrix from the image with sform and qform should be identical to ";
    std::cerr << "the matrix from the image with sform only" << std::endl;
    std::cerr << "sform and qform image direction:" << std::endl << inputImageDirection << std::endl;
    std::cerr << "sform-only image direction:" << std::endl << inputImageNoQformDirection << std::endl;
    return EXIT_FAILURE;
  }
  // Verify that the qform alone is not identical to the sform (due to lossy storage capacity)
  // This difference is small, but periodically causes numerical precision errors,
  // VERIFY THAT THE DIRECTIONS ARE ACTUALLY DIFFERENT, They are expected to be different due to lossy
  // representation of the qform.
  if (inputImageDirection == inputImageNoSformDirection)
  {
    std::cerr << "Error: direction matrices from sform and qform should differ because of lossy ";
    std::cerr << "storage methods." << std::endl;
    std::cerr << "sform and qform image direction:" << std::endl << inputImageDirection << std::endl;
    std::cerr << "qform-only image direction:" << std::endl << inputImageNoSformDirection << std::endl;
    return EXIT_FAILURE;
  }

  // Write image that originally had no sform direction representation into a file with both sform and qform
  const std::string                            testOutputDir = argv[4];
  const std::string                            testFilename = testOutputDir + "/test_filled_sform.nii.gz";
  itk::ImageFileWriter<TestImageType>::Pointer writer = itk::ImageFileWriter<TestImageType>::New();
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(inputImageNoSform, testFilename));


  // This time it should read from the newly written "sform" code in the image, which should
  // be the same as reading from qform of the original image
  TestImageType::Pointer reReadImage = itk::ReadImage<TestImageType>(testFilename);
  const auto             reReadImageDirection = reReadImage->GetDirection();
  const auto             mdd = reReadImage->GetMetaDataDictionary();
  std::string            sformCodeFromNifti;
  const bool             exposeSuccess = itk::ExposeMetaData<std::string>(mdd, "sform_code_name", sformCodeFromNifti);
  if (!exposeSuccess || sformCodeFromNifti != "NIFTI_XFORM_SCANNER_ANAT")
  {
    std::cerr << "Error: sform not set during writing" << std::endl;
    return EXIT_FAILURE;
  }
  bool isCloseQformConverted = true;
  for (int r = 0; r < 3; ++r)
  {
    for (int c = 0; c < 3; ++c)
    {
      const double diff = itk::Math::abs(inputImageNoSformDirection[r][c] - reReadImageDirection[r][c]);
      if (diff > 1e-8)
      {
        isCloseQformConverted = false;
      }
    }
  }
  if (!isCloseQformConverted)
  {
    std::cerr << "Error: sform reconstructed from qform is not sufficiently similar to qform" << std::endl;
    std::cerr << "qform-only image direction:" << std::endl << inputImageNoSformDirection << std::endl;
    std::cerr << "Reconstructed sform image direction:" << std::endl << reReadImageDirection << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
