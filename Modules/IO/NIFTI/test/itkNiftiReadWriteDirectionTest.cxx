/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include <iostream>
#include <fstream>
#include "itkNiftiImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

// debug
#include <map>

int
itkNiftiReadWriteDirectionTest(int ac, char * av[])
{
  if (ac < 5)
  {
    std::cerr << "itkNiftiReadWriteDirectionTest: <ImageWithBoth Q and S forms> <no qform> <no sform> <TestOutputDir>"
              << std::endl;
    std::cerr << "5 arguments required, recieved " << ac << std::endl;
    for (int i = 0; i < ac; ++i)
    {
      std::cerr << "\t" << i << " : " << av[i] << std::endl;
    }
    return EXIT_FAILURE;
  }

  using TestImageType = itk::Image<float, 3>;
  auto reader_lambda = [](std::string filename) -> TestImageType::Pointer {
    itk::ImageFileReader<TestImageType>::Pointer reader = itk::ImageFileReader<TestImageType>::New();
    std::string                                  test(filename);
    reader->SetFileName(test);
    reader->Update();
    return reader->GetOutput();
  };

  using TestImageType = itk::Image<float, 3>;
  TestImageType::Pointer LPSLabel = reader_lambda(av[1]);
  TestImageType::Pointer LPSLabel_noqform = reader_lambda(av[2]);
  TestImageType::Pointer LPSLabel_nosform = reader_lambda(av[3]);

  const auto LPSLabel_direction = LPSLabel->GetDirection();
  const auto LPSLabel_noqform_direction = LPSLabel_noqform->GetDirection();
  const auto LPSLabel_nosform_direction = LPSLabel_nosform->GetDirection();

  // Verify that the sform from LPSLabel being used when reading an image with both sform and qform
  // The sforms should be identical in both cases.
  if (LPSLabel_direction != LPSLabel_noqform_direction)
  {
    return EXIT_FAILURE;
  }
  // Verify that the qform alone is not identical to the sform (due to lossy storage capacity)
  // This difference is small, but periodically causes numerical precision errors,
  // VERIFY THAT THE DIRECTIONS ARE ACTUALLY DIFFERENT, They are expected to be different due to lossy
  // representaiton of the qform.
  if (LPSLabel_direction == LPSLabel_nosform_direction)
  {
    return EXIT_FAILURE;
  }

  // Write image that originally had no sform direction representation into a file with both sform and qform
  const std::string                            test_output_dir = av[4];
  const std::string                            test_filename = test_output_dir + "/test_filled_sform.nii.gz";
  itk::ImageFileWriter<TestImageType>::Pointer writer = itk::ImageFileWriter<TestImageType>::New();
  writer->SetFileName(test_filename);
  writer->SetInput(LPSLabel_nosform);
  writer->Update();


  // This time it should read from the newly written "sform" code in the image, which should
  // be the same as reading from qform of the original iamge
  TestImageType::Pointer reread = reader_lambda(test_filename);
  const auto             reread_direction = reread->GetDirection();
  const auto             mdd = reread->GetMetaDataDictionary();
  std::string            sform_code_from_nifti;
  const bool expose_success = itk::ExposeMetaData<std::string>(mdd, "sform_code_name", sform_code_from_nifti);
  if (!expose_success || sform_code_from_nifti != "NIFTI_XFORM_SCANNER_ANAT")
  {
    std::cerr << "Error: sform not set during writing" << std::endl;
    return EXIT_FAILURE;
  }
  bool is_close_qform_converted = true;
  for (int r = 0; r < 3; ++r)
  {
    for (int c = 0; c < 3; ++c)
    {
      const double diff = std::fabs(LPSLabel_nosform_direction[r][c] - reread_direction[r][c]);
      if (diff > 1e-8)
      {
        is_close_qform_converted = false;
      }
    }
  }
  if (!is_close_qform_converted)
  {
    std::cerr << LPSLabel_nosform_direction << " \n\n" << reread_direction << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
