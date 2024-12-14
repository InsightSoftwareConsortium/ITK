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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#include "itkTestDriverInclude.h"

#include "itkMultiThreaderBase.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionConstIterator.h"
#include "itkTestingStretchIntensityImageFilter.h"
#include "itkTestingExtractSliceImageFilter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkTestingHashImageFilter.h"

namespace
{
RegressionTestParameters  regressionTestParameters;
std::vector<HashPairType> hashTestList;
RedirectOutputParameters  redirectOutputParameters;
} // namespace

RegressionTestParameters &
GetRegressionTestParameters()
{
  return regressionTestParameters;
}

std::vector<HashPairType> &
GetHashTestList()
{
  return hashTestList;
}

RedirectOutputParameters &
GetRedirectOutputParameters()
{
  return redirectOutputParameters;
}

namespace
{
char
my_to_lower(const char c)
{
  return static_cast<char>(::tolower(c));
}
} // namespace

void
usage()
{
  std::cerr << "usage: itkTestDriver [options] prg [args]" << '\n';
  std::cerr << "       itkTestDriver --no-process [options]" << '\n';
  std::cerr << '\n';
  std::cerr << "itkTestDriver alter the environment, run a test program and compare the images" << '\n';
  std::cerr << "produced." << '\n';
  std::cerr << '\n';
  std::cerr << "Options:" << '\n';
  std::cerr << "  --add-before-libpath PATH" << '\n';
  std::cerr << "      Add a path to the library path environment. This option take care of" << '\n';
  std::cerr << "      choosing the right environment variable for your system." << '\n';
  std::cerr << "      This option can be used several times." << '\n';
  std::cerr << '\n';
  std::cerr << "  --add-before-env NAME VALUE" << '\n';
  std::cerr << "      Add a VALUE to the variable name in the environment." << '\n';
  std::cerr << "      The separator used is the default one on the system." << '\n';
  std::cerr << "      This option can be used several times." << '\n';
  std::cerr << '\n';
  std::cerr << "  --add-before-env-with-sep NAME VALUE SEP" << '\n';
  std::cerr << "      Add a VALUE to the variable name in the environment using the provided separator." << '\n';
  std::cerr << "      This option can be used several times." << '\n';
  std::cerr << '\n';
  std::cerr << "  --remove-env NAME" << '\n';
  std::cerr << "      Remove the variable name from the environment." << '\n';
  std::cerr << "      This option can be used several times." << '\n';
  std::cerr << '\n';
  std::cerr << "  --compare TEST BASELINE" << '\n';
  std::cerr << "      Compare the TEST image to the BASELINE one." << '\n';
  std::cerr << "      This option can be used several times." << '\n';
  std::cerr << '\n';
  std::cerr << "  --compare-MD5 TEST md5hash0 [ md5hash1 ... ]" << '\n';
  std::cerr << "      Compare the TEST image file's md5 hash to the provided hash." << '\n';
  std::cerr << "      md5hash0 is required and assumed to be a hash." << '\n';
  std::cerr << "      Additional arguments are considered hashes when the string is 32 hexi-decimal characters. "
            << '\n';
  std::cerr << "      This option can be used several times for multiple comparisons." << '\n';
  std::cerr << '\n';
  std::cerr << "  --with-threads THREADS" << '\n';
  std::cerr << "      Use at most THREADS threads." << '\n';
  std::cerr << '\n';
  std::cerr << "  --without-threads" << '\n';
  std::cerr << "      Use at most one thread." << '\n';
  std::cerr << '\n';
  std::cerr << "  --compareNumberOfPixelsTolerance TOLERANCE" << '\n';
  std::cerr << "      When comparing images with --compare, allow TOLERANCE pixels to differ." << '\n';
  std::cerr << "      Default is 0." << '\n';
  std::cerr << '\n';
  std::cerr << "  --compareRadiusTolerance TOLERANCE" << '\n';
  std::cerr << "      Default is 0." << '\n';
  std::cerr << '\n';
  std::cerr << "  --compareIntensityTolerance TOLERANCE" << '\n';
  std::cerr << "      Default is 2.0." << '\n';
  std::cerr << '\n';
  std::cerr << "  --compareCoordinateTolerance TOLERANCE" << '\n';
  std::cerr << "      Default is 1.0e-6. Relative to the first spacing element of the input image." << '\n';
  std::cerr << '\n';
  std::cerr << "  --compareDirectionTolerance TOLERANCE" << '\n';
  std::cerr << "      Default is 1.0e-6." << '\n';
  std::cerr << '\n';
  std::cerr << "  --ignoreInputInformation" << '\n';
  std::cerr << "      Skip verification of matching origins, spacings, and directions of input images." << '\n';
  std::cerr << '\n';
  std::cerr << "  --no-process" << '\n';
  std::cerr << "      The test driver will not invoke any process." << '\n';
  std::cerr << '\n';
  std::cerr << "  --full-output" << '\n';
  std::cerr << "      Causes the full output of the test to be passed to cdash." << '\n';
  std::cerr << "  --redirect-output TEST_OUTPUT" << '\n';
  std::cerr << "      Redirects the test output to the file TEST_OUTPUT." << '\n';
  std::cerr << '\n';
  std::cerr << "  --" << '\n';
  std::cerr << "      The options after -- are not interpreted by this program and passed" << '\n';
  std::cerr << "      directly to the test program." << '\n';
  std::cerr << '\n';
  std::cerr << "  --help" << '\n';
  std::cerr << "      Display this message and exit." << '\n';
  std::cerr << '\n';
}


int
ProcessArguments(int * argc, ArgumentStringType * argv, ProcessedOutputType * processedOutput)
{
#if defined(LINUX) && !defined(__MINGW32__) && defined(ITK_HAS_FEENABLEEXCEPT)
  itk::FloatingPointExceptions::Enable();
#endif
  regressionTestParameters.intensityTolerance = 2.0;
  regressionTestParameters.numberOfPixelsTolerance = 0;
  regressionTestParameters.radiusTolerance = 0;
  regressionTestParameters.verifyInputInformation = true;
  regressionTestParameters.coordinateTolerance = 1.0e-6;
  regressionTestParameters.directionTolerance = 1.0e-6;
  redirectOutputParameters.redirect = false;

  if (processedOutput)
  {
    processedOutput->externalProcessMustBeCalled = true;
  }

  // parse the command line
  int  i = 1;
  bool skip = false;
  while (i < *argc)
  {
    if (!skip && strcmp((*argv)[i], "--compare") == 0)
    {
      if (i + 2 >= *argc)
      {
        usage();
        return 1;
      }
      regressionTestParameters.compareList.emplace_back((*argv)[i + 1], (*argv)[i + 2]);
      (*argv) += 3;
      *argc -= 3;
    }
    else if (!skip && strcmp((*argv)[i], "--compare-MD5") == 0)
    {
      if (i + 2 >= *argc)
      {
        usage();
        return 1;
      }
      const char * filename = (*argv)[i + 1];
      std::string  md5hash0 = (*argv)[i + 2];

      // convert hash to all lowercase letters
      std::transform(md5hash0.begin(), md5hash0.end(), md5hash0.begin(), my_to_lower);

      // check that the hash is of expected format
      if (md5hash0.size() != 32 || md5hash0.find_first_not_of("0123456789abcdef") != std::string::npos)
      {
        std::cerr << "Warning: argument does not appear to be a valid md5 hash \"" << md5hash0 << "\"." << '\n';
      }

      std::vector<std::string> hashVector;
      hashVector.push_back(md5hash0);

      (*argv) += 3;
      (*argc) -= 3;

      // continue eating hash values
      while (*argc - i > 0)
      {
        std::string md5hashN = (*argv)[i];

        // convert hash to all lowercase letters
        std::transform(md5hashN.begin(), md5hashN.end(), md5hashN.begin(), my_to_lower);

        // check if the next argument is a hash
        if (md5hashN.size() != 32 || md5hashN.find_first_not_of("0123456789abcdef") != std::string::npos)
        {
          break;
        }

        // add the hash
        hashVector.push_back(md5hashN);

        // successful hash,
        // move the arguments along
        ++(*argv);
        --(*argc);
      }

      hashTestList.emplace_back(filename, hashVector);
    }
    else if (!skip && strcmp((*argv)[i], "--") == 0)
    {
      skip = true;
      i += 1;
    }
    else if (!skip && strcmp((*argv)[i], "--help") == 0)
    {
      usage();
      return 1;
    }
    else if (!skip && strcmp((*argv)[i], "--with-threads") == 0)
    {
      if (i + 1 >= *argc)
      {
        usage();
        return 1;
      }
      // set the environment which will be read by the subprocess
      std::string threadEnv = "ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=";
      threadEnv += (*argv)[i + 1];
      itksys::SystemTools::PutEnv(threadEnv.c_str());
      // and set the number of threads locally for the comparison
      itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(std::stoi((*argv)[i + 1]));
      *argv += 2;
      *argc -= 2;
    }
    else if (!skip && strcmp((*argv)[i], "--without-threads") == 0)
    {
      itksys::SystemTools::PutEnv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1");
      itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(1);
      *argv += 1;
      *argc -= 1;
    }
    else if (!skip && strcmp((*argv)[i], "--compareNumberOfPixelsTolerance") == 0)
    {
      if (i + 1 >= *argc)
      {
        usage();
        return 1;
      }
      regressionTestParameters.numberOfPixelsTolerance = std::stoi((*argv)[i + 1]);
      *argv += 2;
      *argc -= 2;
    }
    else if (!skip && strcmp((*argv)[i], "--compareRadiusTolerance") == 0)
    {
      if (i + 1 >= *argc)
      {
        usage();
        return 1;
      }
      regressionTestParameters.radiusTolerance = std::stoi((*argv)[i + 1]);
      (*argv) += 2;
      *argc -= 2;
    }
    else if (!skip && strcmp((*argv)[i], "--compareIntensityTolerance") == 0)
    {
      if (i + 1 >= *argc)
      {
        usage();
        return 1;
      }
      regressionTestParameters.intensityTolerance = std::stod((*argv)[i + 1]);
      (*argv) += 2;
      *argc -= 2;
    }
    else if (!skip && strcmp((*argv)[i], "--compareCoordinateTolerance") == 0)
    {
      if (i + 1 >= *argc)
      {
        usage();
        return 1;
      }
      regressionTestParameters.coordinateTolerance = std::stod((*argv)[i + 1]);
      (*argv) += 2;
      *argc -= 2;
    }
    else if (!skip && strcmp((*argv)[i], "--compareDirectionTolerance") == 0)
    {
      if (i + 1 >= *argc)
      {
        usage();
        return 1;
      }
      regressionTestParameters.directionTolerance = std::stod((*argv)[i + 1]);
      (*argv) += 2;
      *argc -= 2;
    }
    else if (!skip && strcmp((*argv)[i], "--ignoreInputInformation") == 0)
    {
      regressionTestParameters.verifyInputInformation = false;
      (*argv) += 1;
      *argc -= 1;
    }
    else if (!skip && strcmp((*argv)[i], "--add-before-libpath") == 0)
    {
      if (i + 1 >= *argc)
      {
        usage();
        return 1;
      }
      if (processedOutput)
      {
        processedOutput->add_before_libpath.push_back((*argv)[i + 1]);
      }
      (*argv) += 2;
      *argc -= 2;
    }
    else if (!skip && strcmp((*argv)[i], "--add-before-env") == 0)
    {
      if (i + 2 >= *argc)
      {
        usage();
        return 1;
      }
      if (processedOutput)
      {
        processedOutput->add_before_env.push_back((*argv)[i + 1]);
        processedOutput->add_before_env.push_back((*argv)[i + 2]);
      }
      (*argv) += 3;
      *argc -= 3;
    }
    else if (!skip && strcmp((*argv)[i], "--add-before-env-with-sep") == 0)
    {
      if (i + 3 >= *argc)
      {
        usage();
        return 1;
      }
      if (processedOutput)
      {
        processedOutput->add_before_env_with_sep.push_back((*argv)[i + 1]);
        processedOutput->add_before_env_with_sep.push_back((*argv)[i + 2]);
        processedOutput->add_before_env_with_sep.push_back((*argv)[i + 3]);
      }
      (*argv) += 4;
      *argc -= 4;
    }
    else if (!skip && strcmp((*argv)[i], "--remove-env") == 0)
    {
      if (i + 1 >= *argc)
      {
        usage();
        return 1;
      }

      itksys::SystemTools::UnPutEnv((*argv)[i + 1]);

      (*argv) += 2;
      *argc -= 2;
    }
    else if (!skip && strcmp((*argv)[i], "--full-output") == 0)
    {
      // emit the string to tell ctest that the full output should be
      // passed to cdash.
      std::cout << "CTEST_FULL_OUTPUT" << '\n';
      (*argv) += 1;
      *argc -= 1;
    }
    else if (!skip && strcmp((*argv)[i], "--no-process") == 0)
    {
      // The test driver needs to invoke another executable
      // For example, the python interpreter to run Wrapping tests.
      if (processedOutput)
      {
        processedOutput->externalProcessMustBeCalled = false;
      }
      (*argv) += 1;
      *argc -= 1;
    }
    else if (!skip && strcmp((*argv)[i], "--redirectOutput") == 0)
    {
      if (i + 1 >= *argc)
      {
        usage();
        return 1;
      }
      redirectOutputParameters.redirect = true;
      redirectOutputParameters.fileName = (*argv)[i + 1];
      *argv += 2;
      *argc -= 2;
    }
    else
    {
      if (processedOutput)
      {
        processedOutput->args.push_back((*argv)[i]);
      }
      i += 1;
    }
  }

  return 0;
}


/// Get the PixelType and ComponentType from fileName
void
GetImageType(const char * fileName, itk::IOPixelEnum & pixelType, itk::IOComponentEnum & componentType)
{
  using ImageType = itk::Image<unsigned char, 3>;
  auto imageReader = itk::ImageFileReader<ImageType>::New();
  imageReader->SetFileName(fileName);
  imageReader->UpdateOutputInformation();

  pixelType = imageReader->GetImageIO()->GetPixelType();
  componentType = imageReader->GetImageIO()->GetComponentType();
}

// Regression Testing Code
//
// This method returns:
//  max int    , if there is an error reading baselineImageFilename
//  max int - 2, if there is an error reading testImageFilename
//  max int - 1, if the size of the images don't match
//  the number of pixel beyond the tolerance
//  otherwise zero is returned if the difference is with in tolerances
template <typename PixelType>
int
RegressionTestHelper(const char *       testImageFilename,
                     const char *       baselineImageFilename,
                     int                reportErrors,
                     double             intensityTolerance,
                     itk::SizeValueType numberOfPixelsTolerance,
                     unsigned int       radiusTolerance,
                     bool               verifyInputInformation,
                     double             coordinateTolerance,
                     double             directionTolerance)
{
  // Use the factory mechanism to read the test and baseline files and convert
  // them to double
  using ImageType = itk::Image<PixelType, ITK_TEST_DIMENSION_MAX>;
  using OutputType = itk::Image<unsigned char, ITK_TEST_DIMENSION_MAX>;
  using DiffOutputType = itk::Image<unsigned char, 2>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  // Read the baseline file
  auto baselineReader = ReaderType::New();
  baselineReader->SetFileName(baselineImageFilename);
  try
  {
    baselineReader->UpdateLargestPossibleRegion();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected while reading " << baselineImageFilename << " : " << e.GetDescription();
    return itk::NumericTraits<int>::max();
  }

  // Read the file generated by the test
  auto testReader = ReaderType::New();
  testReader->SetFileName(testImageFilename);
  try
  {
    testReader->UpdateLargestPossibleRegion();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected while reading " << testImageFilename << " : " << e.GetDescription() << '\n';
    return itk::NumericTraits<int>::max();
  }

  // The sizes of the baseline and test image must match
  const typename ImageType::SizeType baselineSize = baselineReader->GetOutput()->GetLargestPossibleRegion().GetSize();
  const typename ImageType::SizeType testSize = testReader->GetOutput()->GetLargestPossibleRegion().GetSize();

  if (baselineSize != testSize)
  {
    std::cerr << "The size of the Baseline image and Test image do not match!" << '\n';
    std::cerr << "Baseline image: " << baselineImageFilename << " has size " << baselineSize << '\n';
    std::cerr << "Test image:     " << testImageFilename << " has size " << testSize << '\n';
    return itk::NumericTraits<int>::max() - 1;
  }

  // Now compare the two images
  using DiffType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto diff = DiffType::New();
  diff->SetValidInput(baselineReader->GetOutput());
  diff->SetTestInput(testReader->GetOutput());
  diff->SetDifferenceThreshold(intensityTolerance);
  diff->SetToleranceRadius(radiusTolerance);
  diff->SetVerifyInputInformation(verifyInputInformation);
  diff->SetCoordinateTolerance(coordinateTolerance);
  diff->SetDirectionTolerance(directionTolerance);
  diff->UpdateLargestPossibleRegion();

  const itk::SizeValueType status = diff->GetNumberOfPixelsWithDifferences();

  if (!reportErrors)
  {
    // The measurement errors should be reported for both success and errors
    // to facilitate setting tight tolerances of tests.
    const std::string shortFilename = itksys::SystemTools::GetFilenameName(baselineImageFilename);

    std::cout << "<DartMeasurement name=\"ImageError " << shortFilename << "\" type=\"numeric/double\">";
    std::cout << status;
    std::cout << "</DartMeasurement>" << '\n';
  }

  // if there are discrepancies, create an diff image
  if ((status > numberOfPixelsTolerance) && reportErrors)
  {

    // Report actual image error to best baseline
    std::cout << "<DartMeasurement name=\"ImageError\" type=\"numeric/double\">";
    std::cout << status;
    std::cout << "</DartMeasurement>" << '\n';


    // Report statistics for pixels which exceed tolerances
    std::cout << "<DartMeasurement name=\"ImageError Minimum\" type=\"numeric/double\">";
    std::cout << diff->GetMinimumDifference() << "</DartMeasurement>" << '\n';

    std::cout << "<DartMeasurement name=\"ImageError Maximum\" type=\"numeric/double\">";
    std::cout << diff->GetMaximumDifference() << "</DartMeasurement>" << '\n';

    std::cout << "<DartMeasurement name=\"ImageError Mean\" type=\"numeric/double\">";
    std::cout << diff->GetMeanDifference() << "</DartMeasurement>" << '\n';


    using RescaleType = itk::Testing::StretchIntensityImageFilter<ImageType, OutputType>;
    using ExtractType = itk::Testing::ExtractSliceImageFilter<OutputType, DiffOutputType>;
    using WriterType = itk::ImageFileWriter<DiffOutputType>;
    using RegionType = itk::ImageRegion<ITK_TEST_DIMENSION_MAX>;
    OutputType::SizeType size{};

    auto rescale = RescaleType::New();
    rescale->SetOutputMinimum(itk::NumericTraits<unsigned char>::NonpositiveMin());
    rescale->SetOutputMaximum(itk::NumericTraits<unsigned char>::max());
    rescale->SetInput(diff->GetOutput());
    rescale->UpdateLargestPossibleRegion();
    size = rescale->GetOutput()->GetLargestPossibleRegion().GetSize();

    // Get the center slice of the image,  In 3D, the first slice
    // is often a black slice with little debugging information.
    OutputType::IndexType index{};
    for (unsigned int i = 2; i < ITK_TEST_DIMENSION_MAX; ++i)
    {
      index[i] = size[i] / 2; // NOTE: Integer Divide used to get approximately
                              // the center slice
      size[i] = 0;
    }

    RegionType region;
    region.SetIndex(index);

    region.SetSize(size);

    auto extract = ExtractType::New();
    extract->SetDirectionCollapseToIdentity();
    extract->SetInput(rescale->GetOutput());
    extract->SetExtractionRegion(region);

    auto writer = WriterType::New();
    writer->SetInput(extract->GetOutput());

    std::ostringstream diffName;
    diffName << testImageFilename << ".diff.png";
    try
    {
      rescale->SetInput(diff->GetOutput());
      rescale->Update();
    }
    catch (const std::exception & e)
    {
      std::cerr << "Error during rescale of " << diffName.str() << '\n';
      std::cerr << e.what() << '\n';
    }
    catch (...)
    {
      std::cerr << "Error during rescale of " << diffName.str() << '\n';
    }
    writer->SetFileName(diffName.str().c_str());
    try
    {
      writer->Update();
    }
    catch (const std::exception & e)
    {
      std::cerr << "Error during write of " << diffName.str() << '\n';
      std::cerr << e.what() << '\n';
    }
    catch (...)
    {
      std::cerr << "Error during write of " << diffName.str() << '\n';
    }

    std::cout << "<DartMeasurementFile name=\"DifferenceImage\" type=\"image/png\">";
    std::cout << diffName.str();
    std::cout << "</DartMeasurementFile>" << '\n';

    std::ostringstream baseName;
    baseName << testImageFilename << ".base.png";
    try
    {
      rescale->SetInput(baselineReader->GetOutput());
      rescale->Update();
    }
    catch (const std::exception & e)
    {
      std::cerr << "Error during rescale of " << baseName.str() << '\n';
      std::cerr << e.what() << '\n';
    }
    catch (...)
    {
      std::cerr << "Error during rescale of " << baseName.str() << '\n';
    }
    try
    {
      writer->SetFileName(baseName.str().c_str());
      writer->Update();
    }
    catch (const std::exception & e)
    {
      std::cerr << "Error during write of " << baseName.str() << '\n';
      std::cerr << e.what() << '\n';
    }
    catch (...)
    {
      std::cerr << "Error during write of " << baseName.str() << '\n';
    }

    std::cout << "<DartMeasurementFile name=\"BaselineImage\" type=\"image/png\">";
    std::cout << baseName.str();
    std::cout << "</DartMeasurementFile>" << '\n';

    std::ostringstream testName;
    testName << testImageFilename << ".test.png";
    try
    {
      rescale->SetInput(testReader->GetOutput());
      rescale->Update();
    }
    catch (const std::exception & e)
    {
      std::cerr << "Error during rescale of " << testName.str() << '\n';
      std::cerr << e.what() << '\n';
    }
    catch (...)
    {
      std::cerr << "Error during rescale of " << testName.str() << '\n';
    }
    try
    {
      writer->SetFileName(testName.str().c_str());
      writer->Update();
    }
    catch (const std::exception & e)
    {
      std::cerr << "Error during write of " << testName.str() << '\n';
      std::cerr << e.what() << '\n';
    }
    catch (...)
    {
      std::cerr << "Error during write of " << testName.str() << '\n';
    }

    std::cout << "<DartMeasurementFile name=\"TestImage\" type=\"image/png\">";
    std::cout << testName.str();
    std::cout << "</DartMeasurementFile>" << '\n';
  }
  return (status > numberOfPixelsTolerance) ? static_cast<int>(status) : 0;
}

int
RegressionTestImage(const char *       testImageFilename,
                    const char *       baselineImageFilename,
                    int                reportErrors,
                    double             intensityTolerance,
                    itk::SizeValueType numberOfPixelsTolerance,
                    unsigned int       radiusTolerance,
                    bool               verifyInputInformation,
                    double             coordinateTolerance,
                    double             directionTolerance)
{
  itk::IOPixelEnum     pixelType;
  itk::IOComponentEnum componentType;
  try
  {
    GetImageType(testImageFilename, pixelType, componentType);

    switch (componentType)
    {
      case itk::IOComponentEnum::UCHAR:
      case itk::IOComponentEnum::USHORT:
      case itk::IOComponentEnum::UINT:
      case itk::IOComponentEnum::ULONG:
      case itk::IOComponentEnum::ULONGLONG:
        return RegressionTestHelper<unsigned long long>(testImageFilename,
                                                        baselineImageFilename,
                                                        reportErrors,
                                                        intensityTolerance,
                                                        numberOfPixelsTolerance,
                                                        radiusTolerance,
                                                        verifyInputInformation,
                                                        coordinateTolerance,
                                                        directionTolerance);
      case itk::IOComponentEnum::CHAR:
      case itk::IOComponentEnum::SHORT:
      case itk::IOComponentEnum::INT:
      case itk::IOComponentEnum::LONG:
      case itk::IOComponentEnum::LONGLONG:
        return RegressionTestHelper<long long>(testImageFilename,
                                               baselineImageFilename,
                                               reportErrors,
                                               intensityTolerance,
                                               numberOfPixelsTolerance,
                                               radiusTolerance,
                                               verifyInputInformation,
                                               coordinateTolerance,
                                               directionTolerance);
      case itk::IOComponentEnum::FLOAT:
      case itk::IOComponentEnum::DOUBLE:
        return RegressionTestHelper<double>(testImageFilename,
                                            baselineImageFilename,
                                            reportErrors,
                                            intensityTolerance,
                                            numberOfPixelsTolerance,
                                            radiusTolerance,
                                            verifyInputInformation,
                                            coordinateTolerance,
                                            directionTolerance);
      case itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      default:
        std::cerr << "Exception detected while reading " << baselineImageFilename << " : "
                  << "Unknown component type";
        return itk::NumericTraits<int>::max();
    }
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected while reading " << baselineImageFilename << " : " << e.GetDescription();
    return itk::NumericTraits<int>::max();
  }
}

template <typename TImageType>
std::string
ComputeHash(const char * testImageFilename)
{
  using ImageType = TImageType;
  using ReaderType = itk::ImageFileReader<ImageType>;


  // Read the file generated by the test
  auto testReader = ReaderType::New();
  testReader->SetFileName(testImageFilename);
  try
  {
    testReader->UpdateLargestPossibleRegion();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected while reading " << testImageFilename << " : " << e.GetDescription() << '\n';
    throw; // re-throw
  }

  using HashFilterType = itk::Testing::HashImageFilter<ImageType>;

  auto hasher = HashFilterType::New();
  hasher->SetInput(testReader->GetOutput());
  hasher->Update();

  return hasher->GetHash();
}

int
HashTestImage(const char * testImageFilename, const std::vector<std::string> & baselineMD5Vector)
{
  const itk::ImageIOBase::Pointer iobase =
    itk::ImageIOFactory::CreateImageIO(testImageFilename, itk::ImageIOFactory::IOFileModeEnum::ReadMode);

  if (iobase.IsNull())
  {
    itkGenericExceptionMacro("Unable to determine ImageIO reader for \"" << testImageFilename << '"');
  }

  // Read the image information
  iobase->SetFileName(testImageFilename);
  iobase->ReadImageInformation();

  // get output information about input image
  const itk::IOComponentEnum componentType = iobase->GetComponentType();

  std::string testMD5 = "";
  switch (componentType)
  {
    case itk::IOComponentEnum::CHAR:
      testMD5 = ComputeHash<itk::VectorImage<char, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::UCHAR:
      testMD5 = ComputeHash<itk::VectorImage<unsigned char, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::SHORT:
      testMD5 = ComputeHash<itk::VectorImage<short, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::USHORT:
      testMD5 = ComputeHash<itk::VectorImage<unsigned short, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::INT:
      testMD5 = ComputeHash<itk::VectorImage<int, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::UINT:
      testMD5 = ComputeHash<itk::VectorImage<unsigned int, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::LONG:
      testMD5 = ComputeHash<itk::VectorImage<long, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::ULONG:
      testMD5 = ComputeHash<itk::VectorImage<unsigned long, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::LONGLONG:
      testMD5 = ComputeHash<itk::VectorImage<long long, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::ULONGLONG:
      testMD5 = ComputeHash<itk::VectorImage<unsigned long long, ITK_TEST_DIMENSION_MAX>>(testImageFilename);
      break;
    case itk::IOComponentEnum::FLOAT:
    case itk::IOComponentEnum::DOUBLE:
      std::cerr << "Hashing is not supporting for float and double images." << '\n';
      itkGenericExceptionMacro("Hashing is not supported for images of float or doubles.");

    case itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE:
    default:
      assert(false); // should never get here unless we forgot a type
      itkGenericExceptionMacro("Logic error!");
  }

  auto iter = baselineMD5Vector.begin();
  assert(baselineMD5Vector.size());
  do
  {
    if (*iter == testMD5)
    {
      // success, let's get out of here
      return 0;
    }
  } while (++iter != baselineMD5Vector.end());

  // failed to match print the different md5s
  std::cout << "<DartMeasurement name=\"TestMD5\" type=\"text/string\">";
  std::cout << testMD5;
  std::cout << "</DartMeasurement>" << '\n';


  // print out all md5 baselines
  for (iter = baselineMD5Vector.begin(); iter != baselineMD5Vector.end(); ++iter)
  {
    std::cout << "<DartMeasurement name=\"BaselineMD5\" type=\"text/string\">";
    std::cout << *iter;
    std::cout << "</DartMeasurement>" << '\n';
  }

  using ImageType = itk::Image<double, ITK_TEST_DIMENSION_MAX>;
  using SliceImageType = itk::Image<double, 2>;
  using OutputType = itk::Image<unsigned char, 2>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using RescaleType = itk::Testing::StretchIntensityImageFilter<SliceImageType, OutputType>;
  using ExtractType = itk::Testing::ExtractSliceImageFilter<ImageType, SliceImageType>;

  // setup reader
  auto reader = ReaderType::New();
  reader->SetFileName(testImageFilename);
  reader->UpdateLargestPossibleRegion();

  ImageType::SizeType size = reader->GetOutput()->GetLargestPossibleRegion().GetSize();

  // Get the center slice of the image,  In 3D, the first slice
  // is often a black slice with little debugging information.
  ImageType::IndexType index{};
  for (unsigned int i = 2; i < ITK_TEST_DIMENSION_MAX; ++i)
  {
    index[i] = size[i] / 2; // NOTE: Integer Divide used to get approximately
    // the center slice
    size[i] = 0;
  }


  ImageType::RegionType region;
  region.SetIndex(index);

  region.SetSize(size);

  auto extract = ExtractType::New();
  extract->SetDirectionCollapseToIdentity();
  extract->SetInput(reader->GetOutput());
  extract->SetExtractionRegion(region);

  auto rescale = RescaleType::New();
  rescale->SetOutputMinimum(itk::NumericTraits<unsigned char>::NonpositiveMin());
  rescale->SetOutputMaximum(itk::NumericTraits<unsigned char>::max());
  rescale->SetInput(extract->GetOutput());

  std::ostringstream testName;
  testName << testImageFilename << ".test.png";

  try
  {
    rescale->UpdateLargestPossibleRegion();
    itk::WriteImage(rescale->GetOutput(), testName.str());
  }
  catch (const std::exception & e)
  {
    std::cerr << "Error during rescale and writing of " << testName.str() << '\n';
    std::cerr << e.what() << '\n';
  }
  catch (...)
  {
    std::cerr << "Unknown error during rescale and writing of " << testName.str() << '\n';
  }

  std::cout << "<DartMeasurementFile name=\"TestImage\" type=\"image/png\">";
  std::cout << testName.str();
  std::cout << "</DartMeasurementFile>" << '\n';

  return 1;
}

std::map<std::string, int>
RegressionTestBaselines(char * baselineFilename)
{
  std::map<std::string, int> baselines;
  baselines[std::string(baselineFilename)] = 0;

  std::string originalBaseline(baselineFilename);

  int                          x = 0;
  const std::string::size_type suffixPos = originalBaseline.rfind(".");
  std::string                  suffix;
  if (suffixPos != std::string::npos)
  {
    suffix = originalBaseline.substr(suffixPos, originalBaseline.length());
    originalBaseline.erase(suffixPos, originalBaseline.length());
  }
  while (++x)
  {
    std::ostringstream filename;
    filename << originalBaseline << '.' << x << suffix;
    std::ifstream filestream(filename.str().c_str());
    if (!filestream)
    {
      break;
    }
    baselines[filename.str()] = 0;
    filestream.close();
  }
  return baselines;
}
