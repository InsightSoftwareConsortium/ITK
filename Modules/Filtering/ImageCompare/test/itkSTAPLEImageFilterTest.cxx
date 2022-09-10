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
#include "itkSTAPLEImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

class StaplerBase
{
public:
  StaplerBase() = default;
  virtual ~StaplerBase() = default;

  void
  SetOutputFileName(const char * s)
  {
    m_OutputFile = s;
  }

  const std::string &
  GetOutputFileName() const
  {
    return m_OutputFile;
  }

  void
  AddFileName(const char * s)
  {
    std::string tmp(s);
    m_Files.push_back(tmp);
  }

  const std::string &
  GetFileName(unsigned int i) const
  {
    return m_Files[i];
  }

  void
  ClearFileNames()
  {
    m_Files.clear();
  }

  unsigned int
  GetNumberOfFiles() const
  {
    return static_cast<unsigned int>(m_Files.size());
  }

  virtual const std::vector<double> &
  GetSensitivity() = 0;
  virtual const std::vector<double> &
  GetSpecificity() = 0;
  virtual double
  GetSensitivity(unsigned int) = 0;
  virtual double
  GetSpecificity(unsigned int) = 0;
  virtual unsigned short
  GetForeground() const = 0;
  virtual void
  SetForeground(unsigned short) = 0;
  virtual unsigned int
  GetMaximumIterations() const = 0;
  virtual void
  SetMaximumIterations(unsigned int) = 0;
  virtual void
  SetConfidenceWeight(double) = 0;
  virtual double
  GetConfidenceWeight() const = 0;


  virtual int
  Execute() = 0;
  virtual unsigned int
  GetElapsedIterations() = 0;

protected:
  std::vector<std::string> m_Files;
  std::string              m_OutputFile;
};

template <unsigned int VDimension>
class Stapler : public StaplerBase
{
public:
  using OutputImageType = itk::Image<double, VDimension>;
  using InputImageType = itk::Image<unsigned short, VDimension>;
  using StapleFilterType = itk::STAPLEImageFilter<InputImageType, OutputImageType>;

  Stapler()
  {
    m_Stapler = StapleFilterType::New();
    this->SetForeground(1);
  }
  ~Stapler() override = default;

  unsigned int
  GetMaximumIterations() const override
  {
    return m_Stapler->GetMaximumIterations();
  }
  void
  SetMaximumIterations(unsigned int maximumIterations) override
  {
    m_Stapler->SetMaximumIterations(maximumIterations);
  }
  double
  GetConfidenceWeight() const override
  {
    return m_Stapler->GetConfidenceWeight();
  }
  void
  SetConfidenceWeight(double w) override
  {
    m_Stapler->SetConfidenceWeight(w);
  }

  const std::vector<double> &
  GetSensitivity() override
  {
    return m_Stapler->GetSensitivity();
  }
  const std::vector<double> &
  GetSpecificity() override
  {
    return m_Stapler->GetSpecificity();
  }

  double
  GetSensitivity(unsigned int i) override
  {
    return m_Stapler->GetSensitivity(i);
  }
  double
  GetSpecificity(unsigned int i) override
  {
    return m_Stapler->GetSpecificity(i);
  }

  unsigned short
  GetForeground() const override
  {
    return m_Stapler->GetForegroundValue();
  }
  void
  SetForeground(unsigned short l) override
  {
    m_Stapler->SetForegroundValue(l);
  }

  unsigned int
  GetElapsedIterations() override
  {
    return m_Stapler->GetElapsedIterations();
  }

  int
  Execute() override;

private:
  typename StapleFilterType::Pointer m_Stapler;
};


template <unsigned int VDimension>
int
Stapler<VDimension>::Execute()
{
  typename itk::ImageFileReader<InputImageType>::Pointer  reader;
  typename itk::ImageFileWriter<OutputImageType>::Pointer writer = itk::ImageFileWriter<OutputImageType>::New();

  size_t numberOfFiles = m_Files.size();

  // Set the inputs
  for (size_t i = 0; i < numberOfFiles; ++i)
  {
    reader = itk::ImageFileReader<InputImageType>::New();
    reader->SetFileName(m_Files[i].c_str());

    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    m_Stapler->SetInput(itk::Math::CastWithRangeCheck<unsigned int>(i), reader->GetOutput());
  }

  writer->SetFileName(m_OutputFile.c_str());
  writer->SetInput(m_Stapler->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}


int
itkSTAPLEImageFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr
      << "Usage: " << itkNameOfTestExecutableMacro(argv)
      << " fileDimensionality outputFileName foregroundValue maximumIterations confidenceWeight file1 file2 ... fileN"
      << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputImageType = itk::Image<unsigned short, Dimension>;
  using OutputImageType = itk::Image<double, Dimension>;

  using STAPLEImageFilterType = itk::STAPLEImageFilter<InputImageType, OutputImageType>;
  auto stapleImageFilter = STAPLEImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(stapleImageFilter, STAPLEImageFilter, ImageToImageFilter);


  StaplerBase * stapler;

  if (std::stoi(argv[1]) == 2)
  {
    stapler = new Stapler<2>;
  }
  else if (std::stoi(argv[1]) == 3)
  {
    stapler = new Stapler<3>;
  }
  else
  {
    std::cerr << "Only 2D and 3D data is currently supported" << std::endl;
    return EXIT_FAILURE;
  }

  for (int i = 0; i < argc - 6; ++i)
  {
    stapler->AddFileName(argv[i + 6]);
  }

  stapler->SetOutputFileName(argv[2]);

  auto foregroundValue = static_cast<unsigned short>(std::stoi(argv[3]));
  stapler->SetForeground(foregroundValue);
  ITK_TEST_SET_GET_VALUE(foregroundValue, stapler->GetForeground());

  auto maximumIterations = static_cast<unsigned int>(std::stoi(argv[4]));
  stapler->SetMaximumIterations(maximumIterations);
  ITK_TEST_SET_GET_VALUE(maximumIterations, stapler->GetMaximumIterations());

  auto confidenceWeight = static_cast<double>(std::stod(argv[5]));
  stapler->SetConfidenceWeight(confidenceWeight);
  ITK_TEST_SET_GET_VALUE(confidenceWeight, stapler->GetConfidenceWeight());

  // Execute the stapler
  int ret = stapler->Execute();
  if (ret != EXIT_SUCCESS)
  {
    std::cerr << "Stapler failed!" << std::endl;
    return EXIT_FAILURE;
  }

  double avgP = 0.0;
  double avgQ = 0.0;
  // Print out the specificities
  std::cout << "Number of elapsed iterations = " << stapler->GetElapsedIterations() << std::endl;

  //  std::cout.precision(5);
  //  std::cout.setf(ios::fixed, ios::floatfield);

  std::cout << "File "
            << "\t\tSensitivity(p) "
            << "\tSpecificity(q)" << std::endl;
  std::cout << "-----"
            << "\t\t-------------- "
            << "\t--------------" << std::endl;

  for (unsigned int i = 0; i < stapler->GetNumberOfFiles(); ++i)
  {
    avgQ += stapler->GetSpecificity(i);
    avgP += stapler->GetSensitivity(i);
    std::cout << i << ": " << stapler->GetFileName(i) << "\t" << stapler->GetSensitivity(i) << "\t\t"
              << stapler->GetSpecificity(i) << std::endl;
  }

  std::vector<double> specificity = stapler->GetSpecificity();
  std::cout << "Specificity: ";
  for (auto value : specificity)
  {
    std::cout << value << " ";
  }
  std::cout << std::endl;

  std::vector<double> sensitivity = stapler->GetSensitivity();
  std::cout << "Sensitivity: ";
  for (auto value : specificity)
  {
    std::cout << value << " ";
  }
  std::cout << std::endl;

  avgP /= static_cast<double>(stapler->GetNumberOfFiles());
  avgQ /= static_cast<double>(stapler->GetNumberOfFiles());

  std::cout << "Mean:\t\t" << avgP << "\t\t" << avgQ << std::endl;

  // Test index exceptions
  unsigned int i = stapler->GetNumberOfFiles() + 1;
  ITK_TRY_EXPECT_EXCEPTION(stapler->GetSensitivity(i));
  ITK_TRY_EXPECT_EXCEPTION(stapler->GetSpecificity(i));

  delete stapler;


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
