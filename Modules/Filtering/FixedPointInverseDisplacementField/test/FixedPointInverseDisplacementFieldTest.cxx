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

#include "itkWin32Header.h"
#include <iostream>
#include <fstream>
#include "itkNumericTraits.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFixedPointInverseDisplacementFieldImageFilter.h"


int
FixedPointInverseDisplacementFieldTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "usage: " << argv[0] << " input_filename output_filename" << std::endl;
    return 1;
  }

  constexpr unsigned int Dimension = 3;

  using VectorPixelType = itk::Vector<float, Dimension>;
  using InputDFType = itk::Image<VectorPixelType, Dimension>;
  using OutputDFType = itk::Image<VectorPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputDFType>;
  using WriterType = itk::ImageFileWriter<OutputDFType>;

  using FPInverseType = itk::FixedPointInverseDisplacementFieldImageFilter<InputDFType, OutputDFType>;

  // read the file
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  try
  {
    reader->UpdateLargestPossibleRegion();
  }
  catch (itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected while reading " << argv[1];
    std::cerr << " : " << e.GetDescription();
    return 1;
  }

  // invert the Displacementfield
  InputDFType::Pointer inputDf = reader->GetOutput();

  FPInverseType::Pointer inverter = FPInverseType::New();
  inverter->SetInput(inputDf);
  inverter->SetOutputOrigin(inputDf->GetOrigin());
  inverter->SetSize(inputDf->GetLargestPossibleRegion().GetSize());
  inverter->SetOutputSpacing(inputDf->GetSpacing());
  inverter->SetNumberOfIterations(20);
  inverter->Update();
  OutputDFType::Pointer outputDf = inverter->GetOutput();


  // write the file
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(inverter->GetOutput());
  try
  {
    writer->SetFileName(argv[2]);
    writer->Update();
  }
  catch (...)
  {
    std::cerr << "Error during write of " << argv[2] << std::endl;
    return 1;
  }

  return 0;
}
