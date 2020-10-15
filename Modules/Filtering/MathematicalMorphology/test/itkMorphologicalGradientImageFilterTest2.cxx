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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMorphologicalGradientImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSimpleFilterWatcher.h"
#include <fstream>

int
itkMorphologicalGradientImageFilterTest2(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 2;

  using PType = unsigned char;
  using IType = itk::Image<PType, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using StructuringElementType = itk::BinaryBallStructuringElement<PType, dim>;
  StructuringElementType structuringElement;
  structuringElement.SetRadius(2);
  structuringElement.CreateStructuringElement();

  using GradientType = itk::MorphologicalGradientImageFilter<IType, IType, StructuringElementType>;
  GradientType::Pointer gradient = GradientType::New();
  gradient->SetInput(reader->GetOutput());
  gradient->SetKernel(structuringElement);
  itk::SimpleFilterWatcher watcher(gradient);

  const GradientType::AlgorithmEnum algorithmType = gradient->GetAlgorithm();
  std::cout << "algorithmType : " << algorithmType << std::endl;

  using WriterType = itk::ImageFileWriter<IType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(gradient->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught ! " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    structuringElement.SetRadius(4);
    gradient->SetAlgorithm(GradientType::AlgorithmEnum::BASIC);
    gradient->Update();
    const GradientType::AlgorithmEnum algorithmType1 = gradient->GetAlgorithm();
    std::cout << "algorithmType1 : " << algorithmType1 << std::endl;
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return EXIT_FAILURE;
  }

  try
  {
    using SRType = itk::FlatStructuringElement<dim>;
    SRType::RadiusType elementRadius;
    elementRadius.Fill(4);
    SRType structuringElement2 = SRType::Box(elementRadius);
    using Gradient1Type = itk::MorphologicalGradientImageFilter<IType, IType, SRType>;
    Gradient1Type::Pointer gradient1 = Gradient1Type::New();
    gradient1->SetInput(reader->GetOutput());
    gradient1->SetKernel(structuringElement2);
    gradient1->SetAlgorithm(GradientType::AlgorithmEnum::VHGW);
    gradient1->Update();
    const GradientType::AlgorithmEnum algorithmType2 = gradient1->GetAlgorithm();
    std::cout << "algorithmType : " << algorithmType2 << std::endl;

    gradient1->SetAlgorithm(GradientType::AlgorithmEnum::ANCHOR);
    gradient1->Update();
    const GradientType::AlgorithmEnum algorithmType3 = gradient1->GetAlgorithm();
    std::cout << "algorithmType : " << algorithmType3 << std::endl;
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
