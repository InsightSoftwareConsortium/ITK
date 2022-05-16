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

//  Software Guide : BeginCommandLineArgs
//  INPUTS:  {brainweb165a10f17.mha}
//  ARGUMENTS: {WhiteMatterSegmentation.mhd}
//  Software Guide : EndCommandLineArgs


#include "itkConfidenceConnectedImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkCurvatureFlowImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

// Software Guide : BeginLatex
//
// This example is a 3D version of the previous ConfidenceConnected example.
// In this particular case, we are extracting the white matter from an input
// Brain MRI dataset.
//
// Software Guide : EndLatex


int
main(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage " << std::endl;
    return EXIT_FAILURE;
  }


  using InternalPixelType = float;
  constexpr unsigned int Dimension = 3;
  using InternalImageType = itk::Image<InternalPixelType, Dimension>;

  using OutputPixelType = unsigned char;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using CastingFilterType =
    itk::CastImageFilter<InternalImageType, OutputImageType>;
  auto caster = CastingFilterType::New();


  using ReaderType = itk::ImageFileReader<InternalImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  using CurvatureFlowImageFilterType =
    itk::CurvatureFlowImageFilter<InternalImageType, InternalImageType>;
  auto smoothing = CurvatureFlowImageFilterType::New();

  using ConnectedFilterType =
    itk::ConfidenceConnectedImageFilter<InternalImageType, InternalImageType>;
  auto confidenceConnected = ConnectedFilterType::New();

  smoothing->SetInput(reader->GetOutput());
  confidenceConnected->SetInput(smoothing->GetOutput());
  caster->SetInput(confidenceConnected->GetOutput());
  writer->SetInput(caster->GetOutput());

  smoothing->SetNumberOfIterations(2);
  smoothing->SetTimeStep(0.05);

  confidenceConnected->SetMultiplier(2.5);
  confidenceConnected->SetNumberOfIterations(5);
  confidenceConnected->SetInitialNeighborhoodRadius(2);
  confidenceConnected->SetReplaceValue(255);

  InternalImageType::IndexType index1;
  index1[0] = 118;
  index1[1] = 133;
  index1[2] = 92;
  confidenceConnected->AddSeed(index1);

  InternalImageType::IndexType index2;
  index2[0] = 63;
  index2[1] = 135;
  index2[2] = 94;
  confidenceConnected->AddSeed(index2);

  InternalImageType::IndexType index3;
  index3[0] = 63;
  index3[1] = 157;
  index3[2] = 90;
  confidenceConnected->AddSeed(index3);

  InternalImageType::IndexType index4;
  index4[0] = 111;
  index4[1] = 150;
  index4[2] = 90;
  confidenceConnected->AddSeed(index4);

  InternalImageType::IndexType index5;
  index5[0] = 111;
  index5[1] = 50;
  index5[2] = 88;
  confidenceConnected->AddSeed(index5);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
