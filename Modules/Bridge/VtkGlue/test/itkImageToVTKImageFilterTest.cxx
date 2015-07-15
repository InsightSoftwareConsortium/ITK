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

#include "itkImageToVTKImageFilter.h"

#include "itkRandomImageSource.h"

int itkImageToVTKImageFilterTest(int, char *[])
{
  typedef itk::Image<float, 2 >                 ImageType;
  typedef itk::RandomImageSource<ImageType>     SourceType;
  typedef itk::ImageToVTKImageFilter<ImageType> ConnectorType;

  ImageType::SizeType size;
  size.Fill(20);
  SourceType::Pointer source = SourceType::New();
  source->SetSize(size);

  ConnectorType::Pointer connector = ConnectorType::New();
  connector->SetInput(source->GetOutput());

  connector->UpdateLargestPossibleRegion();

  connector->GetOutput()->Print(std::cout);
  connector->GetImporter()->Print(std::cout);
  connector->GetExporter()->Print(std::cout);

  connector->Print(std::cout);

  connector->Update();

  return EXIT_SUCCESS;
}
