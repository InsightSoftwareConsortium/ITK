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

#include "itkVTKImageToImageFilter.h"

#include "vtkImageNoiseSource.h"
#include "vtkSmartPointer.h"

int itkVTKImageToImageFilterTest(int, char*[])
{
  typedef itk::Image<double, 2>                 ImageType;
  typedef vtkSmartPointer<vtkImageNoiseSource>  VTKNoiseType;
  typedef itk::VTKImageToImageFilter<ImageType> ConnectorType;

  VTKNoiseType noise_source = VTKNoiseType::New();
  noise_source->SetWholeExtent(0,20,0,20,0,0);
  noise_source->SetMinimum(0.0);
  noise_source->SetMaximum(1.0);
  noise_source->Update();

  ConnectorType::Pointer connector = ConnectorType::New();
  connector->SetInput(noise_source->GetOutput());
  connector->Update();

  connector->GetOutput()->Print(std::cout);
  connector->GetImporter()->Print(std::cout);
  connector->GetExporter()->Print(std::cout);

  connector->Print(std::cout);

  return EXIT_SUCCESS;
}
