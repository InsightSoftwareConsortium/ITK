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
#include "itkImageFileReader.h"
#include "itkImageToVTKImageFilter.h"
#include "itkNormalizeImageFilter.h"
#include "itkChangeInformationImageFilter.h"
#include <vtkRenderWindowInteractor.h>

#include "itkFDFImageIOFactory.h"
#include "itkFDFImageIO.h"

#include <vtkImageViewer.h>

#include "itkImage.h"

int
main(int argc, char ** argv)
{
  using PixelType = float;
  constexpr unsigned int Dimension = 2;

  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using ImageToVTKType = itk::ImageToVTKImageFilter<ImageType>;
  using NormalizeFilter = itk::NormalizeImageFilter<ImageType, ImageType>;
  using ChangeInformationFilter = itk::ChangeInformationImageFilter<ImageType>;

  // Register FDF Factory
  itk::FDFImageIOFactory::RegisterOneFactory();

  ReaderType::Pointer              reader = ReaderType::New();
  NormalizeFilter::Pointer         normalizer = NormalizeFilter::New();
  ChangeInformationFilter::Pointer movingChange = ChangeInformationFilter::New();

  reader->SetFileName("/home/glenn/development/reader/test.fdf");

  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & exp)
  {
    std::cerr << "Exception caught" << std::endl;
    std::cerr << exp << std::endl;
  }

  std::cout << reader << std::endl;

  normalizer->SetInput(reader->GetOutput());

  movingChange->SetInput(normalizer->GetOutput());
  movingChange->CenterImageOn();


  vtkRenderWindowInteractor * iren = vtkRenderWindowInteractor::New();

  ImageToVTKType::Pointer bridge = ImageToVTKType::New();
  bridge->SetInput(movingChange->GetOutput());

  vtkImageViewer * viewer = vtkImageViewer::New();
  viewer->SetInput(bridge->GetOutput());
  viewer->SetColorWindow(1);
  viewer->SetColorLevel(0.1);
  // viewer->SetupInteractor(iren);

  while (1)
    viewer->Render();

  viewer->Delete();
  // iren->Delete();

  return 0;
}
