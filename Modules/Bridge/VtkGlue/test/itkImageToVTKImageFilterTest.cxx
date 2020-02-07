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

#include "itkImageToVTKImageFilter.h"

#include "itkRandomImageSource.h"

#include "vtkMatrix3x3.h"

int
itkImageToVTKImageFilterTest(int, char *[])
{
  const int dim = 3;
  using ImageType = itk::Image<float, dim>;
  using SourceType = itk::RandomImageSource<ImageType>;
  using SpacingType = SourceType::SpacingType;
  using OriginType = SourceType::PointType;
  using DirectionType = SourceType::DirectionType;
  using ConnectorType = itk::ImageToVTKImageFilter<ImageType>;

  ImageType::SizeType size;
  size[0] = 40;
  size[1] = 10;
  size[2] = 20;
  SourceType::Pointer source = SourceType::New();
  source->SetSize(size);

  SpacingType spacing;
  spacing[0] = 0.5;
  spacing[1] = -2;
  spacing[2] = 1;
  OriginType origin;
  origin[0] = -1.5;
  origin[1] = 0.222;
  origin[2] = 0;
  DirectionType direction;
  direction.Fill(0.0);
  direction[0][1] = 1;
  direction[1][0] = -1;
  direction[2][2] = 0.7;
  source->SetSpacing(spacing);
  source->SetOrigin(origin);
  source->SetDirection(direction);

  source->Update();
  auto input = source->GetOutput();
  input->Print(std::cout);

  ConnectorType::Pointer connector = ConnectorType::New();
  connector->SetInput(input);
  connector->UpdateLargestPossibleRegion();
  connector->Update();

  auto output = connector->GetOutput();
  output->Print(std::cout);

  for (int i = 0; i < dim; ++i)
  {
    if (input->GetLargestPossibleRegion().GetSize()[i] != static_cast<itk::SizeValueType>(output->GetDimensions()[i]))
    {
      std::cerr << "Error: sizes do not match for component (" << i << ")." << std::endl;
      return EXIT_FAILURE;
    }
    if (input->GetSpacing()[i] != output->GetSpacing()[i])
    {
      std::cerr << "Error: spacings do not match for component (" << i << ")." << std::endl;
      return EXIT_FAILURE;
    }
    if (input->GetOrigin()[i] != output->GetOrigin()[i])
    {
      std::cerr << "Error: origins do not match for component (" << i << ")." << std::endl;
      return EXIT_FAILURE;
    }
#if VTK_MAJOR_VERSION >= 9 || (VTK_MAJOR_VERSION == 8 && VTK_MINOR_VERSION >= 90)
    for (int j = 0; j < dim; ++j)
    {
      if (input->GetDirection()[i][j] != output->GetDirectionMatrix()->GetData()[i * 3 + j])
      {
        std::cerr << "Error: directions do not match for component (" << i << "," << j << ")." << std::endl;
        return EXIT_FAILURE;
      }
    }
#endif
  }

  return EXIT_SUCCESS;
}
