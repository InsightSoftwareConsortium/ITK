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

#define ITK_LEGACY_SILENT
#include "itkOrientImageFilter.h"
#include "itkImageToImageFilter.h"
#include "itkTestingMacros.h"

using ImageType = itk::Image<std::string, 3>;

using OrientImageFilterType = itk::OrientImageFilter<ImageType, ImageType>;
using PermuteOrderArrayType = OrientImageFilterType::PermuteOrderArrayType;

static void
PrintImg(ImageType::Pointer img, const OrientImageFilterType::PermuteOrderArrayType & permute)
{
  // Print the volume
  ImageType::IndexType Index;
  for (Index[1] = 0; Index[1] < 4; Index[1]++)
  {
    for (Index[2] = 0; Index[2] < 4; Index[2]++)
    {
      for (Index[0] = 0; Index[0] < 4; Index[0]++)
      {
        std::cerr << img->GetPixel(Index).c_str()[permute[0]] << img->GetPixel(Index).c_str()[permute[1]]
                  << img->GetPixel(Index).c_str()[permute[2]] << ' ';
      }
      std::cerr << " | ";
    }
    std::cerr << '\n';
  }
  std::cerr << '\n';
}

ImageType::Pointer
CreateAxialImage()
{
  const ImageType::SizeType   imageSize = { { 4, 4, 4 } };
  ImageType::IndexType        imageIndex = { { 0, 0, 0 } };
  const ImageType::RegionType region{ imageIndex, imageSize };
  auto                        img = ImageType::New();
  img->SetRegions(region);
  img->Allocate();

  for (imageIndex[2] = 0; imageIndex[2] < 4; imageIndex[2]++)
  {
    std::string slice;
    if (imageIndex[2] < 2)
    {
      slice = "I";
    }
    else
    {
      slice = "S";
    }
    for (imageIndex[1] = 0; imageIndex[1] < 4; imageIndex[1]++)
    {
      std::string row;
      if (imageIndex[1] < 2)
      {
        row = "A";
      }
      else
      {
        row = "P";
      }
      for (imageIndex[0] = 0; imageIndex[0] < 4; imageIndex[0]++)
      {
        std::string column;
        if (imageIndex[0] < 2)
        {
          column = "R";
        }
        else
        {
          column = "L";
        }
        const std::string label = column + row + slice;
        img->SetPixel(imageIndex, label);
      }
    }
  }

  return img;
}

ImageType::Pointer
CreateCoronalImage()
{
  const ImageType::SizeType   imageSize = { { 4, 4, 4 } };
  ImageType::IndexType        imageIndex = { { 0, 0, 0 } };
  const ImageType::RegionType region{ imageIndex, imageSize };
  auto                        img = ImageType::New();
  img->SetRegions(region);
  img->Allocate();

  ImageType::DirectionType imageDirection;
  imageDirection[0][0] = 1;
  imageDirection[1][0] = 0;
  imageDirection[2][0] = 0;

  imageDirection[0][1] = 0;
  imageDirection[1][1] = 0;
  imageDirection[2][1] = -1;

  imageDirection[0][2] = 0;
  imageDirection[1][2] = 1;
  imageDirection[2][2] = 0;
  img->SetDirection(imageDirection);
  for (imageIndex[2] = 0; imageIndex[2] < 4; imageIndex[2]++)
  {
    std::string slice;
    if (imageIndex[2] < 2)
    {
      slice = "A";
    }
    else
    {
      slice = "P";
    }
    for (imageIndex[1] = 0; imageIndex[1] < 4; imageIndex[1]++)
    {
      std::string row;
      if (imageIndex[1] < 2)
      {
        row = "S";
      }
      else
      {
        row = "I";
      }
      for (imageIndex[0] = 0; imageIndex[0] < 4; imageIndex[0]++)
      {
        std::string column;
        if (imageIndex[0] < 2)
        {
          column = "R";
        }
        else
        {
          column = "L";
        }
        const std::string label = column + row + slice;
        img->SetPixel(imageIndex, label);
      }
    }
  }

  return img;
}

int
itkOrientImageFilterTest2(int, char *[])
{
  const ImageType::Pointer axialImage = CreateAxialImage();
  std::cerr << "Original" << '\n';
  OrientImageFilterType::PermuteOrderArrayType permute;
  permute[0] = 0;
  permute[1] = 1;
  permute[2] = 2;
  PrintImg(axialImage, permute);

  itk::OrientImageFilter<ImageType, ImageType>::Pointer orienter = itk::OrientImageFilter<ImageType, ImageType>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(orienter, OrientImageFilter, ImageToImageFilter);

  orienter->UseImageDirectionOn();
  orienter->SetInput(axialImage);

  // Try permuting axes
  orienter->SetDesiredCoordinateOrientationToAxial();
  orienter->Update();

  const ImageType::Pointer axial = orienter->GetOutput();
  std::cerr << "axial" << '\n';
  std::cout << "PermuteOrder: " << orienter->GetPermuteOrder() << '\n';
  std::cout << "FlipAxes: " << orienter->GetFlipAxes() << '\n';
  orienter->GetOutput()->Print(std::cout);
  PrintImg(axial, orienter->GetPermuteOrder());

  // Go to coronal
  orienter = itk::OrientImageFilter<ImageType, ImageType>::New();
  orienter->UseImageDirectionOn();
  orienter->SetInput(axialImage);
  orienter->SetDesiredCoordinateOrientationToCoronal();
  orienter->Update();

  const ImageType::Pointer coronal = orienter->GetOutput();
  std::cerr << "coronal" << '\n';
  orienter->GetOutput()->Print(std::cout);
  std::cout << "PermuteOrder: " << orienter->GetPermuteOrder() << '\n';
  std::cout << "FlipAxes: " << orienter->GetFlipAxes() << '\n';
  orienter->GetOutput()->Print(std::cout);
  PrintImg(coronal, orienter->GetPermuteOrder());

  // Go to sagittal
  orienter = itk::OrientImageFilter<ImageType, ImageType>::New();
  orienter->UseImageDirectionOn();
  orienter->SetInput(axialImage);
  orienter->SetDesiredCoordinateOrientationToSagittal();
  orienter->Update();

  const ImageType::Pointer sagittal = orienter->GetOutput();
  std::cerr << "sagittal" << '\n';
  std::cout << "PermuteOrder: " << orienter->GetPermuteOrder() << '\n';
  std::cout << "FlipAxes: " << orienter->GetFlipAxes() << '\n';
  orienter->GetOutput()->Print(std::cout);
  PrintImg(sagittal, orienter->GetPermuteOrder());

  // ----------------------------------------------------------------------

  orienter->SetInput(axialImage);
  std::cout << "RIP" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "LIP" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "RSP" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "LSP" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "RIA" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "LIA" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "RSA" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "LSA" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "IRP" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "ILP" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "SRP" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "SLP" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "IRA" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "ILA" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "SRA" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "SLA" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "RPI" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "LPI" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "RAI" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "LAI" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "RPS" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "LPS" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "RAS" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "LAS" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "PRI" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "PLI" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "ARI" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "ALI" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "PRS" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "PLS" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "ARS" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "ALS" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "IPR" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "SPR" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "IAR" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "SAR" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "IPL" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "SPL" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "IAL" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "SAL" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "PIR" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "PSR" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "AIR" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "ASR" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "PIL" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "PSL" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "AIL" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  std::cout << "ASL" << '\n';
  orienter->SetDesiredCoordinateOrientation(
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << '\n';

  return EXIT_SUCCESS;
}
