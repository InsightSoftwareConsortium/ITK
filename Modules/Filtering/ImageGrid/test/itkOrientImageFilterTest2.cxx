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

#include "itkOrientImageFilter.h"
#include "itkImageToImageFilter.h"
#include "itkTestingMacros.h"

typedef itk::Image<std::string,3> ImageType;

typedef itk::OrientImageFilter<ImageType,ImageType>  OrientImageFilterType;
typedef OrientImageFilterType::PermuteOrderArrayType PermuteOrderArrayType;

static void PrintImg(ImageType::Pointer img,
                     const OrientImageFilterType::PermuteOrderArrayType &permute)
{
  // Print the volume
  ImageType::IndexType Index;
  for(Index[1] = 0; Index[1] < 4; Index[1]++)
    {
    for(Index[2] = 0; Index[2] < 4; Index[2]++)
      {
      for(Index[0] = 0; Index[0] < 4; Index[0]++)
        {
        std::cerr << img->GetPixel(Index).c_str()[permute[0]]
                  << img->GetPixel(Index).c_str()[permute[1]]
                  << img->GetPixel(Index).c_str()[permute[2]]
                  << " ";
        }
      std::cerr <<  " | ";
      }
    std::cerr << std::endl;
    }
  std::cerr << std::endl;
}

ImageType::Pointer CreateAxialImage()
{
  const ImageType::SizeType imageSize = {{4, 4, 4}};
  ImageType::IndexType imageIndex = {{0, 0, 0}};
  ImageType::RegionType region;
  region.SetSize(imageSize);
  region.SetIndex(imageIndex);
  ImageType::Pointer img = ImageType::New();
  img->SetLargestPossibleRegion(region);
  img->SetBufferedRegion(region);
  img->SetRequestedRegion(region);
  img->Allocate();

  std::string row, column, slice, label;
  for(imageIndex[2] = 0; imageIndex[2] < 4; imageIndex[2]++)
    {
    if (imageIndex[2] < 2)
      {
      slice = "I";
      }
    else
      {
      slice = "S";
      }
    for(imageIndex[1] = 0; imageIndex[1] < 4; imageIndex[1]++)
      {
      if (imageIndex[1] < 2)
        {
        row = "A";
        }
      else
        {
        row = "P";
        }
      for(imageIndex[0] = 0; imageIndex[0] < 4; imageIndex[0]++)
        {
        if (imageIndex[0] < 2)
          {
          column = "R";
          }
        else
          {
          column = "L";
          }
        label = column + row + slice;
        img->SetPixel(imageIndex, label);
        }
      }
    }

  return img;
}

ImageType::Pointer CreateCoronalImage()
{
  const ImageType::SizeType imageSize = {{4, 4, 4}};
  ImageType::IndexType imageIndex = {{0, 0, 0}};
  ImageType::RegionType region;
  region.SetSize(imageSize);
  region.SetIndex(imageIndex);
  ImageType::Pointer img = ImageType::New();
  img->SetLargestPossibleRegion(region);
  img->SetBufferedRegion(region);
  img->SetRequestedRegion(region);
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
  std::string row, column, slice, label;
  for(imageIndex[2] = 0; imageIndex[2] < 4; imageIndex[2]++)
    {
    if (imageIndex[2] < 2)
      {
      slice = "A";
      }
    else
      {
      slice = "P";
      }
    for(imageIndex[1] = 0; imageIndex[1] < 4; imageIndex[1]++)
      {
      if (imageIndex[1] < 2)
        {
        row = "S";
        }
      else
        {
        row = "I";
        }
      for(imageIndex[0] = 0; imageIndex[0] < 4; imageIndex[0]++)
        {
        if (imageIndex[0] < 2)
          {
          column = "R";
          }
        else
          {
          column = "L";
          }
        label = column + row + slice;
        img->SetPixel(imageIndex, label);
        }
      }
    }

  return img;
}

int itkOrientImageFilterTest2(int, char *[])
{
  ImageType::Pointer axialImage = CreateAxialImage();
  std::cerr << "Original" << std::endl;
  OrientImageFilterType::PermuteOrderArrayType permute;
  permute[0] = 0; permute[1] = 1; permute[2] = 2;
  PrintImg(axialImage, permute);

  itk::OrientImageFilter<ImageType,ImageType>::Pointer orienter =
    itk::OrientImageFilter<ImageType,ImageType>::New();

  EXERCISE_BASIC_OBJECT_METHODS( orienter, OrientImageFilter, ImageToImageFilter );

  orienter->UseImageDirectionOn();
  orienter->SetInput(axialImage);

  // Try permuting axes
  orienter->SetDesiredCoordinateOrientationToAxial();
  orienter->Update();

  ImageType::Pointer axial = orienter->GetOutput();
  std::cerr << "axial" << std::endl;
  std::cout << "PermuteOrder: " << orienter->GetPermuteOrder() << std::endl;
  std::cout << "FlipAxes: " << orienter->GetFlipAxes() << std::endl;
  orienter->GetOutput()->Print(std::cout);
  PrintImg(axial, orienter->GetPermuteOrder());

  // Go to coronal
  orienter = itk::OrientImageFilter<ImageType,ImageType>::New();
  orienter->UseImageDirectionOn();
  orienter->SetInput(axialImage);
  orienter->SetDesiredCoordinateOrientationToCoronal();
  orienter->Update();

  ImageType::Pointer coronal = orienter->GetOutput();
  std::cerr << "coronal" << std::endl;
  orienter->GetOutput()->Print (std::cout);
  std::cout << "PermuteOrder: " << orienter->GetPermuteOrder() << std::endl;
  std::cout << "FlipAxes: " << orienter->GetFlipAxes() << std::endl;
  orienter->GetOutput()->Print(std::cout);
  PrintImg(coronal, orienter->GetPermuteOrder());

  // Go to sagittal
  orienter = itk::OrientImageFilter<ImageType,ImageType>::New();
  orienter->UseImageDirectionOn();
  orienter->SetInput(axialImage);
  orienter->SetDesiredCoordinateOrientationToSagittal();
  orienter->Update();

  ImageType::Pointer sagittal = orienter->GetOutput();
  std::cerr << "sagittal" << std::endl;
  std::cout << "PermuteOrder: " << orienter->GetPermuteOrder() << std::endl;
  std::cout << "FlipAxes: " << orienter->GetFlipAxes() << std::endl;
  orienter->GetOutput()->Print(std::cout);
  PrintImg(sagittal, orienter->GetPermuteOrder());

  // ----------------------------------------------------------------------

  orienter->SetInput(axialImage);
  std::cout << "RIP" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "LIP" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "RSP" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "LSP" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "RIA" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "LIA" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "RSA" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "LSA" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "IRP" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "ILP" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "SRP" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "SLP" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "IRA" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "ILA" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "SRA" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "SLA" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "RPI" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "LPI" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "RAI" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "LAI" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "RPS" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "LPS" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "RAS" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "LAS" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "PRI" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "PLI" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "ARI" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "ALI" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "PRS" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "PLS" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "ARS" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "ALS" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "IPR" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "SPR" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "IAR" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "SAR" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "IPL" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "SPL" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "IAL" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "SAL" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "PIR" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "PSR" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "AIR" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "ASR" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "PIL" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "PSL" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "AIL" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  std::cout << "ASL" << std::endl;
  orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL);
  orienter->Update();
  std::cout << orienter->GetOutput()->GetDirection() << std::endl;

  return EXIT_SUCCESS;
}
