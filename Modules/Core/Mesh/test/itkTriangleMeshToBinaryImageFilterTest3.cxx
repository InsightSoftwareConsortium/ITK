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

#include "itkTriangleMeshToBinaryImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkMeshFileReader.h"
#include "itkMath.h"

int
itkTriangleMeshToBinaryImageFilterTest3(int argc, char * argv[])
{

  if (argc != 12)
  {
    std::cerr << "Usage: itkTriangleMeshToBinaryImageFilterTest3 ";
    std::cerr << " inputFilename.vtk outputImageMask";
    std::cerr << " imageSizeX imageSizeY imageSizeZ ";
    std::cerr << " imageOriginX imageOriginY imageOriginZ ";
    std::cerr << " imageSpacingX imageSpacingY imageSpacingZ ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;

  using MeshType = itk::Mesh<float, Dimension>;
  using ReaderType = itk::MeshFileReader<MeshType>;

  ReaderType::Pointer polyDataReader = ReaderType::New();

  polyDataReader->SetFileName(argv[1]);

  try
  {
    polyDataReader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<unsigned char, 3>;

  using TriangleImageType = itk::TriangleMeshToBinaryImageFilter<MeshType, ImageType>;

  TriangleImageType::Pointer imageFilter = TriangleImageType::New();

  imageFilter->SetInput(polyDataReader->GetOutput());

  ImageType::SizeType size;

  size[0] = std::stoi(argv[3]);
  size[1] = std::stoi(argv[4]);
  size[2] = std::stoi(argv[5]);

  imageFilter->SetSize(size);

  ImageType::PointType origin;

  origin[0] = std::stod(argv[6]);
  origin[1] = std::stod(argv[7]);
  origin[2] = std::stod(argv[8]);

  imageFilter->SetOrigin(origin);

  ImageType::SpacingType spacing;

  spacing[0] = std::stod(argv[9]);
  spacing[1] = std::stod(argv[10]);
  spacing[2] = std::stod(argv[11]);

  imageFilter->SetSpacing(spacing);

  const ImageType::IndexType & inbuiltIndex = imageFilter->GetIndex();
  if ((inbuiltIndex[0] == 0) && (inbuiltIndex[1] == 0) && (inbuiltIndex[2] == 0))
  {
    ImageType::IndexType index;

    index[0] = 1;
    index[1] = 0;
    index[2] = 0;
    imageFilter->SetIndex(index);
  }

  const ImageType::DirectionType & inbuiltDirection = imageFilter->GetDirection();
  if ((itk::Math::ExactlyEquals(inbuiltDirection[0][0],
                                itk::NumericTraits<ImageType::DirectionType::ValueType>::OneValue())) &&
      (itk::Math::ExactlyEquals(inbuiltDirection[1][1],
                                itk::NumericTraits<ImageType::DirectionType::ValueType>::OneValue())) &&
      (itk::Math::ExactlyEquals(inbuiltDirection[2][2],
                                itk::NumericTraits<ImageType::DirectionType::ValueType>::OneValue())))
  {
    ImageType::DirectionType Direction;

    Direction[0][0] = 1.5;
    Direction[1][1] = 1;
    Direction[2][2] = 1;
    imageFilter->SetDirection(Direction);
  }
  imageFilter->SetInsideValue(200);
  imageFilter->SetOutsideValue(0);
  const double imTolerance = imageFilter->GetTolerance();
  if (imTolerance > 1e-5)
  {
    imageFilter->SetTolerance(1e-5);
  }
  else
  {
    imageFilter->SetTolerance(1e-6);
  }
  std::cout << "[PASSED]" << std::endl;

  // Testing PrintSelf
  std::cout << imageFilter << std::endl;

  // Update the filter
  imageFilter->Update();

  const ImageType::SpacingType & mySpacing = imageFilter->GetOutput()->GetSpacing();

  if ((itk::Math::NotExactlyEquals(mySpacing[0], spacing[0])) &&
      (itk::Math::NotExactlyEquals(mySpacing[1], spacing[1])) &&
      (itk::Math::NotExactlyEquals(mySpacing[2], spacing[2])))
  {
    std::cerr << "image->GetSpacing() != spacing" << std::endl;
    return EXIT_FAILURE;
  }
  const ImageType::ValueType & inPixel = imageFilter->GetInsideValue();
  if (inPixel == 0.0)
  {
    std::cerr << "image->GetInsideValue() == 0" << std::endl;
    return EXIT_FAILURE;
  }
  const ImageType::PixelType & outPixel = imageFilter->GetOutsideValue();
  if (outPixel != 0.0)
  {
    std::cerr << "image->GetOutsideValue() != 0" << std::endl;
    return EXIT_FAILURE;
  }
  const ImageType::SizeType & imSize = imageFilter->GetSize();
  if ((imSize[0] != size[0]) && (imSize[1] != size[1]) && (imSize[2] != size[2]))
  {
    std::cerr << "image->GetSize() != size" << std::endl;
    return EXIT_FAILURE;
  }

  itk::WriteImage(imageFilter->GetOutput(), argv[2], true);

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;
}
