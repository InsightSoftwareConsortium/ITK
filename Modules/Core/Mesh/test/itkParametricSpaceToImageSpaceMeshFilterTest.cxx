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
#include "itkImageRegionConstIterator.h"
#include "itkMesh.h"
#include "itkParametricSpaceToImageSpaceMeshFilter.h"
#include "itkTestingMacros.h"

template <class TPosition>
struct helper
{};

template <unsigned int VDimension>
struct helper<itk::Index<VDimension>>
{
  static constexpr unsigned int Dimension = VDimension;
  using PositionType = itk::Index<VDimension>;

  template <class TImage, class TIterator>
  static PositionType
  GetPosition(const TImage *, const TIterator & it)
  {
    return it.GetIndex();
  }
};

template <typename TCoord, unsigned int VDimension>
struct helper<itk::Point<TCoord, VDimension>>
{
  static constexpr unsigned int Dimension = VDimension;
  using PositionType = itk::Point<TCoord, VDimension>;

  template <class TImage, class TIterator>
  static PositionType
  GetPosition(const TImage * image, const TIterator & it)
  {
    typename TImage::PointType p;
    image->TransformIndexToPhysicalPoint(it.GetIndex(), p);

    PositionType point;
    point.CastFrom(p);

    return point;
  }
};


template <class TPosition>
int
InternalTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  using PositionType = TPosition;

  const unsigned int ImageDimension = TPosition::Dimension;
  using ImagePixelType = unsigned char;
  using ImageType = itk::Image<ImagePixelType, ImageDimension>;

  const unsigned int MeshDimension = TPosition::Dimension;
  using InputMeshType = itk::Mesh<PositionType, MeshDimension>;
  using OutputMeshType = itk::Mesh<typename InputMeshType::PointType, MeshDimension>;

  // Read the input image
  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }

  // Store the input image for convenience
  typename ImageType::Pointer image = reader->GetOutput();

  auto mesh = InputMeshType::New();

  // Get the input image indexes for the mesh filter
  itk::ImageRegionConstIterator<ImageType> imageIterator(image, image->GetBufferedRegion());
  imageIterator.GoToBegin();

  using PointType = typename InputMeshType::PointType;
  PointType point;
  point.Fill(0.);

  using PointDataContainer = typename InputMeshType::PointDataContainer;
  using PointDataContainerPointer = typename InputMeshType::PointDataContainerPointer;

  PointDataContainerPointer pointData = PointDataContainer::New();

  // Define arbitrary initial value for mesh point data
  typename InputMeshType::PointIdentifier pointId = 0;
  imageIterator.GoToBegin();

  using ImagePointType = typename ImageType::PointType;

  while (!imageIterator.IsAtEnd())
  {
    // Convert the pixel position into a Point
    ImagePointType p = helper<ImagePointType>::GetPosition(image.GetPointer(), imageIterator);

    for (unsigned int dim = 0; dim < ImageDimension; ++dim)
    {
      point[dim] = p[dim];
    }
    mesh->SetPoint(pointId, point);

    // Transfer the data to the value associated with the elementId
    PositionType position = helper<PositionType>::GetPosition(image.GetPointer(), imageIterator);
    pointData->InsertElement(pointId, position);
    ++imageIterator;
    ++pointId;
  }

  mesh->SetPointData(pointData.GetPointer());

  using ParametricFilterType = itk::ParametricSpaceToImageSpaceMeshFilter<InputMeshType, OutputMeshType>;

  auto parametricFilter = ParametricFilterType::New();

  if (parametricFilter.IsNull())
  {
    return EXIT_FAILURE;
  }

  ITK_EXERCISE_BASIC_OBJECT_METHODS(parametricFilter, ParametricSpaceToImageSpaceMeshFilter, MeshToMeshFilter);


  // Set the input mesh for the parametric filter
  parametricFilter->SetInput(mesh);

  try
  {
    parametricFilter->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Error: " << e.what() << std::endl;
    return EXIT_FAILURE;
  }

  if (parametricFilter->GetOutput()->GetNumberOfPoints() != mesh->GetNumberOfPoints())
  {
    std::cerr << "Input and Output have different number of points" << std::endl;
    return EXIT_FAILURE;
  }
  if (parametricFilter->GetOutput()->GetNumberOfCells() != mesh->GetNumberOfCells())
  {
    std::cerr << "Input and Output have different number of cells" << std::endl;
    return EXIT_FAILURE;
  }

  imageIterator.GoToBegin();
  pointId = 0;

  while (!imageIterator.IsAtEnd())
  {
    // Convert the pixel position into a Point
    ImagePointType p = helper<ImagePointType>::GetPosition(image.GetPointer(), imageIterator);
    ImagePointType refData = parametricFilter->GetOutput()->GetPointData()->ElementAt(pointId);

    typename OutputMeshType::PointType position = parametricFilter->GetOutput()->GetPoints()->ElementAt(pointId);
    PositionType                       refPoint = helper<PositionType>::GetPosition(image.GetPointer(), imageIterator);

    for (unsigned int dim = 0; dim < ImageDimension; ++dim)
    {
      if (static_cast<double>(position[dim]) != static_cast<double>(refPoint[dim]))
      {
        std::cerr << "position " << position << " != ref " << refPoint << std::endl;
        return EXIT_FAILURE;
      }

      if (p[dim] != refData[dim])
      {
        std::cerr << "p " << p << " != refData " << refData << std::endl;
        return EXIT_FAILURE;
      }
    }

    ++imageIterator;
    ++pointId;
  }

  return EXIT_SUCCESS;
}

int
itkParametricSpaceToImageSpaceMeshFilterTest(int argc, char * argv[])
{
  constexpr unsigned int ImageDimension = 2;
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using IndexType = ImageType::IndexType;

  if (InternalTest<IndexType>(argc, argv) == EXIT_FAILURE)
  {
    std::cerr << "Failure for itk::Index" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test succeeded for itk::Image< unsigned char, 2 >::IndexType" << std::endl;

  using PointType = ImageType::PointType;
  if (InternalTest<PointType>(argc, argv) == EXIT_FAILURE)
  {
    std::cerr << "Failure for itk::Point" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test succeeded for itk::Image< unsigned char, 2 >::PointType" << std::endl;

  return EXIT_SUCCESS;
}
