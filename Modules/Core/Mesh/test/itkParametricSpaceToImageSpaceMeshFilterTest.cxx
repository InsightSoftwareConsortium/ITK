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

#include "itkImageFileReader.h"
#include "itkImageRegionConstIterator.h"
#include "itkMesh.h"
#include "itkParametricSpaceToImageSpaceMeshFilter.h"


int itkParametricSpaceToImageSpaceMeshFilterTest(int argc, char * argv[])
{

  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char ImagePixelType;

  typedef itk::Image< ImagePixelType, Dimension >     ImageType;

  // Declare the mesh pixel type.
  typedef float MeshPixelType;

  typedef   itk::Point< MeshPixelType, Dimension >    MeshPointDataType;

  typedef itk::PointSet< MeshPixelType, Dimension >   PointSetType;
  typedef PointSetType::PointType                     PointType;
  typedef PointSetType::PointsContainerPointer        PointsContainerPointer;

  typedef   itk::Mesh< MeshPointDataType, Dimension >
    InputMeshType;
  typedef   itk::Mesh< InputMeshType::PointType, Dimension >
    ImageSpaceMeshType;

  // Read the input image
  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  // Store the input image for convenience
  ImageType::Pointer image = reader->GetOutput();


  InputMeshType::Pointer mesh = InputMeshType::New();

  PointSetType::Pointer pointSet = PointSetType::New();

  // Get the input image indexes for the mesh filter
  itk::ImageRegionConstIterator<ImageType> imageIterator(
    image, image->GetBufferedRegion() );

  imageIterator.GoToBegin();

  typedef PointSetType::PointType PointType;
  PointType point;

  unsigned long pointId = 0;
  while( !imageIterator.IsAtEnd() )
    {
    // Convert the pixel position into a Point
    image->TransformIndexToPhysicalPoint( imageIterator.GetIndex() , point );
    pointSet->SetPoint( pointId, point );
    // Transfer the pixel data to the value associated with the point.
    pointSet->SetPointData( pointId, imageIterator.Get() );
    ++imageIterator;
    ++pointId;
    }

  PointsContainerPointer points = pointSet->GetPoints();
  mesh->SetPoints(points);

  typedef   itk::ParametricSpaceToImageSpaceMeshFilter<
                                      InputMeshType,
                                      ImageSpaceMeshType
                                     > ParametricFilterType;

  ParametricFilterType::Pointer parametricFilter = ParametricFilterType::New();

  if( parametricFilter.IsNull() )
    {
    return EXIT_FAILURE;
    }

  // Set the input mesh for the parametric filter
  parametricFilter->SetInput(mesh);

  parametricFilter->Update();

  // Get some data about the output
  std::cout << "Points = " << parametricFilter->GetOutput()->
    GetNumberOfPoints() << std::endl;


  return EXIT_SUCCESS;
}
