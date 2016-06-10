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

template< class TPosition >
struct helper
{
};

template< unsigned int VDimension >
struct helper< itk::Index< VDimension > >
{
  static ITK_CONSTEXPR_VAR unsigned int Dimension = VDimension;
  typedef itk::Index< VDimension > PositionType;

  template< class TImage, class TIterator >
  static PositionType GetPosition( const TImage* , const TIterator& it )
  {
    return it.GetIndex();
  }
};

template< typename TCoord, unsigned int VDimension >
struct helper< itk::Point< TCoord, VDimension > >
{
  static ITK_CONSTEXPR_VAR unsigned int Dimension = VDimension;
  typedef itk::Point< TCoord, VDimension > PositionType;

  template< class TImage, class TIterator >
  static PositionType GetPosition( const TImage* image, const TIterator& it )
  {
    typename TImage::PointType p;
    image->TransformIndexToPhysicalPoint( it.GetIndex(), p );

    PositionType point;
    point.CastFrom( p );

    return point;
  }
};


template< class TPosition >
int InternalTest(int argc, char* argv[])
{
  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  typedef TPosition PositionType;

  const unsigned int ImageDimension = TPosition::Dimension;
  typedef unsigned char                                 ImagePixelType;
  typedef itk::Image< ImagePixelType, ImageDimension >  ImageType;

  const unsigned int MeshDimension = TPosition::Dimension;
  typedef itk::Mesh< PositionType, MeshDimension >                      InputMeshType;
  typedef itk::Mesh< typename InputMeshType::PointType, MeshDimension > OutputMeshType;

  // Read the input image
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
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
  typename ImageType::Pointer image = reader->GetOutput();

  typename InputMeshType::Pointer mesh = InputMeshType::New();

  // Get the input image indexes for the mesh filter
  itk::ImageRegionConstIterator<ImageType> imageIterator( image, image->GetBufferedRegion() );
  imageIterator.GoToBegin();

  typedef typename InputMeshType::PointType PointType;
  PointType point;
  point.Fill( 0. );

  typedef typename InputMeshType::PointDataContainer        PointDataContainer;
  typedef typename InputMeshType::PointDataContainerPointer PointDataContainerPointer;

  PointDataContainerPointer pointData = PointDataContainer::New();

  // Define arbitrary initial value for mesh point data
  typename InputMeshType::PointIdentifier pointId = 0;
  imageIterator.GoToBegin();

  typedef typename ImageType::PointType ImagePointType;

  while( !imageIterator.IsAtEnd() )
    {
    // Convert the pixel position into a Point
    ImagePointType p = helper< ImagePointType >::GetPosition( image.GetPointer(), imageIterator );

    for( unsigned int dim = 0; dim < ImageDimension; dim++ )
      {
      point[ dim ] = p[ dim ];
      }
    mesh->SetPoint( pointId, point );

    // Transfer the data to the value associated with the elementId
    PositionType position = helper< PositionType >::GetPosition( image.GetPointer(), imageIterator );
    pointData->InsertElement( pointId, position );
    ++imageIterator;
    ++pointId;
    }

  mesh->SetPointData(pointData.GetPointer());

  typedef itk::ParametricSpaceToImageSpaceMeshFilter<
    InputMeshType,
    OutputMeshType > ParametricFilterType;

  typename ParametricFilterType::Pointer parametricFilter = ParametricFilterType::New();

  if( parametricFilter.IsNull() )
    {
    return EXIT_FAILURE;
    }

  // Set the input mesh for the parametric filter
  parametricFilter->SetInput(mesh);

  try
    {
    parametricFilter->Update();
    }
  catch( itk::ExceptionObject& e )
    {
    std::cerr << "Error: " << e.what() << std::endl;
    return EXIT_FAILURE;
    }

  if( parametricFilter->GetOutput()->GetNumberOfPoints() != mesh->GetNumberOfPoints() )
    {
    std::cerr << "Input and Output have different number of points" << std::endl;
    return EXIT_FAILURE;
    }
  if( parametricFilter->GetOutput()->GetNumberOfCells() != mesh->GetNumberOfCells() )
    {
    std::cerr << "Input and Output have different number of cells" << std::endl;
    return EXIT_FAILURE;
    }

  imageIterator.GoToBegin();
  pointId = 0;

  while( !imageIterator.IsAtEnd() )
    {
    // Convert the pixel position into a Point
    ImagePointType p = helper< ImagePointType >::GetPosition( image.GetPointer(), imageIterator );
    ImagePointType refData = parametricFilter->GetOutput()->GetPointData()->ElementAt( pointId );

    typename OutputMeshType::PointType position = parametricFilter->GetOutput()->GetPoints()->ElementAt( pointId );
    PositionType refPoint = helper< PositionType >::GetPosition( image.GetPointer(), imageIterator );

    for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
      if( static_cast< double >( position[dim] ) != static_cast< double >( refPoint[dim] ) )
      {
        std::cerr << "position " << position << " != ref " << refPoint << std::endl;
        return EXIT_FAILURE;
      }

      if( p[dim] != refData[dim] )
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

int itkParametricSpaceToImageSpaceMeshFilterTest(int argc, char * argv[])
{
  const unsigned int ImageDimension = 2;
  typedef unsigned char                           PixelType;
  typedef itk::Image< PixelType, ImageDimension > ImageType;
  typedef ImageType::IndexType                    IndexType;

  if( InternalTest< IndexType >( argc, argv ) == EXIT_FAILURE )
    {
    std::cerr << "Failure for itk::Index" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test succeeded for itk::Image< unsigned char, 2 >::IndexType" << std::endl;

  typedef ImageType::PointType  PointType;
  if( InternalTest< PointType >( argc, argv ) == EXIT_FAILURE )
    {
    std::cerr << "Failure for itk::Point" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test succeeded for itk::Image< unsigned char, 2 >::PointType" << std::endl;

  return EXIT_SUCCESS;
}
