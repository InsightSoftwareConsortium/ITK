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

#include <iostream>

#include "itkQuadEdgeMesh.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkImageRegionIteratorWithIndex.h"


int itkBinaryMask3DQuadEdgeMeshSourceTest(int, char *[])
{

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the types of the output images
  typedef itk::Image<unsigned short,   Dimension>   ImageType;

  // Declare the type of the index,size and region to initialize images
  typedef itk::Index<Dimension>                     IndexType;
  typedef itk::Size<Dimension>                      SizeType;
  typedef itk::ImageRegion<Dimension>               RegionType;
  typedef ImageType::PixelType                      PixelType;

  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  // Declare the type of the Mesh
  typedef itk::QuadEdgeMesh<double, 3>              MeshType;
  typedef MeshType::PointType                       PointType;

  typedef itk::BinaryMask3DMeshSource< ImageType, MeshType >   MeshSourceType;

  const PixelType backgroundValue = 0;
  const PixelType internalValue   = 1;

  SizeType size;
  size[0] = 128;
  size[1] = 128;
  size[2] = 128;

  IndexType start;
  start.Fill(0);

  RegionType region;
  region.SetSize( size );
  region.SetIndex( start );

  ImageType::Pointer image = ImageType::New();

  image->SetRegions( region );
  image->Allocate();

  image->FillBuffer( backgroundValue );

  IteratorType it( image, region );
  it.GoToBegin();

  PointType             point;
  PointType             center;
  PointType::VectorType radial;

  IndexType centralIndex = start;
  centralIndex[0] += size[0] / 2;
  centralIndex[1] += size[1] / 2;
  centralIndex[2] += size[2] / 2;

  image->TransformIndexToPhysicalPoint( centralIndex, center );

  //
  //  Create a digitized sphere in the middle of the image.
  //
  while( !it.IsAtEnd() )
    {
    image->TransformIndexToPhysicalPoint( it.GetIndex(), point );
    radial = point - center;
    if ( radial.GetNorm() < 60.0)
      {
      it.Set( internalValue );
      }
    ++it;
    }

  MeshSourceType::Pointer meshSource = MeshSourceType::New();

  meshSource->SetInput( image );
  meshSource->SetObjectValue( internalValue );

  try
    {
    meshSource->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << meshSource->GetNumberOfNodes() << std::endl;
  std::cout << meshSource->GetNumberOfCells() << std::endl;

  return EXIT_SUCCESS;

}
