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

#include "itkBinaryMask3DMeshSource.h"

// Define the dimension of the images
const unsigned int Dimension = 3;

// Declare the types of the output images
typedef itk::Image<unsigned short,   Dimension>   ImageType;

// Declare the type of the index,size and region to initialize images
typedef ImageType::IndexType                     IndexType;
typedef ImageType::SizeType                      SizeType;
typedef ImageType::RegionType                    RegionType;
typedef ImageType::PixelType                     PixelType;
typedef ImageType::Pointer                       ImagePointerType;

void CreateCubeConfig(
                  ImagePointerType image,
                  const unsigned int& StartX,
                  const unsigned int& StartY,
                  const unsigned int& StartZ,
                  const unsigned char& value1,
                  const unsigned char& value2,
                  const unsigned char& value3,
                  const unsigned char& value4,
                  const unsigned char& value5,
                  const unsigned char& value6,
                  const unsigned char& value7,
                  const unsigned char& value8 );

void Create16CubeConfig(
                  ImagePointerType image,
                  const unsigned int& StartX,
                  const unsigned int& StartY,
                  const unsigned int& StartZ,
                  const unsigned char& value1,
                  const unsigned char& value2,
                  const unsigned char& value3,
                  const unsigned char& value4 );

int itkBinaryMask3DMeshSourceTest(int argc, char *argv[] )
{
  // Declare the type of the Mesh
  typedef itk::Mesh<double>                                  MeshType;
  typedef itk::BinaryMask3DMeshSource< ImageType, MeshType > MeshSourceType;

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

  ImagePointerType image = ImageType::New();
  image->SetRegions( region );
  image->Allocate();
  image->FillBuffer( backgroundValue );

  unsigned int i,j,k,l;

  for( unsigned char counter = 0; counter < 18; counter++ )
    {
    i = ( counter / 1 ) % 2; // 0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1.
    j = ( counter / 2 ) % 2; // 0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1.
    k = ( counter / 4 ) % 2; // 0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1.
    l = ( counter / 8 ) % 2; // 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1.
    Create16CubeConfig( image, 0, 0, 3 * counter, i, j, k, l );
    }

  MeshSourceType::Pointer meshSource = MeshSourceType::New();
  meshSource->SetInput( image );
  meshSource->SetObjectValue( internalValue );

  if ( argc == 2 )
    {
    if ( atoi( argv[1] ) == 1 )
      {
      size[0] = 9;
      size[1] = 9;
      size[2] = 9;
      region.SetSize(size);
      meshSource->SetRegionOfInterest( region );
      }
    }

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

  std::cout << meshSource->GetNameOfClass()   << std::endl;
  std::cout << meshSource->GetNumberOfNodes() << std::endl;
  std::cout << meshSource->GetNumberOfCells() << std::endl;
  std::cout << meshSource << std::endl;

  return EXIT_SUCCESS;

}

void CreateCubeConfig(
                  ImagePointerType image,
                  const unsigned int& StartX,
                  const unsigned int& StartY,
                  const unsigned int& StartZ,
                  const unsigned char& value1,
                  const unsigned char& value2,
                  const unsigned char& value3,
                  const unsigned char& value4,
                  const unsigned char& value5,
                  const unsigned char& value6,
                  const unsigned char& value7,
                  const unsigned char& value8 )
{
  IndexType index;

  // first corner 0, 0, 0 offset
  index[0] = StartX;
  index[1] = StartY;
  index[2] = StartZ;
  image->SetPixel( index, value1 );

  // second  corner 1, 0, 0
  index[0] = StartX + 1;
  index[1] = StartY;
  index[2] = StartZ;
  image->SetPixel( index, value2 );

  // third   corner 1, 1, 0
  index[0] = StartX + 1;
  index[1] = StartY + 1;
  index[2] = StartZ;
  image->SetPixel( index, value3 );

  // fourth  corner 0, 1, 0
  index[0] = StartX;
  index[1] = StartY + 1;
  index[2] = StartZ;
  image->SetPixel( index, value4 );

  // fifth   corner 0, 0, 1
  index[0] = StartX;
  index[1] = StartY;
  index[2] = StartZ + 1;
  image->SetPixel( index, value5 );

  // sixth   corner 1, 0, 1
  index[0] = StartX + 1;
  index[1] = StartY;
  index[2] = StartZ + 1;
  image->SetPixel( index, value6 );

  // seventh corner 1, 1, 1
  index[0] = StartX + 1;
  index[1] = StartY + 1;
  index[2] = StartZ + 1;
  image->SetPixel( index, value7 );

  // eighth  corner 0, 1, 1
  index[0] = StartX;
  index[1] = StartY + 1;
  index[2] = StartZ + 1;
  image->SetPixel( index, value8 );
}

void Create16CubeConfig(
  ImagePointerType image,
  const unsigned int& StartX,
  const unsigned int& StartY,
  const unsigned int& StartZ,
  const unsigned char& value1,
  const unsigned char& value2,
  const unsigned char& value3,
  const unsigned char& value4 )
{
  // Case 0
  CreateCubeConfig(
    image,
    StartX + 0, StartY + 0, StartZ + 0,
    value1, value2, value3, value4,
    0, 0, 0, 0 );

  // Case 1
  CreateCubeConfig(
    image,
    StartX + 3, StartY + 0, StartZ + 0,
    value1, value2, value3, value4,
    0, 0, 0, 1 );

  // Case 2
  CreateCubeConfig(
    image,
    StartX + 6, StartY + 0, StartZ + 0,
    value1, value2, value3, value4,
    0, 0, 1, 0 );

  // Case 3
  CreateCubeConfig(
    image,
    StartX + 9, StartY + 0, StartZ + 0,
    value1, value2, value3, value4,
    0, 0, 1, 1 );

  // Case 4
  CreateCubeConfig(
    image,
    StartX + 0, StartY + 3, StartZ + 0,
    value1, value2, value3, value4,
    0, 1, 0, 0 );

  // Case 5
  CreateCubeConfig(
    image,
    StartX + 3, StartY + 3, StartZ + 0,
    value1, value2, value3, value4,
    0, 1, 0, 1 );

  // Case 6
  CreateCubeConfig(
    image,
    StartX + 6, StartY + 3, StartZ + 0,
    value1, value2, value3, value4,
    0, 1, 1, 0 );

  // Case 7
  CreateCubeConfig(
    image,
    StartX + 9, StartY + 3, StartZ + 0,
    value1, value2, value3, value4,
    0, 1, 1, 1 );

  // Case 8
  CreateCubeConfig(
    image,
    StartX + 0, StartY + 6, StartZ + 0,
    value1, value2, value3, value4,
    1, 0, 0, 0 );

  // Case 9
  CreateCubeConfig(
    image,
    StartX + 3, StartY + 6, StartZ + 0,
    value1, value2, value3, value4,
    1, 0, 0, 1 );

  // Case 10
  CreateCubeConfig(
    image,
    StartX + 6, StartY + 6, StartZ + 0,
    value1, value2, value3, value4,
    1, 0, 1, 0 );

  // Case 11
  CreateCubeConfig(
    image,
    StartX + 9, StartY + 6, StartZ + 0,
    value1, value2, value3, value4,
    1, 0, 1, 1 );

  // Case 12
  CreateCubeConfig(
    image,
    StartX + 0, StartY + 9, StartZ + 0,
    value1, value2, value3, value4,
    1, 1, 0, 0 );

  // Case 13
  CreateCubeConfig(
    image,
    StartX + 3, StartY + 9, StartZ + 0,
    value1, value2, value3, value4,
    1, 1, 0, 1 );

  // Case 14
  CreateCubeConfig(
    image,
    StartX + 6, StartY + 9, StartZ + 0,
    value1, value2, value3, value4,
    1, 1, 1, 0 );

  // Case 15
  CreateCubeConfig(
    image,
    StartX + 9, StartY + 9, StartZ + 0,
    value1, value2, value3, value4,
    1, 1, 1, 1 );

}
