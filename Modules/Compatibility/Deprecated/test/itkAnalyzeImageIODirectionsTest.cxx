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

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

//
// This class tests that we can create a 3D image, with a specific direction
// matrix, save it as 3D image and read it back as a 3D image or as a 2D image.
//
class AnalyzeIODirectionHelper
{
public:
  typedef  unsigned char                PixelType;

  typedef itk::Image< PixelType, 3 >    Image3DType;

  typedef Image3DType::DirectionType    Direction3DType;

public:
  AnalyzeIODirectionHelper()
  {
    this->InitializeAxialDirection();
    this->InitializeSagittalDirection();
    this->InitializeCoronalDirection();
  }

  void Test()
  {
    std::cout << std::endl;
    std::cout << "--------------------------------" << std::endl;
    std::cout << "Testing Axial..." << std::endl;
    this->Test( this->m_AxialDirection, "Axial" );

    std::cout << "--------------------------------" << std::endl;
    std::cout << "Testing Sagittal..." << std::endl;
    this->Test( this->m_SagittalDirection, "Sagittal" );

    std::cout << "--------------------------------" << std::endl;
    std::cout << "Testing Coronal..." << std::endl;
    this->Test( this->m_CoronalDirection, "Coronal" );
  }

  void Test( const Direction3DType & direction, const char * directionLabel )
  {
  Image3DType::Pointer image3D = Image3DType::New();

  std::cout << "Direction = " << std::endl;
  std::cout << direction << std::endl;

  Image3DType::SizeType size3D;
  size3D.Fill(50);

  Image3DType::RegionType region3D;
  region3D.SetSize( size3D );

  image3D->SetRegions( region3D );

  image3D->Allocate(true); // initialize buffer to zero

  image3D->SetDirection( direction );

  typedef itk::ImageFileWriter< Image3DType >    Writer3DType;
  typedef itk::ImageFileReader< Image3DType >    Reader3DType;

  Writer3DType::Pointer writer3D = Writer3DType::New();
  writer3D->SetInput( image3D );

  std::string fileName = this->m_OutputDirectory;
  fileName += "/AnalyzeIODirectionTest";
  fileName += directionLabel;
  fileName += ".hdr";

  writer3D->SetFileName( fileName );

  std::cout << "Writing 3D..." << std::endl;
  std::cout << fileName << std::endl;
  writer3D->Update();

  Reader3DType::Pointer reader3D = Reader3DType::New();

  reader3D->SetFileName( fileName );

  std::cout << "Reading 3D...";
  reader3D->Update();
  std::cout << "PASSED !" << std::endl;
  std::cout << "Read 3D Direction = " << std::endl;
  std::cout << reader3D->GetOutput()->GetDirection() << std::endl;

  }

  void SetOutputDirectory( const char * directoryName )
  {
  this->m_OutputDirectory = directoryName;
  }

  void InitializeCoronalDirection()
  {
  this->m_CoronalDirection[0][0] =  1.0;
  this->m_CoronalDirection[0][1] =  0.0;
  this->m_CoronalDirection[0][2] =  0.0;
  this->m_CoronalDirection[1][0] =  0.0;
  this->m_CoronalDirection[1][1] =  0.0;
  this->m_CoronalDirection[1][2] = -1.0;
  this->m_CoronalDirection[2][0] =  0.0;
  this->m_CoronalDirection[2][1] =  1.0;
  this->m_CoronalDirection[2][2] =  0.0;
  }

  void InitializeSagittalDirection()
  {
  this->m_SagittalDirection[0][0] =  0.0;
  this->m_SagittalDirection[0][1] =  0.0;
  this->m_SagittalDirection[0][2] =  1.0;
  this->m_SagittalDirection[1][0] = -1.0;
  this->m_SagittalDirection[1][1] =  0.0;
  this->m_SagittalDirection[1][2] =  0.0;
  this->m_SagittalDirection[2][0] =  0.0;
  this->m_SagittalDirection[2][1] =  1.0;
  this->m_SagittalDirection[2][2] =  0.0;

  }

  void InitializeAxialDirection()
  {
  this->m_AxialDirection[0][0] =  1.0;
  this->m_AxialDirection[0][1] =  0.0;
  this->m_AxialDirection[0][2] =  0.0;
  this->m_AxialDirection[1][0] =  0.0;
  this->m_AxialDirection[1][1] = -1.0;
  this->m_AxialDirection[1][2] =  0.0;
  this->m_AxialDirection[2][0] =  0.0;
  this->m_AxialDirection[2][1] =  0.0;
  this->m_AxialDirection[2][2] =  1.0;
  }

private:
  std::string         m_OutputDirectory;

  Direction3DType     m_AxialDirection;
  Direction3DType     m_SagittalDirection;
  Direction3DType     m_CoronalDirection;
};

int itkAnalyzeImageIODirectionsTest( int argc, char * argv [] )
{
  if( argc != 2 )
    {
    std::cerr << "Usage : " << std::endl;
    std::cerr << argv[0] << " outputDirectory " << std::endl;
    }

  AnalyzeIODirectionHelper  testHelper;

  testHelper.SetOutputDirectory( argv[1] );

  try
    {
    testHelper.Test();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
  catch( ... )
    {
    std::cerr << "Caught non ITK exception" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test PASSED !" << std::endl;
  return EXIT_SUCCESS;
}
