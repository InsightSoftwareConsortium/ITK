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

#include "itkNumericTraits.h"
#include "itkTriangleMeshToBinaryImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkMeshFileReader.h"
#include "itkTestingMacros.h"
#include "itkImage.h"
#include "itkMath.h"
int itkTriangleMeshToBinaryImageFilterTest4( int argc, char * argv [] )
{

  if( argc != 12 )
    {
    std::cerr << "Usage: itkTriangleMeshToBinaryImageFilterTest4 ";
    std::cerr << " inputFilename.vtk outputImageMask";
    std::cerr << " imageSizeX imageSizeY imageSizeZ ";
    std::cerr << " imageOriginX imageOriginY imageOriginZ ";
    std::cerr << " imageSpacingX imageSpacingY imageSpacingZ ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 3;

  typedef itk::Mesh<float, Dimension>           MeshType;
  typedef itk::MeshFileReader< MeshType >       ReaderType;

  ReaderType::Pointer  polyDataReader = ReaderType::New();

  polyDataReader->SetFileName(argv[1]);

  try
    {
    polyDataReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned char, 3> ImageType;

  typedef itk::TriangleMeshToBinaryImageFilter< MeshType, ImageType >  TriangleImageType;

  TriangleImageType::Pointer imageFilter = TriangleImageType::New();

  imageFilter->SetInput( polyDataReader->GetOutput() );

  /* Allocate the 2D image */
  ImageType::SizeType size;

  size[0] = atoi( argv[3] );
  size[1] = atoi( argv[4] );
  size[2] = atoi( argv[5] );

  ImageType::PointType origin;

  origin[0] = atof( argv[6] );
  origin[1] = atof( argv[7] );
  origin[2] = atof( argv[8] );

  ImageType::SpacingType spacing;

  spacing[0] = atof( argv[9] );
  spacing[1] = atof( argv[10] );
  spacing[2] = atof( argv[11] );


  ImageType::IndexType index3D = {{0,0,0}};
  ImageType::RegionType region3D;
  region3D.SetSize( size );
  region3D.SetIndex( index3D );

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetLargestPossibleRegion( region3D );
  inputImage->SetBufferedRegion( region3D );
  inputImage->SetRequestedRegion( region3D );
  inputImage->SetOrigin( origin );
  inputImage->SetSpacing( spacing );
  inputImage->Allocate();

  imageFilter->SetInfoImage(inputImage);

  const ImageType::IndexType& inbuiltIndex = imageFilter->GetIndex();
  if ((inbuiltIndex[0] == 0)&&(inbuiltIndex[1] == 0)&&(inbuiltIndex[2] == 0))
  {
    ImageType::IndexType index;

    index[0] = 1;
    index[1] = 0;
    index[2] = 0;
    imageFilter->SetIndex(index);
  }

  const ImageType::DirectionType& inbuiltDirection = imageFilter->GetDirection();
  if ( (itk::Math::ExactlyEquals(inbuiltDirection[0][0], itk::NumericTraits< ImageType::DirectionType::ValueType >::OneValue())) &&
       (itk::Math::ExactlyEquals(inbuiltDirection[1][1], itk::NumericTraits< ImageType::DirectionType::ValueType >::OneValue())) &&
       (itk::Math::ExactlyEquals(inbuiltDirection[2][2], itk::NumericTraits< ImageType::DirectionType::ValueType >::OneValue())) )
  {
    ImageType::DirectionType Direction;

    Direction[0][0] = 1.5;
    Direction[1][1] = 1;
    Direction[2][2] = 1;
    imageFilter->SetDirection(Direction);
  }
  imageFilter->SetInsideValue(255);
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
  std::cout << imageFilter <<std::endl;

  //Update the filter
  try
    {
    imageFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  const ImageType::SpacingType& mySpacing = imageFilter->GetOutput()->GetSpacing();

  if((itk::Math::NotExactlyEquals(mySpacing[0], spacing[0]))&&(itk::Math::NotExactlyEquals(mySpacing[1], spacing[1]))&&(itk::Math::NotExactlyEquals(mySpacing[2] , spacing[2])))
    {
    std::cerr << "image->GetSpacing() != spacing" <<std::endl;
    return EXIT_FAILURE;
    }
  const ImageType::ValueType& inPixel = imageFilter->GetInsideValue();
  if( inPixel == 0.0 )
    {
    std::cerr << "image->GetInsideValue() == 0" <<std::endl;
    return EXIT_FAILURE;
    }
  const ImageType::PixelType& outPixel = imageFilter->GetOutsideValue();
  if( outPixel != 0.0 )
    {
    std::cerr << "image->GetOutsideValue() != 0" <<std::endl;
    return EXIT_FAILURE;
    }

  const ImageType::SizeType& imSize = imageFilter->GetSize();
  if((imSize[0] != size[0])&&(imSize[1] != size[1])&&(imSize[2] != size[2]))
    {
    std::cerr << "image->GetSize() != size" <<std::endl;
    return EXIT_FAILURE;
    }

  //setting a different signature of spacing as double//
  double spacingAsDoubleArray[3];

  spacingAsDoubleArray[0] = atof( argv[9] );
  spacingAsDoubleArray[1] = atof( argv[10] );
  spacingAsDoubleArray[2] = atof( argv[11] );
  imageFilter->SetSpacing( spacingAsDoubleArray );

  //setting a different signature of origin as double//
  double originAsDoubleArray[3];

  originAsDoubleArray[0] = atof( argv[6] );
  originAsDoubleArray[1] = atof( argv[7] );
  originAsDoubleArray[2] = atof( argv[8] );
  imageFilter->SetOrigin( originAsDoubleArray );

  try
    {
    imageFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  //setting a different signature of spacing as float//
  float spacingAsFloatArray[3];

  spacingAsFloatArray[0] = atof( argv[9] );
  spacingAsFloatArray[1] = atof( argv[10] );
  spacingAsFloatArray[2] = atof( argv[11] );
  imageFilter->SetSpacing( spacingAsFloatArray );

  const ImageType::SpacingType& testSpacing = imageFilter->GetSpacing();

  for(unsigned i = 0; i < 3; ++i)
    {
    if(itk::Math::NotExactlyEquals(testSpacing[i], spacingAsFloatArray[i]))
      {
      std::cerr << "SetSpacing failure " << testSpacing << std::endl
                << "!= " << spacingAsFloatArray[0]
                << " " << spacingAsFloatArray[1]
                << " " << spacingAsFloatArray[2] << std::endl;
      return EXIT_FAILURE;
      }
    }
  //setting a different signature of origin as float//
  float originAsFloatArray[3];

  originAsFloatArray[0] = atof( argv[6] );
  originAsFloatArray[1] = atof( argv[7] );
  originAsFloatArray[2] = atof( argv[8] );
  imageFilter->SetOrigin( originAsFloatArray );
  const ImageType::PointType& testOrigin =imageFilter->GetOrigin();
  for(unsigned i = 0; i < 3; ++i)
    {
    if(itk::Math::NotExactlyEquals(testOrigin[i], originAsFloatArray[i]))
      {
      std::cerr << "SetOrigin failure " << testOrigin << std::endl
                << "!= " << originAsFloatArray[0]
                << " " << originAsFloatArray[1]
                << " " << originAsFloatArray[2] << std::endl;
      return EXIT_FAILURE;
      }
    }

  try
    {
    imageFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  //Exercising Printself//
  imageFilter->Print(std::cout);

  typedef itk::ImageFileWriter<ImageType > WriterType;
  WriterType::Pointer imageWriter = WriterType::New();
  imageWriter->SetInput(imageFilter->GetOutput() );
  imageWriter->SetFileName( argv[2] );
  imageWriter->UseCompressionOn();
  imageWriter->Update();

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}
