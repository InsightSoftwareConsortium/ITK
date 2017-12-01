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

#include "itkVTKTetrahedralMeshReader.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkPhysicsBasedNonRigidRegistrationMethod.h"
#include "itkWarpImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImageFileWriter.h"
#include "itkMetaImageIO.h"
#include "itkTestingMacros.h"

#include <iostream>


int itkPhysicsBasedNonRigidRegistrationMethodTest( int argc, char *argv[] )
{
  if( argc != 12 )
    {
    std::cerr << "Missing Parameters" << std::endl;
    std::cerr << "Usage: " << argv[0]
      << " fixedImageFile"
      << " movingImageFile"
      << " maskImageFile"
      << " meshFile"
      << " outputImageFile"
      << " selectionFraction"
      << " nonConnectivity"
      << " blockRadius"
      << " searchRadius"
      << " approximationSteps"
      << " outlierRejectionSteps";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
  // Legacy compat with older MetaImages
  itk::MetaImageIO::SetDefaultDoublePrecision(6);

  const unsigned int ImageDimension = 3;

  typedef short                                          InputPixelType;
  typedef itk::Image< InputPixelType,  ImageDimension >  InputImageType;
  typedef float                                          MeshPixelType;
  typedef itk::Mesh< MeshPixelType, ImageDimension >     MeshType;

  typedef itk::ImageFileReader< InputImageType >         ImageReaderType;

  // Read fixed image
  ImageReaderType::Pointer fixedImageReader = ImageReaderType::New();
  fixedImageReader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( fixedImageReader->Update() );

  // Read moving image
  ImageReaderType::Pointer movingImageReader = ImageReaderType::New();
  movingImageReader->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( movingImageReader->Update() );

  // Read mask image
  ImageReaderType::Pointer maskImageReader = ImageReaderType::New();
  maskImageReader->SetFileName( argv[3] );

  TRY_EXPECT_NO_EXCEPTION( maskImageReader->Update() );

  // Read mesh
  typedef itk::VTKTetrahedralMeshReader< MeshType > MeshReaderType;

  MeshReaderType::Pointer meshReader = MeshReaderType::New();
  meshReader->SetFileName( argv[4] );

  TRY_EXPECT_NO_EXCEPTION( meshReader->Update() );

  // Create PhysicsBasedNonRigidRegistrationMethod filter
  typedef itk::Image< itk::Vector< MeshPixelType, ImageDimension >,
    ImageDimension > DeformationFieldType;
  typedef itk::fem::PhysicsBasedNonRigidRegistrationMethod<
    InputImageType, InputImageType, InputImageType, MeshType,
    DeformationFieldType> PBNRRFilterType;

  PBNRRFilterType::Pointer filter = PBNRRFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter,
    PhysicsBasedNonRigidRegistrationMethod,
    ImageToImageFilter );


  InputImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  filter->SetFixedImage( fixedImage );
  TEST_SET_GET_VALUE( fixedImage, filter->GetFixedImage() );

  InputImageType::Pointer movingImage = movingImageReader->GetOutput();
  filter->SetMovingImage( movingImage );
  TEST_SET_GET_VALUE( movingImage, filter->GetMovingImage() );

  InputImageType::Pointer maskImage = maskImageReader->GetOutput();
  filter->SetMaskImage( maskImage );
  TEST_SET_GET_VALUE( maskImage, filter->GetMaskImage() );

  MeshType::Pointer mesh = meshReader->GetOutput();
  filter->SetMesh( mesh );
  TEST_SET_GET_VALUE( mesh, filter->GetMesh() );

  double selectionFraction = atof( argv[6] );
  filter->SetSelectFraction( selectionFraction );
  TEST_SET_GET_VALUE( selectionFraction, filter->GetSelectFraction() );

  unsigned int nonConnectivity = atoi( argv[7] );
  filter->SetNonConnectivity( nonConnectivity );
  TEST_SET_GET_VALUE( nonConnectivity, filter->GetNonConnectivity() );

  PBNRRFilterType::ImageSizeType::SizeValueType blockRadiusValue =
    static_cast< PBNRRFilterType::ImageSizeType::SizeValueType >( atof( argv[8] ) );
  PBNRRFilterType::ImageSizeType blockRadius;
  blockRadius.Fill( blockRadiusValue );
  filter->SetBlockRadius( blockRadius );
  TEST_SET_GET_VALUE( blockRadius, filter->GetBlockRadius() );

  PBNRRFilterType::ImageSizeType::SizeValueType searchRadiusValue =
    static_cast< PBNRRFilterType::ImageSizeType::SizeValueType >( atof( argv[9] ) );
  PBNRRFilterType::ImageSizeType searchRadius;
  searchRadius.Fill( searchRadiusValue );
  filter->SetSearchRadius( searchRadius );
  TEST_SET_GET_VALUE( searchRadius, filter->GetSearchRadius() );

  unsigned int approximationSteps = atoi( argv[10] );
  filter->SetApproximationSteps( approximationSteps );
  TEST_SET_GET_VALUE( approximationSteps, filter->GetApproximationSteps() );

  unsigned int outlierRejectionSteps = atoi( argv[11] );
  filter->SetOutlierRejectionSteps( outlierRejectionSteps );
  TEST_SET_GET_VALUE( outlierRejectionSteps, filter->GetOutlierRejectionSteps() );


  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Display the FEM filter to improve code coverage
  const PBNRRFilterType::FEMFilterType* FEMFilter = filter->GetFEMFilter();
  std::cerr << "FEMFilter: " << FEMFilter << std::endl;


  DeformationFieldType::Pointer deformationField = filter->GetOutput();

  // Warp image
  typedef itk::WarpImageFilter< InputImageType, InputImageType,
    DeformationFieldType > WarpFilterType;
  WarpFilterType::Pointer warpFilter = WarpFilterType::New();

  typedef itk::LinearInterpolateImageFunction< InputImageType, double >
    InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  warpFilter->SetInterpolator( interpolator );

  warpFilter->SetInput( movingImageReader->GetOutput() );
  warpFilter->SetOutputSpacing( deformationField->GetSpacing() );
  warpFilter->SetOutputOrigin( deformationField->GetOrigin() );
  warpFilter->SetDisplacementField( deformationField );


  TRY_EXPECT_NO_EXCEPTION( warpFilter->Update() );

  // Write warped image to file
  typedef itk::ImageFileWriter< InputImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[5] );
  writer->SetInput( warpFilter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cerr << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
