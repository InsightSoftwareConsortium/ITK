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

#include "itkFEMRegistrationFilter.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

#include <fstream>


// Typedefs used for registration

// itkFEMRegistrationFilter2DTest.cxx tests the itk::FEMRegistrationFilter
// class on 2D images
const unsigned int ImageDimension = 3;

typedef unsigned char InputImagePixelType;
typedef float         DeformationFieldPixelType;

typedef itk::Image< InputImagePixelType, ImageDimension >         InputImageType;
typedef itk::Vector< DeformationFieldPixelType, ImageDimension >  DeformationFieldVectorType;
typedef itk::Image< DeformationFieldVectorType, ImageDimension >  DeformationFieldImageType;

typedef itk::fem::Element3DC0LinearHexahedronMembrane ElementType;


// Template function to fill in an image with a value.
template< typename TImage >
void FillImage( TImage * image, typename TImage::PixelType value )
{
  typedef itk::ImageRegionIteratorWithIndex< TImage > Iterator;
  Iterator it( image, image->GetBufferedRegion() );

  for( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    it.Set( value );
    }
}

// Template function to fill in an image with a circle.
template< typename TImage >
void FillWithCircle( TImage * image, double * center, double radius,
  typename TImage::PixelType foregnd, typename TImage::PixelType backgnd )
{
  typedef itk::ImageRegionIteratorWithIndex< TImage > Iterator;
  Iterator it( image, image->GetBufferedRegion() );

  typename TImage::IndexType index;
  double r2 = itk::Math::sqr( radius );
  for( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    index = it.GetIndex();
    double distance = 0;
    for( unsigned int j = 0; j < TImage::ImageDimension; ++j )
      {
      distance += itk::Math::sqr( (double) index[j] - center[j] );
      }
    if( distance <= r2 )
      {
      it.Set( foregnd );
      }
    else
      {
      it.Set( backgnd );
      }
    }
}

int itkFEMRegistrationFilterTest2( int argc, char *argv[] )
{

  typedef InputImageType::IndexType   IndexType;
  typedef InputImageType::SizeType    SizeType;
  typedef InputImageType::RegionType  RegionType;
  typedef InputImageType::SpacingType SpacingType;
  typedef InputImageType::PointType   PointType;


  // Generate input images and initial deformation field

  PointType imageOrigin;
  imageOrigin[0] = 100.0;
  imageOrigin[1] =  50.0;
  imageOrigin[2] = 200.0;

  SpacingType spacing;
  InputImageType::SizeValueType sizeArray[ImageDimension];
  for( unsigned int i = 0; i < ImageDimension; ++i )
    {
    sizeArray[i] = 32;
    spacing[i] = 2.0;
    }

  SizeType size;
  size.SetSize( sizeArray );

  IndexType index;
  index.Fill( 0 );

  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  InputImageType::Pointer movingImage = InputImageType::New();
  InputImageType::Pointer fixedImage = InputImageType::New();

  DeformationFieldImageType::Pointer initField = DeformationFieldImageType::New();

  movingImage->SetLargestPossibleRegion( region );
  movingImage->SetBufferedRegion( region );
  movingImage->SetOrigin( imageOrigin );
  movingImage->SetSpacing( spacing );
  movingImage->Allocate();

  fixedImage->SetLargestPossibleRegion( region );
  fixedImage->SetBufferedRegion( region );
  fixedImage->SetOrigin( imageOrigin );
  fixedImage->SetSpacing( spacing );
  fixedImage->Allocate();

  initField->SetLargestPossibleRegion( region );
  initField->SetBufferedRegion( region );
  initField->SetOrigin( imageOrigin );
  initField->SetSpacing( spacing );
  initField->Allocate();

  double center[ImageDimension];
  double radius;
  InputImagePixelType fgnd = 250;
  InputImagePixelType bgnd = 15;

  // Set the circle center
  for( unsigned int i = 0; i < ImageDimension; ++i )
    {
    center[i] = 16;
    }

  // Fill fixed image with a circle
  radius = 8;
  FillWithCircle< InputImageType >( fixedImage, center, radius, fgnd, bgnd );

  // Fill moving image with a circle
  radius = 5;
  FillWithCircle< InputImageType >( movingImage, center, radius, fgnd, bgnd );

  // Fill initial deformation with zero vectors
  DeformationFieldVectorType zeroVec;
  zeroVec.Fill( 0.0 );
  FillImage< DeformationFieldImageType >( initField, zeroVec );


  typedef itk::fem::FEMObject< ImageDimension > FEMObjectType;
  typedef itk::fem::FEMRegistrationFilter< InputImageType,
                                          InputImageType,
                                          FEMObjectType > RegistrationType;


  // Run registration and warp moving
  for( unsigned int met = 0; met < 4; ++met )
    {
    RegistrationType::Pointer registrator = RegistrationType::New();

    EXERCISE_BASIC_OBJECT_METHODS( registrator, FEMRegistrationFilter,
      ImageToImageFilter );

    registrator->SetFixedImage( fixedImage );
    TEST_SET_GET_VALUE( fixedImage, registrator->GetFixedImage() );

    registrator->SetMovingImage( movingImage );
    TEST_SET_GET_VALUE( movingImage, registrator->GetMovingImage() );

    unsigned int maxLevel = 2;
    registrator->SetMaxLevel( maxLevel );
    TEST_SET_GET_VALUE( maxLevel, registrator->GetMaxLevel() );

    bool useNormalizedGradient = true;
    TEST_SET_GET_BOOLEAN( registrator, UseNormalizedGradient, useNormalizedGradient );

    registrator->ChooseMetric( met );

    unsigned int numberOfMaxIterations = 5;
    registrator->SetMaximumIterations( numberOfMaxIterations, 0 );
    registrator->SetMaximumIterations( numberOfMaxIterations, 1 );

    RegistrationType::Float elasticity = 10;
    registrator->SetElasticity( elasticity, 0 );
    registrator->SetElasticity( elasticity, 1 );
    TEST_SET_GET_VALUE( elasticity, registrator->GetElasticity() );

    RegistrationType::Float rho = 1;
    registrator->SetRho( rho, 0 );
    registrator->SetRho( rho, 1 );
    //TEST_SET_GET_VALUE( rho, registrator->GetRho() );

    RegistrationType::Float gamma = 1.;
    registrator->SetGamma( gamma, 0 );
    registrator->SetGamma( gamma, 1 );
    //TEST_SET_GET_VALUE( gamma, registrator->GetGamma() );

    RegistrationType::Float alpha = 1.;
    registrator->SetAlpha( alpha );
    TEST_SET_GET_VALUE( alpha, registrator->GetAlpha() );

    registrator->SetMeshPixelsPerElementAtEachResolution( 8, 0 );
    registrator->SetMeshPixelsPerElementAtEachResolution( 4, 1 );

    unsigned int widthOfMetricRegion;
    if( met == 0 || met == 3 )
      {
      widthOfMetricRegion = 0;
      }
    else
      {
      widthOfMetricRegion = 1;
      }
    registrator->SetWidthOfMetricRegion( widthOfMetricRegion, 0 );
    TEST_SET_GET_VALUE( widthOfMetricRegion,
      registrator->GetWidthOfMetricRegion() );


    registrator->SetNumberOfIntegrationPoints( 2, 0 );
    registrator->SetNumberOfIntegrationPoints( 2, 1 );

    RegistrationType::Float timeStep = 1.;
    registrator->SetTimeStep( timeStep );
    TEST_SET_GET_VALUE( timeStep, registrator->GetTimeStep() );

    unsigned int doLineSearchOnImageEnergy;
    unsigned int employRegridding;
    if( met == 0 )
      {
      doLineSearchOnImageEnergy = 2;
      employRegridding = true;
      }
    else
      {
      doLineSearchOnImageEnergy = 0;
      employRegridding = false;
      }

    registrator->SetDoLineSearchOnImageEnergy( doLineSearchOnImageEnergy );
    TEST_SET_GET_VALUE( doLineSearchOnImageEnergy,
      registrator->GetDoLineSearchOnImageEnergy() );

    registrator->SetEmployRegridding( employRegridding );
    TEST_SET_GET_VALUE( employRegridding, registrator->GetEmployRegridding() );

    bool useLandmarks = true;
    TEST_SET_GET_BOOLEAN( registrator, UseLandmarks, useLandmarks );

    bool useMassMatrix = true;
    TEST_SET_GET_BOOLEAN( registrator, UseMassMatrix, useMassMatrix );

    RegistrationType::Float energyReductionFactor = 0.0;
    registrator->SetEnergyReductionFactor( energyReductionFactor );
    TEST_SET_GET_VALUE( energyReductionFactor,
      registrator->GetEnergyReductionFactor() );

    unsigned int lineSearchMaximumIterations = 100;
    registrator->SetLineSearchMaximumIterations( lineSearchMaximumIterations );
    TEST_SET_GET_VALUE( lineSearchMaximumIterations,
      registrator->GetLineSearchMaximumIterations() );

    bool createMeshFromImage = true;
    TEST_SET_GET_BOOLEAN( registrator, CreateMeshFromImage, createMeshFromImage );

    double standardDeviation = 0.5;
    registrator->SetStandardDeviations( standardDeviation );
    //TEST_SET_GET_VALUE( standardDeviations, registrator->GetStandardDeviations() );

    standardDeviation = 1.0;
    RegistrationType::StandardDeviationsType standardDeviations;
    standardDeviations.Fill( standardDeviation );
    registrator->SetStandardDeviations( standardDeviations );
    //TEST_SET_GET_VALUE( standardDeviations, registrator->GetStandardDeviations() );

    unsigned int maximumKernelWidth = 30;
    registrator->SetMaximumKernelWidth( maximumKernelWidth );
    TEST_SET_GET_VALUE( maximumKernelWidth, registrator->GetMaximumKernelWidth() );

    double maximumError = 0.1;
    registrator->SetMaximumError( maximumError );
    TEST_SET_GET_VALUE( maximumError, registrator->GetMaximumError() );


    itk::fem::MaterialLinearElasticity::Pointer material =
      itk::fem::MaterialLinearElasticity::New();
    material->SetGlobalNumber( 0 );
    material->SetYoungsModulus( registrator->GetElasticity() );
    material->SetCrossSectionalArea( 1.0 );
    material->SetThickness( 1.0 );
    material->SetMomentOfInertia( 1.0 );
    material->SetPoissonsRatio( 0. ); // DON'T CHOOSE 1.0!!
    material->SetDensityHeatProduct( 1.0 );

    // Create the element type
    ElementType::Pointer element1 = ElementType::New();
    element1->SetMaterial( dynamic_cast< itk::fem::MaterialLinearElasticity * >( &*material ) );
    registrator->SetElement( &*element1 );
    registrator->SetMaterial( material );

    try
      {
      // Register the images
      registrator->RunRegistration();
      }
    catch( ::itk::ExceptionObject & err )
      {
        std::cerr << "ITK exception detected: "  << err;
        std::cout << "Test failed!" << std::endl;
        return EXIT_FAILURE;
      }
    catch( ... )
      {
      // fixme - changes to femparray cause it to fail : old version works
      std::cout << "Caught an exception: " << std::endl;
      return EXIT_FAILURE;
      // std::cout << err << std::endl;
      // throw err;
      }

    if( argc == 2 )
      {
      std::string outFileName = argv[1];
      std::stringstream ss;
      ss << met;
      outFileName += ss.str();
      outFileName += ".mhd";
      typedef itk::ImageFileWriter< RegistrationType::FieldType > ImageWriterType;
      ImageWriterType::Pointer writer = ImageWriterType::New();
      writer->SetFileName( outFileName );
      writer->SetInput( registrator->GetDisplacementField() );
      writer->Update();
      }

    if( argc == 3 )
      {
      std::string outFileName = argv[2];
      std::stringstream ss;
      ss << met;
      outFileName += ss.str();
      outFileName += ".mhd";
      typedef itk::ImageFileWriter< InputImageType > ImageWriterType;
      ImageWriterType::Pointer writer = ImageWriterType::New();
      writer->SetFileName( outFileName );
      writer->SetInput( registrator->GetWarpedImage() );
      writer->Update();
      }
    }

  /*
  // get warped reference image
  // ---------------------------------------------------------
  std::cout << "Compare warped moving and fixed." << std::endl;

  // compare the warp and fixed images
  itk::ImageRegionIterator<ImageType> fixedIter( fixed,
      fixed->GetBufferedRegion() );
  itk::ImageRegionIterator<ImageType> warpedIter( registrator->GetWarpedImage(),
      fixed->GetBufferedRegion() );

  unsigned int numPixelsDifferent = 0;
  while( !fixedIter.IsAtEnd() )
    {
    if( fixedIter.Get() != warpedIter.Get() )
      {
      numPixelsDifferent++;
      }
    ++fixedIter;
    ++warpedIter;
    }

  std::cout << "Number of pixels different: " << numPixelsDifferent;
  std::cout << std::endl;

  if( numPixelsDifferent > 400 )
    {
    std::cout << "Test failed - too many pixels different." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;
  */

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
