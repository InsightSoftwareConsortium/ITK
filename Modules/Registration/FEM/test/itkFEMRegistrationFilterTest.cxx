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


#include <fstream>
#include "itkFEMRegistrationFilter.h"

#include "itkImageFileWriter.h"

// tyepdefs used for registration
const unsigned int ImageDimension = 3;
typedef unsigned char                                 PixelType;
typedef itk::Image<PixelType, ImageDimension>         ImageType;
typedef itk::fem::Element3DC0LinearHexahedronMembrane ElementType;

// Template function to fill in an image with a value
template <typename TImage>
void
FillImage(
  TImage * image,
  typename TImage::PixelType value )
{

  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator it( image, image->GetBufferedRegion() );

  for( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    it.Set( value );
    }

}

// Template function to fill in an image with a circle.
template <typename TImage>
void
FillWithCircle(
  TImage * image,
  double * center,
  double radius,
  typename TImage::PixelType foregnd,
  typename TImage::PixelType backgnd )
{

  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator it( image, image->GetBufferedRegion() );

  typename TImage::IndexType index;
  double r2 = vnl_math_sqr( radius );
  for( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    index = it.GetIndex();
    double distance = 0;
    for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
      {
      distance += vnl_math_sqr( (double) index[j] - center[j]);
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


int itkFEMRegistrationFilterTest(int argc, char *argv[] )
{
  typedef itk::Vector<float, ImageDimension>                VectorType;
  typedef itk::Image<VectorType, ImageDimension>            FieldType;
  typedef ImageType::IndexType                              IndexType;
  typedef ImageType::SizeType                               SizeType;
  typedef ImageType::RegionType                             RegionType;

  //--------------------------------------------------------
  std::cout << "Generate input images and initial deformation field";
  std::cout << std::endl;

  ImageType::SizeValueType sizeArray[ImageDimension];
  for (unsigned int i=0;i<ImageDimension;i++)
    {
    sizeArray[i] = 32;
    }

  SizeType size;
  size.SetSize( sizeArray );

  IndexType index;
  index.Fill( 0 );

  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  ImageType::Pointer moving = ImageType::New();
  ImageType::Pointer fixed = ImageType::New();
  FieldType::Pointer initField = FieldType::New();

  moving->SetLargestPossibleRegion( region );
  moving->SetBufferedRegion( region );
  moving->Allocate();

  fixed->SetLargestPossibleRegion( region );
  fixed->SetBufferedRegion( region );
  fixed->Allocate();

  initField->SetLargestPossibleRegion( region );
  initField->SetBufferedRegion( region );
  initField->Allocate();

  double    center[ImageDimension];
  double    radius;
  PixelType fgnd = 250;
  PixelType bgnd = 15;

  // Set the Cricle Center
  for (unsigned int i=0;i<ImageDimension;i++)
    {
    center[i] = 16;
    }

  radius = 5;
  FillWithCircle<ImageType>( moving, center, radius, fgnd, bgnd );

  // fill fixed with circle
  radius = 8;
  FillWithCircle<ImageType>( fixed, center, radius, fgnd, bgnd );

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill( 0.0 );
  FillImage<FieldType>( initField, zeroVec );

  // -------------------------------------------------------------
  typedef itk::fem::FEMObject<ImageDimension>  FEMObjectType;
  typedef itk::fem::FEMRegistrationFilter<ImageType, ImageType, FEMObjectType> RegistrationType;

  std::cout << "Run registration and warp moving" << std::endl;
  for( unsigned int met = 0; met < 4; met++ )
    {
    RegistrationType::Pointer registrator = RegistrationType::New();
    registrator->SetFixedImage( fixed );
    registrator->SetMovingImage( moving );
    registrator->SetMaxLevel(1);
    registrator->SetUseNormalizedGradient( true );
    registrator->ChooseMetric( met );

    unsigned int maxiters = 5;
    float        e = 10;
    float        p = 1;

    registrator->SetElasticity(e, 0);
    registrator->SetRho(p, 0);
    registrator->SetGamma(1., 0);
    registrator->SetAlpha(1.);
    registrator->SetMaximumIterations( maxiters, 0 );
    registrator->SetMeshPixelsPerElementAtEachResolution(4, 0);
    registrator->SetWidthOfMetricRegion(0, 0);
    if( met == 0 || met == 3 )
      {
      registrator->SetWidthOfMetricRegion(0, 0);
      }
    else
      {
      registrator->SetWidthOfMetricRegion(1, 0);
      }
    registrator->SetNumberOfIntegrationPoints(2, 0);
    registrator->SetDoLineSearchOnImageEnergy( 0 );
    registrator->SetTimeStep(1.);
    if( met == 0 )
      {
      registrator->SetDoLineSearchOnImageEnergy( (int)2);
      registrator->SetEmployRegridding(true);
      }
    else
      {
      registrator->SetDoLineSearchOnImageEnergy( (int)0);
      registrator->SetEmployRegridding(false);
      }
    registrator->SetUseLandmarks(false);

    itk::fem::MaterialLinearElasticity::Pointer m;
    m = itk::fem::MaterialLinearElasticity::New();
    m->SetGlobalNumber(0);                              // Global number of the material ///
    m->SetYoungsModulus(registrator->GetElasticity() ); // Young modulus -- used in the membrane ///
    m->SetCrossSectionalArea(1.0);                      // Crossection area ///
    m->SetThickness(1.0);                               // Crossection area ///
    m->SetMomentOfInertia(1.0);                         // Moment of inertia ///
    m->SetPoissonsRatio(0.);                            // .0;    // poissons -- DONT CHOOSE 1.0!!///
    m->SetDensityHeatProduct(1.0);

    // Create the element type
    ElementType::Pointer e1 = ElementType::New();
    e1->SetMaterial(dynamic_cast<itk::fem::MaterialLinearElasticity *>( &*m ) );
    registrator->SetElement(&*e1);
    registrator->SetMaterial(m);
    registrator->Print( std::cout );

    try
      {
      // Register the images
      registrator->RunRegistration();
      }
    catch( ::itk::ExceptionObject & err )
      {
        std::cerr << "ITK exception detected: "  << err;
        std::cout << "Test FAILED" << std::endl;
        return EXIT_FAILURE;
      }
    catch( ... )
      {
      // fixme - changes to femparray cause it to fail : old version works
      std::cout << "Caught an exception: " << std::endl;
      return EXIT_FAILURE;
      //    std::cout << err << std::endl;
      // throw err;
      }

    if (argc > 1)
      {
      std::cout << "Write out deformation field" << argv[1] << std::endl;
      std::string outFileName = argv[1];
      std::stringstream ss;
      ss << met;
      outFileName += ss.str();
      outFileName += ".mhd";
      typedef itk::ImageFileWriter<RegistrationType::FieldType>  ImageWriterType;
      ImageWriterType::Pointer writer = ImageWriterType::New();
      writer->SetFileName( outFileName );
      writer->SetInput( registrator->GetDisplacementField() );
      writer->Update();
      }

    if (argc > 2)
      {
      std::cout << "Write out deformed image" << argv[2] << std::endl;
      std::string outFileName = argv[2];
      std::stringstream ss;
      ss << met;
      outFileName += ss.str();
      outFileName += ".mhd";
      typedef itk::ImageFileWriter<ImageType>  ImageWriterType;
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

  return EXIT_SUCCESS;

}
