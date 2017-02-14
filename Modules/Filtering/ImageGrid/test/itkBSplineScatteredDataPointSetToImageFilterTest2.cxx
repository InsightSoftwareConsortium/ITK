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

#include "itkCastImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkPointSet.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkTestingMacros.h"


/**
 * In this test, we approximate a sequence of 3D points with a
 * parametric curve described by B-Splines
 */
int itkBSplineScatteredDataPointSetToImageFilterTest2( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << argv[0] << "outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ParametricDimension = 1;
  const unsigned int DataDimension = 3;

  typedef double                                              RealType;
  typedef unsigned char                                       OutputPixelType;
  typedef itk::Vector< RealType, DataDimension >              VectorType;
  typedef itk::Vector< OutputPixelType, DataDimension >       OutputVectorType;
  typedef itk::Image< VectorType, ParametricDimension >       ImageType;
  typedef itk::Image< OutputVectorType, ParametricDimension>  OutputImageType;

  typedef itk::PointSet<VectorType, ParametricDimension> PointSetType;

  PointSetType::Pointer pointSet = PointSetType::New();

  // Sample the helix
  for ( RealType t = 0.0; t <= 1.0+1e-10; t += 0.05 )
    {
    unsigned long i = pointSet->GetNumberOfPoints();

    PointSetType::PointType point;
    point[0] = t;
    pointSet->SetPoint( i, point );

    VectorType V;
    V[0] = 0.25 * std::cos(t*6.0*3.141);
    V[1] = 0.25 * std::sin(t*6.0*3.141);
    V[2] = 4.00 * t;

    pointSet->SetPointData( i, V );
    }

  // Instantiate the filter and set the parameters
  typedef itk::BSplineScatteredDataPointSetToImageFilter
    <PointSetType, ImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, BSplineScatteredDataPointSetToImageFilter,
    PointSetToImageFilter );

  // Define the parametric domain
  ImageType::SpacingType spacing;
  spacing.Fill( 0.01 );
  ImageType::SizeType size;
  size.Fill( 101 );
  ImageType::PointType origin;
  origin.Fill( 0.0 );

  filter->SetSize( size );
  filter->SetOrigin( origin );
  filter->SetSpacing( spacing );
  filter->SetInput( pointSet );

  FilterType::RealType bSplineEpsilon = 1e-4;
  filter->SetBSplineEpsilon( bSplineEpsilon );
  TEST_SET_GET_VALUE( bSplineEpsilon, filter->GetBSplineEpsilon() );

  filter->SetSplineOrder( 3 );
  FilterType::ArrayType ncps;
  ncps.Fill( 4 );
  filter->SetNumberOfControlPoints( ncps );
  filter->SetNumberOfLevels( 5 );
  filter->SetGenerateOutputImage( false );

  FilterType::WeightsContainerType::Pointer pointWeights =
    FilterType::WeightsContainerType::New();

  pointWeights->Initialize();

  unsigned int abritrarySize = filter->GetInput()->GetNumberOfPoints() - 1;
  pointWeights->resize( abritrarySize );
  for( unsigned int i = 0; i < pointWeights->Size(); ++i )
    {
      pointWeights->SetElement( i, 2 );
    }
  filter->SetPointWeights( pointWeights );

  TRY_EXPECT_EXCEPTION( filter->Update() );

  pointWeights->resize( filter->GetInput()->GetNumberOfPoints() );
  for( unsigned int i = 0; i < pointWeights->Size(); ++i )
    {
      pointWeights->SetElement( i, 2 );
    }
  filter->SetPointWeights( pointWeights );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  // Cast the PhiLattice
  typedef itk::CastImageFilter< FilterType::PointDataImageType, OutputImageType >
    CastImageFilterType;
  CastImageFilterType::Pointer caster = CastImageFilterType::New();
  caster->SetInput( filter->GetPhiLattice() );

  // Write the PhiLattice
  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[1] );
  writer->SetInput( caster->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
