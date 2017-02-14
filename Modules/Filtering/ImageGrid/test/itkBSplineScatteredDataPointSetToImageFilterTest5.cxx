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
 * parametric curve described by B-Splines. Specifically, we
 * create 3-D trefoil knot parametric surface:
 *  https://en.wikipedia.org/wiki/Trefoil_knot
 * which is closed in both parametric dimensions.
 */
int itkBSplineScatteredDataPointSetToImageFilterTest5( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << argv[0] << "outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ParametricDimension = 2;
  const unsigned int DataDimension = 3;

  typedef double                                              RealType;
  typedef unsigned char                                       OutputPixelType;
  typedef itk::Vector<RealType, DataDimension>                VectorType;
  typedef itk::Vector< OutputPixelType, DataDimension >       OutputVectorType;
  typedef itk::Image<VectorType, ParametricDimension>         ImageType;
  typedef itk::Image< OutputVectorType, ParametricDimension>  OutputImageType;

  typedef itk::PointSet<VectorType, ParametricDimension> PointSetType;

  PointSetType::Pointer pointSet = PointSetType::New();

  // Sample the trefoil knot.
  // The first parametric dimension, u,  is doing the knot part
  // whereas the second dimension, v, is going around in a simple circle.
  for( RealType u = -2.0 * itk::Math::pi; u <= 2.0 * itk::Math::pi; u += 0.1 )
    {
    for( RealType v = -itk::Math::pi; v <= itk::Math::pi; v += 0.1 )
      {
      PointSetType::PointType point;
      point[0] = ( u + 2.0 * itk::Math::pi ) / ( 4.0 * itk::Math::pi );
      point[1] = ( v + itk::Math::pi ) / ( 2.0 * itk::Math::pi );
      unsigned long i = pointSet->GetNumberOfPoints();
      pointSet->SetPoint( i, point );

      VectorType V;
      V[0] = std::cos( u ) * std::cos( v ) + 3.0 * std::cos( u ) * ( 1.5 + 0.5 * std::sin( 1.5 * u ) );
      V[1] = std::sin( u ) * std::cos( v ) + 3.0 * std::sin( u ) * ( 1.5 + 0.5 * std::sin( 1.5 * u ) );
      V[2] = std::sin( v ) + 2.0 * std::cos( 1.5 * u );

      pointSet->SetPointData( i, V );
      }
    }

  // Instantiate the filter and set the parameters
  typedef itk::BSplineScatteredDataPointSetToImageFilter
    <PointSetType, ImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, BSplineScatteredDataPointSetToImageFilter,
    PointSetToImageFilter );

  // Define the parametric domain
  ImageType::SpacingType spacing;
  spacing[0] = 0.001;
  spacing[1] = 0.01;
  filter->SetSpacing( spacing );

  ImageType::SizeType size;
  size[0] = 1000;
  size[1] = 100;

  ImageType::PointType origin;
  origin.Fill( 0.0 );

  filter->SetSize( size );
  filter->SetOrigin( origin );
  filter->SetSpacing( spacing );
  filter->SetInput( pointSet );
  filter->SetSplineOrder( 3 );

  FilterType::ArrayType ncps;
  ncps[0] = 7;
  ncps[1] = 10;

  filter->SetNumberOfControlPoints( ncps );
  filter->SetNumberOfLevels( 4 );
  filter->SetGenerateOutputImage( false );

  FilterType::ArrayType close;
  close.Fill( 1 );
  filter->SetCloseDimension( close );


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
