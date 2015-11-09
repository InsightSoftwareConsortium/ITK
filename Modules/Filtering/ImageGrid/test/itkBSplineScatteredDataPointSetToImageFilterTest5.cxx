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

#include "itkPointSet.h"

#include "itkBSplineScatteredDataPointSetToImageFilter.h"

/**
 * In this test, we approximate a sequence of 3D points with a
 * parametric curve described by B-Splines.  Specifically, we
 * create 3-D trefoil knot parametric surface:
 *  https://en.wikipedia.org/wiki/Trefoil_knot
 * which is closed in both parametric dimensions.
 */
int
itkBSplineScatteredDataPointSetToImageFilterTest5( int, char * [] )
{

  const unsigned int ParametricDimension = 2;
  const unsigned int DataDimension = 3;

  typedef double                                         RealType;
  typedef itk::Vector<RealType, DataDimension>           VectorType;
  typedef itk::Image<VectorType, ParametricDimension>    ImageType;

  typedef itk::PointSet<VectorType, ParametricDimension> PointSetType;

  PointSetType::Pointer pointSet = PointSetType::New();

  std::cout << "Input Data" << std::endl;

  // Sample the trefoil knot.
  // The first parametric dimension, u,  is doing the knot part
  // whereas the second dimension, v, is going around in a simple circle.
for( RealType u = -2.0 * vnl_math::pi; u <= 2.0 * vnl_math::pi; u += 0.1 )
  {
  for( RealType v = -vnl_math::pi; v <= vnl_math::pi; v += 0.1 )
    {
    PointSetType::PointType point;
    point[0] = ( u + 2.0 * vnl_math::pi ) / ( 4.0 * vnl_math::pi );
    point[1] = ( v + vnl_math::pi ) / ( 2.0 * vnl_math::pi );
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

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
