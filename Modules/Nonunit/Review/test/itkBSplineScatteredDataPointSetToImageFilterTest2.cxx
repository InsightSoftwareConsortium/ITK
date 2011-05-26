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
#if defined(_MSC_VER)
#pragma warning ( disable : 4756 ) // overflow in constant arithmetic
// seems to be a bogus warning for this test
#endif

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPointSet.h"

#include "itkBSplineControlPointImageFunction.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"

/**
 * In this test, we approximate a sequence of 3D points with a
 * parametric curve described by B-Splines
 */
int
itkBSplineScatteredDataPointSetToImageFilterTest2( int argc , char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << argv[0] << "outputPointsAndTangents.txt" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ParametricDimension = 1;
  const unsigned int DataDimension = 3;

  typedef double                                         RealType;
  typedef itk::Vector<RealType, DataDimension>           VectorType;
  typedef itk::Image<VectorType, ParametricDimension>    ImageType;

  typedef itk::PointSet<VectorType, ParametricDimension> PointSetType;

  PointSetType::Pointer pointSet = PointSetType::New();

  std::cout << "Input Data" << std::endl;

  // Sample the helix.
  for ( RealType t = 0.0; t <= 1.0+1e-10; t += 0.05 )
    {
    unsigned long i = pointSet->GetNumberOfPoints();

    PointSetType::PointType point;
    point[0] = t;
    pointSet->SetPoint( i, point );

    VectorType V;
    V[0] = 0.25 * vcl_cos(t*6.0*3.141);
    V[1] = 0.25 * vcl_sin(t*6.0*3.141);
    V[2] = 4.00 * t;

    std::cout << V << std::endl;

    pointSet->SetPointData( i, V );
    }

  // Instantiate the filter and set the parameters
  typedef itk::BSplineScatteredDataPointSetToImageFilter
    <PointSetType, ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  // Define the parametric domain
  ImageType::SpacingType spacing;
  spacing.Fill( 0.001 );
  ImageType::SizeType size;
  size.Fill( 1001 );
  ImageType::PointType origin;
  origin.Fill( 0.0 );

  filter->SetSize( size );
  filter->SetOrigin( origin );
  filter->SetSpacing( spacing );
  filter->SetInput( pointSet );

  filter->SetSplineOrder( 3 );
  FilterType::ArrayType ncps;
  ncps.Fill( 4 );
  filter->SetNumberOfControlPoints( ncps );
  filter->SetNumberOfLevels( 5 );
  filter->SetGenerateOutputImage( false );

  try
    {
    filter->Update();

    typedef itk::BSplineControlPointImageFunction<ImageType> BSplinerType;
    BSplinerType::Pointer bspliner = BSplinerType::New();
    bspliner->SetSplineOrder( filter->GetSplineOrder() );
    bspliner->SetSize( filter->GetSize() );
    bspliner->SetSpacing( filter->GetSpacing() );
    bspliner->SetOrigin( filter->GetOrigin() );
    bspliner->SetInputImage( filter->GetPhiLattice() );

    std::ofstream outputFile;

    outputFile.open( argv[1] );

    PointSetType::PointType parameterPosition;
    VectorType V;
    BSplinerType::GradientType G;

    for ( RealType t = 0.0; t <= 1.0+1e-10; t += 0.01 )
      {
      parameterPosition[0] = t;

      V = bspliner->Evaluate( parameterPosition );
      G = bspliner->EvaluateGradient( parameterPosition );

      outputFile << V[0] << " " << V[1] << " " << V[2];
      outputFile << " : ";
      outputFile << G[0][0] << " " << G[1][0] << " " << G[2][0];
      outputFile << std::endl;
      }

    outputFile.close();

    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
