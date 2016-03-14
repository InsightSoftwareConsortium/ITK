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

#include "itkDisplacementFieldToBSplineImageFilter.h"

int itkDisplacementFieldToBSplineImageFilterTest( int, char * [] )
{
  const unsigned int   ImageDimension = 2;

  typedef itk::Vector<float, ImageDimension>         VectorType;
  typedef itk::Image<VectorType, ImageDimension>     DisplacementFieldType;
  typedef itk::PointSet<VectorType, ImageDimension>  PointSetType;

  // Create a displacement field
  DisplacementFieldType::PointType     origin;
  DisplacementFieldType::SpacingType   spacing;
  DisplacementFieldType::SizeType      size;
  DisplacementFieldType::DirectionType direction;

  direction.SetIdentity();
  origin.Fill( 0.0 );
  spacing.Fill( 0.5 );
  size.Fill( 100 );

  VectorType ones( 1 );

  DisplacementFieldType::Pointer field = DisplacementFieldType::New();
  field->SetOrigin( origin );
  field->SetSpacing( spacing );
  field->SetRegions( size );
  field->SetDirection( direction );
  field->Allocate();
  field->FillBuffer( ones );

  typedef itk::DisplacementFieldToBSplineImageFilter
    <DisplacementFieldType, PointSetType>                   BSplineFilterType;
  typedef BSplineFilterType::RealImageType                  RealImageType;

  RealImageType::Pointer confidenceImage = RealImageType::New();
  confidenceImage->CopyInformation( field );
  confidenceImage->SetRegions( size );
  confidenceImage->Allocate();
  confidenceImage->FillBuffer( 1.0 );

  PointSetType::Pointer pointSet = PointSetType::New();
  pointSet->Initialize();

  VectorType ones_points( 1.0 );

  // Assign some random points within the b-spline domain
  PointSetType::PointType point1;
  point1[0] = 23.75;
  point1[1] = 5.125;
  pointSet->SetPoint( 0, point1 );
  pointSet->SetPointData( 0, ones_points );

  PointSetType::PointType point2;
  point2[0] = 1.75;
  point2[1] = 45.125;
  pointSet->SetPoint( 1, point2 );
  pointSet->SetPointData( 1, ones_points );

  PointSetType::PointType point3;
  point3[0] = 45.75;
  point3[1] = 2.125;
  pointSet->SetPoint( 2, point3 );
  pointSet->SetPointData( 2, ones_points );

  BSplineFilterType::ArrayType numberOfControlPoints;
  numberOfControlPoints.Fill( 4 );

  BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetDisplacementField( field );
  bspliner->SetConfidenceImage( confidenceImage );
  bspliner->SetPointSet( pointSet );
  bspliner->SetUseInputFieldToDefineTheBSplineDomain( true );
  bspliner->SetNumberOfControlPoints( numberOfControlPoints );
  bspliner->SetSplineOrder( 3 );
  bspliner->SetNumberOfFittingLevels( 8 );
  bspliner->EnforceStationaryBoundaryOff();
  bspliner->EnforceStationaryBoundaryOn();
  bspliner->SetEnforceStationaryBoundary( false );
  bspliner->EstimateInverseOff();
  bspliner->EstimateInverseOn();
  bspliner->SetEstimateInverse( false );
  bspliner->Update();
  std::cout << "spline order: " << bspliner->GetSplineOrder() << std::endl;
  std::cout << "number of control points: " << bspliner->GetNumberOfControlPoints() << std::endl;
  std::cout << "number of fitting levels: " << bspliner->GetNumberOfFittingLevels() << std::endl;
  std::cout << "enforce stationary boundary: " << bspliner->GetEnforceStationaryBoundary() << std::endl;
  std::cout << "estimate inverse: " << bspliner->GetEstimateInverse() << std::endl;

  try
    {
    bspliner->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    }

  DisplacementFieldType::IndexType index;
  index[0] = 50;
  index[1] = 50;

  VectorType v = bspliner->GetOutput()->GetPixel( index );

  if( itk::Math::abs( v.GetNorm() - 1.414214 ) >= 0.01 )
    {
    std::cerr << "Failed to find the correct forward displacement vector." << std::endl;
    return EXIT_FAILURE;
    }

  bspliner->SetNumberOfControlPoints( numberOfControlPoints );
  bspliner->SetSplineOrder( 3 );
  bspliner->SetNumberOfFittingLevels( 5 );
  bspliner->EnforceStationaryBoundaryOff();
  bspliner->EnforceStationaryBoundaryOn();
  bspliner->SetEnforceStationaryBoundary( false );
  bspliner->SetEstimateInverse( true );
  bspliner->Update();

  v = bspliner->GetOutput()->GetPixel( index );

  if( itk::Math::abs( v.GetNorm() - 1.414214 ) >= 0.01 )
    {
    std::cerr << "Failed to find the correct inverse displacement vector." << std::endl;
    return EXIT_FAILURE;
    }

  bspliner->Print( std::cout, 3 );


  /** do a second run using only the point set. */

  BSplineFilterType::Pointer bspliner2 = BSplineFilterType::New();
  bspliner2->SetPointSet( pointSet );
  bspliner2->SetUseInputFieldToDefineTheBSplineDomain( false );
  bspliner2->SetBSplineDomainFromImage( field );
  bspliner2->SetNumberOfControlPoints( numberOfControlPoints );
  bspliner2->SetSplineOrder( 3 );
  bspliner2->SetNumberOfFittingLevels( 8 );
  bspliner2->EnforceStationaryBoundaryOff();
  bspliner2->EnforceStationaryBoundaryOn();
  bspliner2->SetEnforceStationaryBoundary( false );
  bspliner2->EstimateInverseOff();
  bspliner2->EstimateInverseOn();
  bspliner2->SetEstimateInverse( false );
  bspliner2->Update();

  try
    {
    bspliner2->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    }

  return EXIT_SUCCESS;
}
