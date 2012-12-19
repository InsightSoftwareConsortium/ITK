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

  typedef itk::Vector<float, ImageDimension>       VectorType;
  typedef itk::Image<VectorType, ImageDimension>   DisplacementFieldType;

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

  typedef itk::DisplacementFieldToBSplineImageFilter<DisplacementFieldType> BSplineFilterType;
  typedef BSplineFilterType::RealImageType                                  RealImageType;

  RealImageType::Pointer confidenceImage = RealImageType::New();
  confidenceImage->CopyInformation( field );
  confidenceImage->SetRegions( size );
  confidenceImage->Allocate();
  confidenceImage->FillBuffer( 1.0 );

  BSplineFilterType::ArrayType numberOfControlPoints;
  numberOfControlPoints.Fill( 4 );

  BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetDisplacementField( field );
  bspliner->SetConfidenceImage( confidenceImage );
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

  if( vnl_math_abs( v.GetNorm() - 1.414214 ) >= 0.01 )
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

  if( vnl_math_abs( v.GetNorm() - 1.414214 ) >= 0.01 )
    {
    std::cerr << "Failed to find the correct inverse displacement vector." << std::endl;
    return EXIT_FAILURE;
    }

  bspliner->Print( std::cout, 3 );

  return EXIT_SUCCESS;
}
