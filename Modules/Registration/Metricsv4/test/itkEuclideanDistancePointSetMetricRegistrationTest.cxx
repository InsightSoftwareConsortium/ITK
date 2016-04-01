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

#include "itkEuclideanDistancePointSetToPointSetMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkAffineTransform.h"
#include "itkCommand.h"
#include "itkMath.h"

template<typename TFilter>
class itkEuclideanDistancePointSetMetricRegistrationTestCommandIterationUpdate : public itk::Command
{
public:
  typedef itkEuclideanDistancePointSetMetricRegistrationTestCommandIterationUpdate   Self;

  typedef itk::Command             Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );

protected:
  itkEuclideanDistancePointSetMetricRegistrationTestCommandIterationUpdate() {};

public:

  virtual void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *) caller, event);
    }

  virtual void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      {
      return;
      }
    const TFilter *optimizer = dynamic_cast< const TFilter * >( object );

    if( !optimizer )
      {
      itkGenericExceptionMacro( "Error dynamic_cast failed" );
      }
    std::cout << "It: " << optimizer->GetCurrentIteration() << " metric value: " << optimizer->GetCurrentMetricValue();
    std::cout << std::endl;
    }
};

// Transform type
typedef itk::Transform<double, 2, 2> itkEuclideanDistancePointSetMetricRegistrationTestTransformType;

/////////////////////////////////////////////////////////
template< typename TTransform, typename TMetric, typename TPointSet >
int itkEuclideanDistancePointSetMetricRegistrationTestRun(
  unsigned int numberOfIterations, double maximumPhysicalStepSize, double pointMax,
  typename TTransform::Pointer & transform, typename TMetric::Pointer & metric )
{
  typedef TPointSet                        PointSetType;
  typedef typename PointSetType::PointType PointType;
  typedef typename PointType::CoordRepType CoordRepType;

  typename PointSetType::Pointer fixedPoints = PointSetType::New();
  fixedPoints->Initialize();

  typename PointSetType::Pointer movingPoints = PointSetType::New();
  movingPoints->Initialize();

  // Create a few points and apply a small rotation to make the moving point set

  float theta = itk::Math::pi / static_cast<float>(180.0) * static_cast<float>(1.0);
  PointType fixedPoint;
  fixedPoint[0] = static_cast<CoordRepType>( 0.0 );
  fixedPoint[1] = static_cast<CoordRepType>( 0.0 );
  fixedPoints->SetPoint( 0, fixedPoint );
  fixedPoint[0] = pointMax;
  fixedPoint[1] = static_cast<CoordRepType>( 0.0 );
  fixedPoints->SetPoint( 1, fixedPoint );
  fixedPoint[0] = pointMax;
  fixedPoint[1] = pointMax;
  fixedPoints->SetPoint( 2, fixedPoint );
  fixedPoint[0] = static_cast<CoordRepType>( 0.0 );
  fixedPoint[1] = pointMax;
  fixedPoints->SetPoint( 3, fixedPoint );
  fixedPoint[0] = pointMax / static_cast<CoordRepType>( 2.0 );
  fixedPoint[1] = pointMax / static_cast<CoordRepType>( 2.0 );
  fixedPoints->SetPoint( 4, fixedPoint );
  unsigned int numberOfPoints = fixedPoints->GetNumberOfPoints();

  PointType movingPoint;
  for( unsigned int n=0; n < numberOfPoints; n ++ )
    {
    fixedPoint = fixedPoints->GetPoint( n );
    movingPoint[0] = fixedPoint[0] * std::cos( theta ) - fixedPoint[1] * std::sin( theta );
    movingPoint[1] = fixedPoint[0] * std::sin( theta ) + fixedPoint[1] * std::cos( theta );
    movingPoints->SetPoint( n, movingPoint );
    std::cout << fixedPoint << " -> " << movingPoint << std::endl;
    }

  // Finish setting up the metric
  metric->SetFixedPointSet( fixedPoints );
  metric->SetMovingPointSet( movingPoints );
  metric->SetMovingTransform( transform );
  metric->Initialize();

  // scales estimator
  typedef itk::RegistrationParameterScalesFromPhysicalShift< TMetric > RegistrationParameterScalesFromShiftType;
  typename RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric( metric );
  // needed with pointset metrics
  shiftScaleEstimator->SetVirtualDomainPointSet( metric->GetVirtualTransformedPointSet() );

  // optimizer
  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  typename OptimizerType::Pointer  optimizer = OptimizerType::New();
  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetScalesEstimator( shiftScaleEstimator );
  optimizer->SetMaximumStepSizeInPhysicalUnits( maximumPhysicalStepSize );

  typedef itkEuclideanDistancePointSetMetricRegistrationTestCommandIterationUpdate<OptimizerType> CommandType;
  typename CommandType::Pointer observer = CommandType::New();
  //optimizer->AddObserver( itk::IterationEvent(), observer );

  // start
  optimizer->StartOptimization();

  std::cout << "numberOfIterations: " << numberOfIterations << std::endl;
  std::cout << "maximumPhysicalStepSize: " << maximumPhysicalStepSize << std::endl;
  std::cout << "Optimizer scales: " << optimizer->GetScales() << std::endl;
  std::cout << "Optimizer learning rate: " << optimizer->GetLearningRate() << std::endl;
  std::cout << "Moving-source final value: " << optimizer->GetCurrentMetricValue() << std::endl;
  if( transform->GetTransformCategory() == TTransform::DisplacementField )
    {
    std::cout << "local-support transform non-zero parameters: " << std::endl;
    typename TTransform::ParametersType params = transform->GetParameters();
    for( itk::SizeValueType n = 0; n < transform->GetNumberOfParameters(); n += transform->GetNumberOfLocalParameters() )
      {
      typename TTransform::ParametersValueType zero = itk::NumericTraits<typename TTransform::ParametersValueType>::ZeroValue();
      if( itk::Math::NotExactlyEquals(params[n], zero) && itk::Math::NotExactlyEquals(params[n+1], zero) )
        {
        std::cout << n << ", " << n+1 << " : " << params[n] << ", " << params[n+1] << std::endl;
        }
      }
    }
  else
    {
    std::cout << "Moving-source final position: " << optimizer->GetCurrentPosition() << std::endl;
    }

  // applying the resultant transform and verify result
  std::cout << "Fixed\tMoving\tMovingTransformed\tFixedTransformed\tDiff" << std::endl;
  bool passed = true;
  typename PointType::ValueType tolerance = static_cast<typename PointType::ValueType>( 1e-4 );
  typename TTransform::InverseTransformBasePointer fixedInverse = metric->GetFixedTransform()->GetInverseTransform();
  for( unsigned int n=0; n < numberOfPoints; n++ )
    {
    // compare the points in moving domain so we don't have to worry about an inverse
    // of the displacement field transform
    PointType transformedFixedPoint = fixedInverse->TransformPoint( fixedPoints->GetPoint( n ) );
    transformedFixedPoint = metric->GetMovingTransform()->TransformPoint( transformedFixedPoint );
    PointType difference;
    movingPoint = movingPoints->GetPoint( n );
    difference[0] = movingPoint[0] - transformedFixedPoint[0];
    difference[1] = movingPoint[1] - transformedFixedPoint[1];
    std::cout << fixedPoints->GetPoint( n ) << "\t" << movingPoint
              << "\t" << transformedFixedPoint << "\t" << difference << std::endl;
    if( fabs( difference[0] ) > tolerance || fabs( difference[1] ) > tolerance )
      {
      passed = false;
      }
    }
  if( ! passed )
    {
    std::cerr << "Results do not match truth within tolerance." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

////////////////////////////////////////////////////////////////////////////////
int itkEuclideanDistancePointSetMetricRegistrationTest( int argc, char *argv[] )
{
  const unsigned int Dimension = 2;

  int finalResult = EXIT_SUCCESS;

  unsigned int numberOfIterations = 100;
  double maximumPhysicalStepSize = static_cast<double>( 2.0 );
  if( argc > 1 )
    {
    numberOfIterations = atoi( argv[1] );
    }
  if( argc > 2 )
    {
    maximumPhysicalStepSize = atof( argv[2] );
    }

  double pointMax = static_cast<double>( 100.0 );

  //
  // Test with affine transform
  //

  // metric
  typedef itk::PointSet<unsigned char, Dimension> PointSetType;
  typedef itk::EuclideanDistancePointSetToPointSetMetricv4<PointSetType> PointSetMetricType;
  PointSetMetricType::Pointer metric = PointSetMetricType::New();

  // transform
  typedef itk::AffineTransform<double, Dimension> AffineTransformType;
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  std::cout << "XX Test with affine transform: " << std::endl;
  int oneResult = itkEuclideanDistancePointSetMetricRegistrationTestRun<AffineTransformType, PointSetMetricType, PointSetType>
    ( numberOfIterations, maximumPhysicalStepSize, pointMax, affineTransform, metric );
  if( oneResult == EXIT_FAILURE )
    {
    finalResult = EXIT_FAILURE;
    std::cerr << "Failed for affine transform." << std::endl;
    }

  //
  //Displacement field transform
  //

  typedef itk::DisplacementFieldTransform<double, Dimension> DisplacementFieldTransformType;
  DisplacementFieldTransformType::Pointer displacementTransform = DisplacementFieldTransformType::New();

  // Setup the physical space to match the point set virtual domain,
  // which is defined by the fixed point set since the fixed transform
  // is identity.
  typedef DisplacementFieldTransformType::DisplacementFieldType FieldType;
  typedef FieldType::RegionType                                 RegionType;
  typedef DisplacementFieldTransformType::ScalarType            RealType;

  FieldType::SpacingType spacing;
  spacing.Fill( static_cast<RealType>( 1.0 ) );

  FieldType::DirectionType direction;
  direction.Fill( static_cast<RealType>( 0.0 ) );
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    direction[d][d] = static_cast<RealType>( 1.0 );
    }

  FieldType::PointType origin;
  origin.Fill( static_cast<RealType>( 0.0 ) );

  RegionType::SizeType regionSize;
  regionSize.Fill( static_cast<itk::SizeValueType>(pointMax) + 1 );

  RegionType::IndexType regionIndex;
  regionIndex.Fill( 0 );

  RegionType region;
  region.SetSize( regionSize );
  region.SetIndex( regionIndex );

  FieldType::Pointer displacementField = FieldType::New();
  displacementField->SetOrigin( origin );
  displacementField->SetDirection( direction );
  displacementField->SetSpacing( spacing );
  displacementField->SetRegions( region );
  displacementField->Allocate();
  DisplacementFieldTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( static_cast<RealType>( 0.0 ) );
  displacementField->FillBuffer( zeroVector );
  displacementTransform->SetDisplacementField( displacementField );

  // metric
  typedef itk::EuclideanDistancePointSetToPointSetMetricv4<PointSetType> PointSetMetricType;
  PointSetMetricType::Pointer metric2 = PointSetMetricType::New();
  //If we don't set the virtual domain when using a displacement field transform, the
  // metric takes it from the transform during initialization.
  //metric2->SetVirtualDomain( spacing, origin, direction, region );

  std::cout << "XX Testing with displacement field transform." << std::endl;
  oneResult = itkEuclideanDistancePointSetMetricRegistrationTestRun<DisplacementFieldTransformType, PointSetMetricType, PointSetType>
    ( numberOfIterations, maximumPhysicalStepSize, pointMax, displacementTransform, metric2 );
  if( oneResult == EXIT_FAILURE )
    {
    finalResult = EXIT_FAILURE;
    std::cerr << "Failed for displacement transform." << std::endl;
    }

  return finalResult;
}
