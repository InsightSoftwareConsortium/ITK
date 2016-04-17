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
#ifndef itkTimeVaryingVelocityFieldIntegrationImageFilter_hxx
#define itkTimeVaryingVelocityFieldIntegrationImageFilter_hxx

#include "itkTimeVaryingVelocityFieldIntegrationImageFilter.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{

/*
 * TimeVaryingVelocityFieldIntegrationImageFilter class definitions
 */
template<typename TTimeVaryingVelocityField, typename TDisplacementField>
TimeVaryingVelocityFieldIntegrationImageFilter
  <TTimeVaryingVelocityField, TDisplacementField>
::TimeVaryingVelocityFieldIntegrationImageFilter()
{
  this->m_LowerTimeBound =  0.0,
  this->m_UpperTimeBound = 1.0,
  this->m_NumberOfIntegrationSteps = 100;
  this->m_NumberOfTimePoints = 0;
  this->SetNumberOfRequiredInputs( 1 );

  if( InputImageDimension - 1 != OutputImageDimension )
    {
    itkExceptionMacro( "The time-varying velocity field (input) should have "
      << "dimensionality of 1 greater than the deformation field (output). " );
    }

  typedef VectorLinearInterpolateImageFunction<TimeVaryingVelocityFieldType,
    ScalarType> DefaultVelocityFieldInterpolatorType;

  typename DefaultVelocityFieldInterpolatorType::Pointer
    velocityFieldInterpolator = DefaultVelocityFieldInterpolatorType::New();

  this->m_VelocityFieldInterpolator = velocityFieldInterpolator;

  typedef VectorLinearInterpolateImageFunction<DisplacementFieldType,
    ScalarType> DefaultDisplacementFieldInterpolatorType;

  typename DefaultDisplacementFieldInterpolatorType::Pointer
    deformationFieldInterpolator = DefaultDisplacementFieldInterpolatorType::New();

  this->m_DisplacementFieldInterpolator = deformationFieldInterpolator;
}

template<typename TTimeVaryingVelocityField, typename TDisplacementField>
TimeVaryingVelocityFieldIntegrationImageFilter
  <TTimeVaryingVelocityField, TDisplacementField>
::~TimeVaryingVelocityFieldIntegrationImageFilter()
{
}

template<typename TTimeVaryingVelocityField, typename TDisplacementField>
void
TimeVaryingVelocityFieldIntegrationImageFilter
  <TTimeVaryingVelocityField, TDisplacementField>
::GenerateOutputInformation()
{
  const TimeVaryingVelocityFieldType * input = this->GetInput();
  DisplacementFieldType * output = this->GetOutput();
  this->m_NumberOfTimePoints = input->GetLargestPossibleRegion().GetSize()[OutputImageDimension];
  if( !input || !output )
    {
    return;
    }

  //
  // The ImageBase::CopyInformation() method ca not be used here
  // because these two images have different dimensions. Therefore
  // the individual elements must be copied for the common dimensions.
  //
  typedef typename DisplacementFieldType::SizeType      SizeType;
  typedef typename DisplacementFieldType::SpacingType   SpacingType;
  typedef typename DisplacementFieldType::PointType     OriginType;
  typedef typename DisplacementFieldType::DirectionType DirectionType;

  SizeType size;
  SpacingType spacing;
  OriginType origin;
  DirectionType direction;

  typedef typename TimeVaryingVelocityFieldType::SizeType       InputSizeType;
  typedef typename TimeVaryingVelocityFieldType::SpacingType    InputSpacingType;
  typedef typename TimeVaryingVelocityFieldType::PointType      InputOriginType;
  typedef typename TimeVaryingVelocityFieldType::DirectionType  InputDirectionType;
  typedef typename TimeVaryingVelocityFieldType::RegionType     InputRegionType;

  const InputSpacingType & inputSpacing = input->GetSpacing();
  const InputOriginType & inputOrigin = input->GetOrigin();
  const InputDirectionType & inputDirection = input->GetDirection();
  const InputRegionType requestedRegion = input->GetRequestedRegion();
  const InputSizeType requestedSize = requestedRegion.GetSize();

  for( unsigned int i = 0; i < OutputImageDimension; i++ )
    {
    size[i] = requestedSize[i];
    spacing[i] = inputSpacing[i];
    origin[i] = inputOrigin[i];

    for( unsigned int j = 0; j < OutputImageDimension; j++ )
      {
      direction[i][j] = inputDirection[i][j];
      }
    }

  output->SetOrigin( origin );
  output->SetSpacing( spacing );
  output->SetDirection( direction );
  output->SetRegions( size );
}

template<typename TTimeVaryingVelocityField, typename TDisplacementField>
void
TimeVaryingVelocityFieldIntegrationImageFilter
  <TTimeVaryingVelocityField, TDisplacementField>
::BeforeThreadedGenerateData()
{
  this->m_VelocityFieldInterpolator->SetInputImage( this->GetInput() );
  this->m_NumberOfTimePoints = this->GetInput()->GetLargestPossibleRegion().GetSize()[InputImageDimension-1];
  if( !this->m_InitialDiffeomorphism.IsNull() )
    {
    this->m_DisplacementFieldInterpolator->SetInputImage( this->m_InitialDiffeomorphism );
    }
}

template<typename TTimeVaryingVelocityField, typename TDisplacementField>
void
TimeVaryingVelocityFieldIntegrationImageFilter
  <TTimeVaryingVelocityField, TDisplacementField>
::ThreadedGenerateData( const OutputRegionType &region, ThreadIdType itkNotUsed( threadId ) )
{
  if( Math::ExactlyEquals( this->m_LowerTimeBound, this->m_UpperTimeBound ) )
    {
    return;
    }

  if( this->m_NumberOfIntegrationSteps == 0 )
    {
    return;
    }

  const TimeVaryingVelocityFieldType * inputField = this->GetInput();

  typename DisplacementFieldType::Pointer outputField = this->GetOutput();

  ImageRegionIteratorWithIndex<DisplacementFieldType> It( outputField, region );

  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    PointType point;
    outputField->TransformIndexToPhysicalPoint( It.GetIndex(), point );
    VectorType displacement = this->IntegrateVelocityAtPoint( point, inputField );
    It.Set( displacement );
    }
}

template<typename TTimeVaryingVelocityField, typename TDisplacementField>
typename TimeVaryingVelocityFieldIntegrationImageFilter
  <TTimeVaryingVelocityField, TDisplacementField>::VectorType
TimeVaryingVelocityFieldIntegrationImageFilter
  <TTimeVaryingVelocityField, TDisplacementField>
::IntegrateVelocityAtPoint( const PointType & initialSpatialPoint,
                            const TimeVaryingVelocityFieldType *inputField )
{
  // Solve the initial value problem using fourth-order Runge-Kutta
  //    y' = f(t, y), y(t_0) = y_0

  VectorType zeroVector;
  zeroVector.Fill( 0.0 );

  // Initial conditions

  PointType startingSpatialPoint = initialSpatialPoint;
  if( !this->m_InitialDiffeomorphism.IsNull() )
    {
    if( this->m_DisplacementFieldInterpolator->IsInsideBuffer( startingSpatialPoint ) )
      {
      startingSpatialPoint += this->m_DisplacementFieldInterpolator->Evaluate( startingSpatialPoint );
      }
    }

  // Perform the integration
  // Need to know how to map the time dimension of the input image to the
  // assumed domain of [0,1].


  typename TimeVaryingVelocityFieldType::PointType spaceTimeOrigin = inputField->GetOrigin();

  typedef typename TimeVaryingVelocityFieldType::RegionType  RegionType;

  RegionType region = inputField->GetLargestPossibleRegion();

  typename RegionType::IndexType lastIndex = region.GetIndex();
  typename RegionType::SizeType size = region.GetSize();
  for( unsigned d = 0; d < InputImageDimension; d++ )
    {
    lastIndex[d] += ( size[d] - 1 );
    }

  typename TimeVaryingVelocityFieldType::PointType spaceTimeEnd;
  typename TimeVaryingVelocityFieldType::PointType pointIn2;
  typename TimeVaryingVelocityFieldType::PointType pointIn3;
  inputField->TransformIndexToPhysicalPoint( lastIndex, spaceTimeEnd );

  // Calculate the delta time used for integration
  const RealType deltaTime = itk::Math::abs( this->m_UpperTimeBound - this->m_LowerTimeBound ) /
    static_cast<RealType>( this->m_NumberOfIntegrationSteps );

  if( deltaTime == 0.0 )
    {
    return zeroVector;
    }

  const RealType timeOrigin = spaceTimeOrigin[InputImageDimension-1];
  const RealType timeEnd = spaceTimeEnd[InputImageDimension-1];
  const RealType timeSpan = timeEnd - timeOrigin;
  RealType timeSign = 1.0;
  if( this->m_UpperTimeBound < this->m_LowerTimeBound )
    {
    timeSign = -1.0;
    }

  VectorType displacement = zeroVector;

  RealType timePoint = timeOrigin + this->m_LowerTimeBound * timeSpan;

  RealType intervalTimePoint = ( timePoint + 1.0 ) / static_cast<RealType>( this->m_NumberOfTimePoints );

  /** Windows not registering + operation so use a loop explicitly */
  PointType spatialPoint = startingSpatialPoint;
  for( unsigned int d = 0; d < OutputImageDimension; d++ )
    {
    spatialPoint[d] += displacement[d];
    }

  for( unsigned int n = 0; n < this->m_NumberOfIntegrationSteps; n++ )
    {
    typename TimeVaryingVelocityFieldType::PointType x1;
    typename TimeVaryingVelocityFieldType::PointType x2;
    typename TimeVaryingVelocityFieldType::PointType x3;
    typename TimeVaryingVelocityFieldType::PointType x4;

    RealType intervalTimePointMinusDeltaTime = intervalTimePoint - timeSign * deltaTime;
    RealType intervalTimePointMinusHalfDeltaTime = intervalTimePoint - timeSign * deltaTime * 0.5;
    if( intervalTimePointMinusHalfDeltaTime < 0.0 )
      {
      intervalTimePointMinusHalfDeltaTime = 0.0;
      }
    if( intervalTimePointMinusHalfDeltaTime > 1.0 )
      {
      intervalTimePointMinusHalfDeltaTime = 1.0;
      }
    if( intervalTimePointMinusDeltaTime < 0.0 )
      {
      intervalTimePointMinusDeltaTime = 0.0;
      }
    if( intervalTimePointMinusDeltaTime > 1.0 )
      {
      intervalTimePointMinusDeltaTime = 1.0;
      }

    for( unsigned int d = 0; d < OutputImageDimension; d++ )
      {
      x1[d] = spatialPoint[d] + displacement[d];
      x2[d] = spatialPoint[d] + displacement[d];
      x3[d] = spatialPoint[d] + displacement[d];
      x4[d] = spatialPoint[d] + displacement[d];
      pointIn2[d] = spatialPoint[d] + displacement[d];
      }

    x1[OutputImageDimension] = intervalTimePointMinusDeltaTime * static_cast<RealType>( this->m_NumberOfTimePoints - 1 );
    x2[OutputImageDimension] = intervalTimePointMinusHalfDeltaTime * static_cast<RealType>( this->m_NumberOfTimePoints - 1 );
    x3[OutputImageDimension] = intervalTimePointMinusHalfDeltaTime * static_cast<RealType>( this->m_NumberOfTimePoints - 1 );
    x4[OutputImageDimension] = intervalTimePoint * static_cast<RealType>( this->m_NumberOfTimePoints - 1 );

    VectorType f1 = zeroVector;
    if ( this->m_VelocityFieldInterpolator->IsInsideBuffer( x1 ) )
      {
      f1 = this->m_VelocityFieldInterpolator->Evaluate( x1 );
      for( unsigned int jj = 0; jj < OutputImageDimension; jj++ )
        {
        x2[jj] += f1[jj] * deltaTime * 0.5;
        }
      }

    VectorType f2 = zeroVector;
    if ( this->m_VelocityFieldInterpolator->IsInsideBuffer( x2 ) )
      {
      f2 = this->m_VelocityFieldInterpolator->Evaluate( x2 );
      for( unsigned int jj = 0; jj < OutputImageDimension; jj++ )
        {
        x3[jj] += f2[jj] * deltaTime * 0.5;
        }
      }

    VectorType f3 = zeroVector;
    if ( this->m_VelocityFieldInterpolator->IsInsideBuffer( x3 ) )
      {
      f3 = this->m_VelocityFieldInterpolator->Evaluate( x3 );
      for( unsigned int jj = 0; jj < OutputImageDimension; jj++ )
        {
        x4[jj] += f3[jj] * deltaTime;
        }
      }

    VectorType f4 = zeroVector;
    if( this->m_VelocityFieldInterpolator->IsInsideBuffer( x4 ) )
      {
      f4 = this->m_VelocityFieldInterpolator->Evaluate( x4 );
      }

    for( unsigned int jj = 0; jj < OutputImageDimension; jj++ )
      {
      pointIn3[jj] = pointIn2[jj] + timeSign * deltaTime/6.0 * ( f1[jj] + 2.0*f2[jj] + 2.0*f3[jj] + f4[jj] );
      displacement[jj] = pointIn3[jj] - startingSpatialPoint[jj];
      }
    pointIn3[OutputImageDimension] = intervalTimePoint * static_cast<RealType>( this->m_NumberOfTimePoints - 1 );

    intervalTimePoint += deltaTime * timeSign;
    }
  return displacement;
}

template<typename TTimeVaryingVelocityField, typename TDisplacementField>
void
TimeVaryingVelocityFieldIntegrationImageFilter
  <TTimeVaryingVelocityField, TDisplacementField>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "VelocityFieldInterpolator: " << this->m_VelocityFieldInterpolator << std::endl;
  os << indent << "LowerTimeBound: " << this->m_LowerTimeBound << std::endl;
  os << indent << "UpperTimeBound: " << this->m_UpperTimeBound << std::endl;
  os << indent << "NumberOfIntegrationSteps: " << this->m_NumberOfIntegrationSteps << std::endl;

  if( !this->m_InitialDiffeomorphism.IsNull() )
    {
    os << indent << "InitialDiffeomorphism: " << this->m_InitialDiffeomorphism << std::endl;
    os << indent << "DisplacementFieldInterpolator: " << this->m_DisplacementFieldInterpolator << std::endl;
    }
}

}  //end namespace itk

#endif
