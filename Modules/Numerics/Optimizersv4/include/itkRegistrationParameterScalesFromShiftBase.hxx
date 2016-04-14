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
#ifndef itkRegistrationParameterScalesFromShiftBase_hxx
#define itkRegistrationParameterScalesFromShiftBase_hxx

#include "itkRegistrationParameterScalesFromShiftBase.h"

namespace itk
{

template< typename TMetric >
RegistrationParameterScalesFromShiftBase< TMetric >
::RegistrationParameterScalesFromShiftBase()
{
  this->m_SmallParameterVariation = 0.01;
}

/** Compute parameter scales */
template< typename TMetric >
void
RegistrationParameterScalesFromShiftBase< TMetric >
::EstimateScales(ScalesType &parameterScales)
{
  this->CheckAndSetInputs();
  this->SetScalesSamplingStrategy();
  this->SampleVirtualDomain();

  const SizeValueType numAllPara = this->GetTransform()->GetNumberOfParameters();
  const SizeValueType numLocalPara = this->GetNumberOfLocalParameters();

  parameterScales.SetSize(numLocalPara);

  FloatType maxShift;

  ParametersType deltaParameters(numAllPara);

  // minNonZeroShift: the minimum non-zero shift.
  FloatType minNonZeroShift = NumericTraits<FloatType>::max();

  OffsetValueType offset = 0;

  if( this->IsDisplacementFieldTransform() )
    {
    //FIXME: remove if we end up not using this
    //if( this->MetricIsPointSetToPointSetType() )
    if (this->GetSamplingStrategy() == Superclass::VirtualDomainPointSetSampling)
      {
      // Use the first virtual point since the center of the virtual domain
      // may not line up with a sample point.
      offset = this->m_Metric->ComputeParameterOffsetFromVirtualPoint( this->m_SamplePoints[0], numLocalPara );
      }
    else
      {
      VirtualIndexType centralIndex = this->GetVirtualDomainCentralIndex();
      offset = this->m_Metric->ComputeParameterOffsetFromVirtualIndex( centralIndex, numLocalPara );
      }
    }

  // compute voxel shift generated from each transform parameter
  for (SizeValueType i=0; i<numLocalPara; i++)
    {
    // For local support, we need to refill deltaParameters with zeros at each loop
    // since smoothing may change the values around the local voxel.
    deltaParameters.Fill(NumericTraits< typename ParametersType::ValueType >::ZeroValue());
    deltaParameters[offset + i] = this->m_SmallParameterVariation;

    maxShift = this->ComputeMaximumVoxelShift(deltaParameters);

    parameterScales[i] = maxShift;
    if ( maxShift > NumericTraits<FloatType>::epsilon() && maxShift < minNonZeroShift )
      {
      minNonZeroShift = maxShift;
      }
    }

  if (Math::ExactlyEquals(minNonZeroShift, NumericTraits<FloatType>::max()))
    {
    itkWarningMacro(  << "Variation in any parameter won't change a voxel position. The default scales (1.0) are used to avoid division-by-zero." );
    parameterScales.Fill(NumericTraits< typename ScalesType::ValueType >::OneValue());
    }
  else
    {
    if( this->IsBSplineTransform() )
      {
      parameterScales.Fill( minNonZeroShift );
      }
    else
      {
      for (SizeValueType i=0; i<numLocalPara; i++)
        {
        if (parameterScales[i] <= NumericTraits<FloatType>::epsilon())
          {
          // To avoid division-by-zero in optimizers, assign a small value for a zero scale.
          parameterScales[i] = minNonZeroShift * minNonZeroShift;
          }
        else
          {
          parameterScales[i] *= parameterScales[i];
          }
        //normalize to unit variation
        parameterScales[i] *= NumericTraits< typename ScalesType::ValueType >::OneValue() / itk::Math::sqr( this->m_SmallParameterVariation );
        }
      }
    }
}

/** Compute the scale for a step. For transform T(x + t * step), the scale
 * w.r.t. the step is the shift produced by step.
 */
template< typename TMetric >
typename RegistrationParameterScalesFromShiftBase< TMetric >::FloatType
RegistrationParameterScalesFromShiftBase< TMetric >
::EstimateStepScale(const ParametersType &step)
{
  this->CheckAndSetInputs();
  this->SetStepScaleSamplingStrategy();
  this->SampleVirtualDomain();

  if( this->TransformHasLocalSupportForScalesEstimation() )
    {
    return this->ComputeMaximumVoxelShift(step);
    }

  // For global transforms, we want a linear approximation of the function
  // of step scale w.r.t "step". This is true only when "step" is close to
  // zero. Therefore, we need to scale "step" down.
  FloatType maxStep = NumericTraits<FloatType>::ZeroValue();
  for (typename ParametersType::SizeValueType p = 0; p < step.GetSize(); p++)
    {
    if (maxStep < std::abs(step[p]))
      {
      maxStep = std::abs(step[p]);
      }
    }
  if (maxStep <= NumericTraits<FloatType>::epsilon())
    {
    return NumericTraits<FloatType>::ZeroValue();
    }
  else
    {
    FloatType factor = this->m_SmallParameterVariation / maxStep;
    ParametersType smallStep(step.size());
    //Use a small step to have a linear approximation.
    smallStep = step * factor;
    return this->ComputeMaximumVoxelShift(smallStep) / factor;
    }
}

/**
 * Estimate the scales of local steps.
 */
template< typename TMetric >
void
RegistrationParameterScalesFromShiftBase< TMetric >
::EstimateLocalStepScales(const ParametersType &step, ScalesType &localStepScales)
{
  if( !this->TransformHasLocalSupportForScalesEstimation() )
    {
    itkExceptionMacro( "EstimateLocalStepScales: the transform doesn't have local support (displacement field or b-spline)." );
    }

  this->CheckAndSetInputs();
  this->SetStepScaleSamplingStrategy();
  this->SampleVirtualDomain();

  ScalesType sampleShifts;
  this->ComputeSampleShifts(step, sampleShifts);

  const SizeValueType numAllPara = this->GetTransform()->GetNumberOfParameters();
  const SizeValueType numPara = this->GetNumberOfLocalParameters();
  const SizeValueType numLocals = numAllPara / numPara;

  localStepScales.SetSize(numLocals);
  localStepScales.Fill(NumericTraits<typename ScalesType::ValueType>::ZeroValue());

  const SizeValueType numSamples = static_cast< const SizeValueType >( this->m_SamplePoints.size() );
  for (SizeValueType c=0; c<numSamples; c++)
    {
    VirtualPointType &point = this->m_SamplePoints[c];
    IndexValueType localId = this->m_Metric->ComputeParameterOffsetFromVirtualPoint( point, numPara) / numPara;
    localStepScales[localId] = sampleShifts[c];
    }
}

/**
 * Compute the maximum shift when a transform is changed with deltaParameters
 */
template< typename TMetric >
typename RegistrationParameterScalesFromShiftBase< TMetric >::FloatType
RegistrationParameterScalesFromShiftBase< TMetric >
::ComputeMaximumVoxelShift(const ParametersType &deltaParameters)
{
  ScalesType sampleShifts;

  this->ComputeSampleShifts(deltaParameters, sampleShifts);

  FloatType maxShift = NumericTraits< FloatType >::ZeroValue();
  for (SizeValueType s=0; s<sampleShifts.size(); s++)
    {
    if (maxShift < sampleShifts[s])
      {
      maxShift = sampleShifts[s];
      }
    }

  return maxShift;
}

/** Print the information about this class */
template< typename TMetric >
void
RegistrationParameterScalesFromShiftBase< TMetric >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

}  // namespace itk

#endif /* itkRegistrationParameterScalesFromShiftBase_hxx */
