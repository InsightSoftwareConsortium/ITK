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
#ifndef __itkRegistrationParameterScalesFromShift_hxx
#define __itkRegistrationParameterScalesFromShift_hxx

#include "itkRegistrationParameterScalesFromShift.h"

namespace itk
{

template< class TMetric >
RegistrationParameterScalesFromShift< TMetric >
::RegistrationParameterScalesFromShift()
{
  this->m_SmallParameterVariation = 0.01;
  this->m_UsePhysicalSpaceForShift = true;
}

/** Compute parameter scales */
template< class TMetric >
void
RegistrationParameterScalesFromShift< TMetric >
::EstimateScales(ScalesType &parameterScales)
{
  this->CheckAndSetInputs();
  this->SetScalesSamplingStrategy();
  this->SampleImageDomain();

  const SizeValueType numAllPara = this->GetTransform()->GetNumberOfParameters();
  const SizeValueType numPara = this->GetNumberOfScales();

  parameterScales.SetSize(numPara);

  FloatType maxShift;

  ParametersType deltaParameters(numAllPara);

  // minNonZeroShift: the minimum non-zero shift.
  FloatType minNonZeroShift = NumericTraits<FloatType>::max();

  SizeValueType offset = 0;
  if( this->HasLocalSupport() )
    {
    VirtualIndexType centralIndex = this->GetVirtualImageCentralIndex();
    VirtualImageConstPointer image = this->GetVirtualImage();
    offset = image->ComputeOffset(centralIndex) * numPara;
    }

  // compute voxel shift generated from each transform parameter
  for (SizeValueType i=0; i<numPara; i++)
    {
    // For local support, we need to refill deltaParameters with zeros at each loop
    // since smoothing may change the values around the local voxel.
    deltaParameters.Fill(NumericTraits< typename ParametersType::ValueType >::Zero);
    deltaParameters[offset + i] = m_SmallParameterVariation;
    maxShift = this->ComputeMaximumVoxelShift(deltaParameters);

    parameterScales[i] = maxShift;
    if ( maxShift > NumericTraits<FloatType>::epsilon() && maxShift < minNonZeroShift )
      {
      minNonZeroShift = maxShift;
      }
    }

  if (minNonZeroShift == NumericTraits<FloatType>::max())
    {
    itkWarningMacro(  << "Variation in any parameter won't change a voxel position."
                      << std::endl
                      << "The default scales (1.0) are used to avoid division-by-zero."
                   );
    parameterScales.Fill(NumericTraits< typename ScalesType::ValueType >::One);
    }
  else
    {
    for (SizeValueType i=0; i<numPara; i++)
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
      parameterScales[i] *= NumericTraits< typename ScalesType::ValueType >::One
        / m_SmallParameterVariation / m_SmallParameterVariation;
      }
    }

}

/** Compute the scale for a step. For transform T(x + t * step), the scale
 * w.r.t. the step is the shift produced by step.
 */
template< class TMetric >
typename RegistrationParameterScalesFromShift< TMetric >::FloatType
RegistrationParameterScalesFromShift< TMetric >
::EstimateStepScale(const ParametersType &step)
{
  this->CheckAndSetInputs();
  this->SetStepScaleSamplingStrategy();
  this->SampleImageDomain();
  return this->ComputeMaximumVoxelShift(step);
}

/**
 * Compute the maximum shift when a transform is changed with deltaParameters
 */
template< class TMetric >
typename RegistrationParameterScalesFromShift< TMetric >::FloatType
RegistrationParameterScalesFromShift< TMetric >
::ComputeMaximumVoxelShift(const ParametersType &deltaParameters)
{
  FloatType shift;
  if (this->GetTransformForward())
    {
    if (this->m_UsePhysicalSpaceForShift)
      {
      shift = this->ComputeMaximumPhysicalShiftTemplated
        <MovingTransformType>(deltaParameters);
      }
    else
      {
      shift = this->ComputeMaximumIndexShiftTemplated
        <MovingTransformType>(deltaParameters);
      }
    }
  else
    {
    if (this->m_UsePhysicalSpaceForShift)
      {
      shift = this->ComputeMaximumPhysicalShiftTemplated
        <FixedTransformType>(deltaParameters);
      }
    else
      {
      shift = this->ComputeMaximumIndexShiftTemplated
        <FixedTransformType>(deltaParameters);
      }
    }
  return shift;
}

/** The templated method of compute the maximimum shift in continous index.
 *  The template argument TTransform may be either MovingTransformType or
 *  FixedTransformType.
 */
template< class TMetric >
template< class TTransform >
typename RegistrationParameterScalesFromShift< TMetric >::FloatType
RegistrationParameterScalesFromShift< TMetric >
::ComputeMaximumIndexShiftTemplated(const ParametersType &deltaParameters)
{
  typedef itk::ContinuousIndex< FloatType, TTransform::OutputSpaceDimension >
    TransformOutputType;
  FloatType voxelShift = NumericTraits< FloatType >::Zero;

  // We save the old parameters and apply the delta parameters to calculate the
  // voxel shift. After it is done, we will reset to the old parameters.
  TransformBase *transform = const_cast<TransformBase *>(this->GetTransform());
  const ParametersType oldParameters = transform->GetParameters();

  FloatType distance;
  const SizeValueType numSamples = this->m_ImageSamples.size();

  VirtualPointType point;
  TransformOutputType newMappedVoxel;

  // Store the old mapped indices to reduce calls to Transform::SetParameters()
  std::vector<TransformOutputType> oldMappedVoxels(numSamples);

  // Compute the indices mapped by the old transform
  for (SizeValueType c=0; c<numSamples; c++)
    {
    point = this->m_ImageSamples[c];
    this->template TransformPointToContinuousIndex<TransformOutputType>(point, oldMappedVoxels[c]);
    } // end for numSamples

  // Apply the delta parameters to the transform
  this->UpdateTransformParameters(deltaParameters);

  // compute the indices mapped by the new transform
  for (SizeValueType c=0; c<numSamples; c++)
    {
    point = this->m_ImageSamples[c];
    this->template TransformPointToContinuousIndex<TransformOutputType>(point, newMappedVoxel);

    // find max shift by checking each sample point
    distance = newMappedVoxel.EuclideanDistanceTo(oldMappedVoxels[c]);
    if ( voxelShift < distance )
      {
      voxelShift = distance;
      }
  } // end for numSamples

  // restore the parameters in the transform
  transform->SetParameters(oldParameters);

  return voxelShift;
}

/** The templated method of compute the maximimum shift in the phyiscal space.
 *  The template argument TTransform may be either MovingTransformType or
 *  FixedTransformType.
 */
template< class TMetric >
template< class TTransform >
typename RegistrationParameterScalesFromShift< TMetric >::FloatType
RegistrationParameterScalesFromShift< TMetric >
::ComputeMaximumPhysicalShiftTemplated(const ParametersType &deltaParameters)
{
  typedef typename TTransform::OutputPointType TransformOutputType;
  FloatType voxelShift = NumericTraits< FloatType >::Zero;

  // We save the old parameters and apply the delta parameters to calculate the
  // voxel shift. After it is done, we will reset to the old parameters.
  TransformBase *transform = const_cast<TransformBase *>(this->GetTransform());
  const ParametersType oldParameters = transform->GetParameters();

  FloatType distance;
  const SizeValueType numSamples = this->m_ImageSamples.size();

  VirtualPointType point;
  TransformOutputType newMappedVoxel;

  // store the old mapped indices to reduce calls to Transform::SetParameters()
  std::vector<TransformOutputType> oldMappedVoxels(numSamples);

  // compute the indices mapped by the old transform
  for (SizeValueType c=0; c<numSamples; c++)
    {
    point = this->m_ImageSamples[c];
    this->template TransformPoint<TransformOutputType>(point, oldMappedVoxels[c]);
    } // end for numSamples

  // Apply the delta parameters to the transform
  this->UpdateTransformParameters(deltaParameters);

  // compute the indices mapped by the new transform
  for (SizeValueType c=0; c<numSamples; c++)
    {
    point = this->m_ImageSamples[c];
    this->template TransformPoint<TransformOutputType>(point, newMappedVoxel);

    // find max shift by checking each sample point
    distance = newMappedVoxel.EuclideanDistanceTo(oldMappedVoxels[c]);
    if ( voxelShift < distance )
      {
      voxelShift = distance;
      }
  } // end for numSamples

  // restore the parameters in the transform
  transform->SetParameters(oldParameters);

  return voxelShift;
}

/** Print the information about this class */
template< class TMetric >
void
RegistrationParameterScalesFromShift< TMetric >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

}  // namespace itk

#endif /* __itkRegistrationParameterScalesFromShift_txx */
