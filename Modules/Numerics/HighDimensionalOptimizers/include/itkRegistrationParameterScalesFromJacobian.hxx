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
#ifndef __itkRegistrationParameterScalesFromJacobian_hxx
#define __itkRegistrationParameterScalesFromJacobian_hxx

#include "itkRegistrationParameterScalesFromJacobian.h"

namespace itk
{

template< class TMetric >
RegistrationParameterScalesFromJacobian< TMetric >
::RegistrationParameterScalesFromJacobian()
{
}

/** Compute parameter scales from average jacobian norms.
 *  For each parameter, compute the squared norm of its transform Jacobian,
 *  then average the squared norm over the sample points. This average is
 *  used as the scale of this parameter.
 */
template< class TMetric >
void
RegistrationParameterScalesFromJacobian< TMetric >
::EstimateScales(ScalesType &parameterScales)
{
  this->CheckAndSetInputs();
  this->SetScalesSamplingStrategy();
  this->SampleImageDomain();

  const SizeValueType numPara = this->GetNumberOfScales();

  parameterScales.SetSize(numPara);

  ParametersType norms(numPara);

  const SizeValueType numSamples = this->m_ImageSamples.size();

  norms.Fill( NumericTraits< typename ParametersType::ValueType >::Zero );
  parameterScales.Fill( NumericTraits< typename ScalesType::ValueType >::One );

  // checking each sample point
  for (SizeValueType c=0; c<numSamples; c++)
    {
    const VirtualPointType point = this->m_ImageSamples[c];

    ParametersType squaredNorms(numPara);
    this->ComputeSquaredJacobianNorms( point, squaredNorms );

    norms = norms + squaredNorms;
    } //for numSamples

  if (numSamples > 0)
    {
    for (SizeValueType p=0; p<numPara; p++)
      {
      parameterScales[p] = norms[p] / numSamples;
      }
    }
}

/**
 *  Compute the scale for a STEP, the impact of a STEP on the transform.
 */
template< class TMetric >
typename RegistrationParameterScalesFromJacobian< TMetric >::FloatType
RegistrationParameterScalesFromJacobian< TMetric >
::EstimateStepScale(const ParametersType &step)
{
  this->CheckAndSetInputs();
  this->SetStepScaleSamplingStrategy();
  this->SampleImageDomain();

  VirtualImageConstPointer image = this->GetVirtualImage();

  const SizeValueType numSamples = this->m_ImageSamples.size();
  const SizeValueType dim = this->GetImageDimension();
  const SizeValueType numPara = this->GetNumberOfScales();

  FloatType norm, normSum;
  normSum = NumericTraits< FloatType >::Zero;

  itk::Array<FloatType> dTdt(dim);

  // checking each sample point
  for (SizeValueType c=0; c<numSamples; c++)
    {
    const VirtualPointType point = this->m_ImageSamples[c];

    JacobianType jacobian;
    if (this->GetTransformForward())
      {
      this->GetMovingTransform()->ComputeJacobianWithRespectToParameters(point, jacobian);
      }
    else
      {
      this->GetFixedTransform()->ComputeJacobianWithRespectToParameters(point, jacobian);
      }

    if (!this->HasLocalSupport())
      {
      dTdt = jacobian * step;
      }
    else
      {
      VirtualIndexType index;
      image->TransformPhysicalPointToIndex(point, index);
      /* ImageBase::ComputeOffset returns the offset in linear continuous index
       * regardless of the number of components in each voxel. The metric will
       * offer a method to make sure the offset is consistent. */
      SizeValueType offset = image->ComputeOffset(index) * numPara;

      ParametersType localStep(numPara);
      for (SizeValueType p=0; p<numPara; p++)
        {
        localStep[p] = step[offset + p];
        }
      dTdt = jacobian * localStep;
      }

    norm = dTdt.two_norm();
    normSum += norm;
    }

  return normSum / numSamples;

}

/** Print the information about this class */
template< class TMetric >
void
RegistrationParameterScalesFromJacobian< TMetric >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

}  // namespace itk

#endif /* __itkRegistrationParameterScalesFromJacobian_txx */
