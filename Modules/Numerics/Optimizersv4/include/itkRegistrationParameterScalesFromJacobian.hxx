/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkRegistrationParameterScalesFromJacobian_hxx
#define itkRegistrationParameterScalesFromJacobian_hxx


namespace itk
{

template <typename TMetric>
void
RegistrationParameterScalesFromJacobian<TMetric>::EstimateScales(ScalesType & parameterScales)
{
  this->CheckAndSetInputs();
  this->SetScalesSamplingStrategy();
  this->SampleVirtualDomain();

  const SizeValueType numPara = this->GetNumberOfLocalParameters();

  parameterScales.SetSize(numPara);

  ParametersType norms(numPara);

  const auto numSamples = static_cast<const SizeValueType>(this->m_SamplePoints.size());

  norms.Fill(NumericTraits<typename ParametersType::ValueType>::ZeroValue());
  parameterScales.Fill(NumericTraits<typename ScalesType::ValueType>::OneValue());

  // checking each sample point
  for (SizeValueType c = 0; c < numSamples; ++c)
  {
    const VirtualPointType point = this->m_SamplePoints[c];

    ParametersType squaredNorms(numPara);
    this->ComputeSquaredJacobianNorms(point, squaredNorms);

    norms = norms + squaredNorms;
  } // for numSamples

  if (numSamples > 0)
  {
    for (SizeValueType p = 0; p < numPara; ++p)
    {
      parameterScales[p] = norms[p] / numSamples;
    }
  }
}

template <typename TMetric>
auto
RegistrationParameterScalesFromJacobian<TMetric>::EstimateStepScale(const ParametersType & step) -> FloatType
{
  this->CheckAndSetInputs();
  this->SetStepScaleSamplingStrategy();
  this->SampleVirtualDomain();

  ScalesType sampleScales;
  this->ComputeSampleStepScales(step, sampleScales);

  const auto numSamples = static_cast<const SizeValueType>(this->m_SamplePoints.size());
  FloatType  scaleSum = NumericTraits<FloatType>::ZeroValue();

  // checking each sample point
  for (SizeValueType c = 0; c < numSamples; ++c)
  {
    scaleSum += sampleScales[c];
  }

  return scaleSum / numSamples;
}

template <typename TMetric>
void
RegistrationParameterScalesFromJacobian<TMetric>::EstimateLocalStepScales(const ParametersType & step,
                                                                          ScalesType &           localStepScales)
{
  if (!this->IsDisplacementFieldTransform())
  {
    itkExceptionMacro(<< "EstimateLocalStepScales: the transform doesn't have local support.");
  }

  this->CheckAndSetInputs();
  this->SetStepScaleSamplingStrategy();
  this->SampleVirtualDomain();

  ScalesType sampleScales;
  this->ComputeSampleStepScales(step, sampleScales);

  const auto          numSamples = static_cast<const SizeValueType>(this->m_SamplePoints.size());
  const SizeValueType numPara = this->GetNumberOfLocalParameters();
  const SizeValueType numAllPara = this->GetTransform()->GetNumberOfParameters();
  const SizeValueType numLocals = numAllPara / numPara;

  localStepScales.SetSize(numLocals);
  localStepScales.Fill(NumericTraits<typename ScalesType::ValueType>::ZeroValue());

  // checking each sample point
  for (SizeValueType c = 0; c < numSamples; ++c)
  {
    VirtualPointType & point = this->m_SamplePoints[c];
    IndexValueType     localId =
      this->m_Metric->ComputeParameterOffsetFromVirtualPoint(point, NumericTraits<SizeValueType>::OneValue());
    localStepScales[localId] = sampleScales[c];
  }
}

template <typename TMetric>
void
RegistrationParameterScalesFromJacobian<TMetric>::ComputeSampleStepScales(const ParametersType & step,
                                                                          ScalesType &           sampleScales)
{
  const auto          numSamples = static_cast<const SizeValueType>(this->m_SamplePoints.size());
  const SizeValueType dim = this->GetDimension();
  const SizeValueType numPara = this->GetNumberOfLocalParameters();

  sampleScales.SetSize(numSamples);

  itk::Array<FloatType> dTdt(dim);

  JacobianType jacobianCache;
  JacobianType jacobian(dim,
                        (this->GetTransformForward() ? this->m_Metric->GetMovingTransform()->GetNumberOfParameters()
                                                     : this->m_Metric->GetFixedTransform()->GetNumberOfParameters()));


  // checking each sample point
  for (SizeValueType c = 0; c < numSamples; ++c)
  {
    const VirtualPointType & point = this->m_SamplePoints[c];

    if (this->GetTransformForward())
    {
      this->m_Metric->GetMovingTransform()->ComputeJacobianWithRespectToParametersCachedTemporaries(
        point, jacobian, jacobianCache);
    }
    else
    {
      this->m_Metric->GetFixedTransform()->ComputeJacobianWithRespectToParametersCachedTemporaries(
        point, jacobian, jacobianCache);
    }

    if (!this->IsDisplacementFieldTransform())
    {
      dTdt = jacobian * step;
    }
    else
    {
      SizeValueType offset = this->m_Metric->ComputeParameterOffsetFromVirtualPoint(point, numPara);

      ParametersType localStep(numPara);
      for (SizeValueType p = 0; p < numPara; ++p)
      {
        localStep[p] = step[offset + p];
      }
      dTdt = jacobian * localStep;
    }

    sampleScales[c] = dTdt.two_norm();
  }
}

template <typename TMetric>
void
RegistrationParameterScalesFromJacobian<TMetric>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // namespace itk

#endif /* itkRegistrationParameterScalesFromJacobian_hxx */
