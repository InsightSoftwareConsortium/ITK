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
#ifndef itkPolarToCartesianTransform_hxx
#define itkPolarToCartesianTransform_hxx


namespace itk
{

template <typename TParametersValueType, unsigned int NDimensions>
PolarToCartesianTransform<TParametersValueType, NDimensions>::PolarToCartesianTransform()
  : Superclass(ParametersDimension)
{
  this->m_Center.Fill(0.0);
}


template <typename TParametersValueType, unsigned int NDimensions>
PolarToCartesianTransform<TParametersValueType, NDimensions>::~PolarToCartesianTransform() = default;


template <typename TParametersValueType, unsigned int NDimensions>
void
PolarToCartesianTransform<TParametersValueType, NDimensions>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Center: " << m_Center << std::endl;
}


template <typename TParametersValueType, unsigned int NDimensions>
typename PolarToCartesianTransform<TParametersValueType, NDimensions>::OutputPointType
PolarToCartesianTransform<TParametersValueType, NDimensions>::TransformPoint(const InputPointType & inputPoint) const
{
  OutputPointType outputPoint(inputPoint);

  double alpha = inputPoint[0];
  if (m_ConstArcIncr)
  {
    if (inputPoint[1] == 0.0)
    {
      // r=0: output collapses to the center; alpha is undefined, avoid arc/r = 0/0 or arc/0.
      alpha = 0.0;
    }
    else
    {
      alpha /= inputPoint[1]; // alpha = arc/r
    }
  }

  if (m_ReturnNaN && (alpha < -Math::pi || Math::pi < alpha))
  {
    using PointNumericTraits = NumericTraits<typename OutputPointType::ValueType>;
    outputPoint[0] = PointNumericTraits::quiet_NaN();
    outputPoint[1] = PointNumericTraits::quiet_NaN();
    return outputPoint;
  }

  alpha += m_AngleOffset; // add offset after NaN return to keep values within [-pi,pi]

  // Pass-through dims (ii >= 2) keep inputPoint values to match CartesianToPolar's round-trip.
  outputPoint[0] = this->m_Center[0] + inputPoint[1] * std::cos(alpha); // m_Center[0] + r*cos(alpha)
  outputPoint[1] = this->m_Center[1] + inputPoint[1] * std::sin(alpha); // m_Center[1] + r*sin(alpha)

  return outputPoint;
}

} // namespace itk

#endif
