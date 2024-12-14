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
#ifndef itkBinaryThresholdSpatialFunction_hxx
#define itkBinaryThresholdSpatialFunction_hxx


namespace itk
{

template <typename TFunction>
void
BinaryThresholdSpatialFunction<TFunction>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent
     << "LowerThreshold: " << static_cast<typename NumericTraits<FunctionOutputType>::PrintType>(m_LowerThreshold)
     << '\n';
  os << indent
     << "UpperThreshold: " << static_cast<typename NumericTraits<FunctionOutputType>::PrintType>(m_UpperThreshold)
     << '\n';

  itkPrintSelfObjectMacro(Function);
}

template <typename TFunction>
auto
BinaryThresholdSpatialFunction<TFunction>::Evaluate(const InputType & point) const -> OutputType
{
  const FunctionOutputType value = m_Function->Evaluate(point);

  if (m_LowerThreshold <= value && value <= m_UpperThreshold)
  {
    return true;
  }
  return false;
}
} // end namespace itk

#endif
