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
#ifndef itkPointSetToPointSetMetric_hxx
#define itkPointSetToPointSetMetric_hxx


namespace itk
{

template <typename TFixedPointSet, typename TMovingPointSet>
PointSetToPointSetMetric<TFixedPointSet, TMovingPointSet>::PointSetToPointSetMetric()
{
  m_FixedPointSet = nullptr;  // has to be provided by the user.
  m_MovingPointSet = nullptr; // has to be provided by the user.
  m_Transform = nullptr;      // has to be provided by the user.
}

template <typename TFixedPointSet, typename TMovingPointSet>
void
PointSetToPointSetMetric<TFixedPointSet, TMovingPointSet>::SetTransformParameters(
  const ParametersType & parameters) const
{
  if (!m_Transform)
  {
    itkExceptionMacro("Transform has not been assigned");
  }
  m_Transform->SetParameters(parameters);
}

template <typename TFixedPointSet, typename TMovingPointSet>
void
PointSetToPointSetMetric<TFixedPointSet, TMovingPointSet>::Initialize()
{
  if (!m_Transform)
  {
    itkExceptionMacro("Transform is not present");
  }

  if (!m_MovingPointSet)
  {
    itkExceptionMacro("MovingPointSet is not present");
  }

  if (!m_FixedPointSet)
  {
    itkExceptionMacro("FixedPointSet is not present");
  }

  // If the PointSet is provided by a source, update the source.
  m_MovingPointSet->UpdateSource();

  // If the point set is provided by a source, update the source.
  m_FixedPointSet->UpdateSource();
}

template <typename TFixedPointSet, typename TMovingPointSet>
void
PointSetToPointSetMetric<TFixedPointSet, TMovingPointSet>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(MovingPointSet);
  itkPrintSelfObjectMacro(FixedPointSet);
  itkPrintSelfObjectMacro(Transform);
}
} // end namespace itk

#endif
