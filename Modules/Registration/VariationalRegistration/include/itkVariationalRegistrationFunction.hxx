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
#ifndef itkVariationalRegistrationFunction_hxx
#define itkVariationalRegistrationFunction_hxx

#include "itkVariationalRegistrationFunction.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>::VariationalRegistrationFunction()
{
  m_MovingImage = nullptr;
  m_FixedImage = nullptr;
  m_DisplacementField = nullptr;
  m_MaskImage = nullptr;

  m_TimeStep = 1.0;

  m_MaskBackgroundThreshold = NumericTraits<MaskImagePixelType>::Zero;

  // Metric calculation members
  m_Metric = NumericTraits<double>::max();
  m_SumOfMetricValues = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_RMSChange = NumericTraits<double>::max();
  m_SumOfSquaredChange = 0.0;

  m_MovingImageWarper = MovingImageWarperType::New();
}

/**
 * Set the function state values before each iteration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>::InitializeIteration()
{
  if (!this->GetMovingImage() || !this->GetFixedImage() || !this->GetDisplacementField())
  {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or DisplacementField not set");
  }

  this->WarpMovingImage();

  // initialize metric computation variables
  m_SumOfMetricValues = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_SumOfSquaredChange = 0.0;
}

/**
 * Return the warped moving image.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
const typename VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>::WarpedImagePointer
VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>::GetWarpedImage() const
{
  return m_MovingImageWarper->GetOutput();
}

/**
 * Warp the moving image into the fixed image space.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>::WarpMovingImage()
{
  if (!this->GetMovingImage() || !this->GetFixedImage() || !this->GetDisplacementField())
  {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or DisplacementField not set");
  }

  try
  {
    m_MovingImageWarper->SetInput(this->GetMovingImage());
    m_MovingImageWarper->SetOutputParametersFromImage(this->GetFixedImage());
    m_MovingImageWarper->SetDisplacementField(this->GetDisplacementField());
    m_MovingImageWarper->UpdateLargestPossibleRegion();
  }
  catch (itk::ExceptionObject & excep)
  {
    itkExceptionMacro(<< "Failed to warp moving image: " << excep);
  }
}

/**
 * Returns an empty struct that is used by the threads to include the
 * required update information for each thread.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void *
VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>::GetGlobalDataPointer() const
{
  auto * global = new GlobalDataStruct();

  global->m_SumOfMetricValues = 0.0;
  global->m_NumberOfPixelsProcessed = 0L;
  global->m_SumOfSquaredChange = 0;

  return global;
}

/**
 * Update the metric and release the per-thread-global data.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>::ReleaseGlobalDataPointer(
  void * gd) const
{
  auto * globalData = (GlobalDataStruct *)gd;

  std::lock_guard<std::mutex> mutexHolder(m_MetricCalculationLock);

  m_SumOfMetricValues += globalData->m_SumOfMetricValues;
  m_NumberOfPixelsProcessed += globalData->m_NumberOfPixelsProcessed;
  m_SumOfSquaredChange += globalData->m_SumOfSquaredChange;

  if (m_NumberOfPixelsProcessed)
  {
    m_Metric = m_SumOfMetricValues / static_cast<double>(m_NumberOfPixelsProcessed);
    m_RMSChange = std::sqrt(m_SumOfSquaredChange / static_cast<double>(m_NumberOfPixelsProcessed));
  }

  delete globalData;
}

/**
 * Standard "PrintSelf" method.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>::PrintSelf(std::ostream & os,
                                                                                          Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "MovingImage: ";
  os << m_MovingImage.GetPointer() << std::endl;
  os << indent << "FixedImage: ";
  os << m_FixedImage.GetPointer() << std::endl;
  os << indent << "DisplacementField: ";
  os << m_DisplacementField.GetPointer() << std::endl;
  os << indent << "MovingImageWarper: ";
  os << m_MovingImageWarper.GetPointer() << std::endl;

  os << indent << "TimeStep: ";
  os << m_TimeStep << std::endl;
  os << indent << "MaskBackgroundThreshold: ";
  os << static_cast<int>(m_MaskBackgroundThreshold) << std::endl;

  os << indent << "Metric: ";
  os << m_Metric << std::endl;
  os << indent << "SumOfSquaredDifference: ";
  os << m_SumOfMetricValues << std::endl;
  os << indent << "NumberOfPixelsProcessed: ";
  os << m_NumberOfPixelsProcessed << std::endl;
  os << indent << "RMSChange: ";
  os << m_RMSChange << std::endl;
  os << indent << "SumOfSquaredChange: ";
  os << m_SumOfSquaredChange << std::endl;
}

} // end namespace itk

#endif
