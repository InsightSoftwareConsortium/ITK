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
#ifndef itkSingleImageCostFunction_hxx
#define itkSingleImageCostFunction_hxx

#include "itkMath.h"

namespace itk
{

template <typename TImage>
SingleImageCostFunction<TImage>::SingleImageCostFunction()
{
  m_Image = nullptr;                 // Provided by user
  m_Interpolator = nullptr;          // Configured in Initialize()
  m_GradientImageFunction = nullptr; // Configured in Initialize()
  m_DerivativeThreshold = 15.0;      // as in original implementation
}


template <typename TImage>
void
SingleImageCostFunction<TImage>::Initialize()
{
  // Ensure image is provided
  if (!m_Image)
  {
    itkExceptionMacro(<< "Image is not present");
  }

  // Ensure interpolator for image is provided
  if (!m_Interpolator)
  {
    m_Interpolator = DefaultInterpolatorType::New();
  }

  // Ensure gradient image function is initialized
  if (!m_GradientImageFunction)
  {
    m_GradientImageFunction = GradientImageFunctionType::New();
  }

  // If the image is provided by a source, update the source.
  if (m_Image->GetSource())
  {
    m_Image->GetSource()->Update();
  }

  // Setup functions
  m_Interpolator->SetInputImage(m_Image);
  m_GradientImageFunction->SetInputImage(m_Image);

  this->SetMinimize();
  // If there are any objects observing the cost function,
  // call them to give the user code a chance to set parameters
  this->InvokeEvent(InitializeEvent());
}


/*
 * Get the value by interpolating the underlying image.
 */
template <typename TImage>
typename SingleImageCostFunction<TImage>::MeasureType
SingleImageCostFunction<TImage>::GetValue(const ParametersType & parameters) const
{
  // Convert parameters to point
  PointType point;
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    point[i] = static_cast<typename PointType::ValueType>(parameters[i]);
  }

  // Ensure point is inside image
  if (m_Interpolator->IsInsideBuffer(point))
  {
    // Evaluate at point
    return static_cast<MeasureType>(m_Interpolator->Evaluate(point));
  }
  else
  {
    return m_OutsideValue;
  }
}


/*
 * Get the derivative by applying the gradient image function.
 */
template <typename TImage>
void
SingleImageCostFunction<TImage>::GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const
{
  // Init the derivative
  derivative.SetSize(ImageDimension);
  derivative.Fill(0.0);

  // Convert parameters to point
  PointType point;
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    point[i] = static_cast<typename PointType::ValueType>(parameters[i]);
  }

  // Ensure point is inside image
  typename GradientImageFunctionType::OutputType output;
  output.Fill(0.0);
  if (m_GradientImageFunction->IsInsideBuffer(point))
  {
    // Evaluate at point
    output = m_GradientImageFunction->Evaluate(point);
  }

  // Convert the image function output to the cost function derivative
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    derivative[i] = static_cast<typename DerivativeType::ValueType>(output[i]);

    // NOTE: The cost function may undefined / unreachable areas
    //           (indicated by very large values) which may skew the gradient.
    //           To avoid this skewing effect, we reset gradient values larger
    //           than a given threshold.
    if (itk::Math::abs(derivative[i]) > m_DerivativeThreshold)
    {
      derivative[i] = 0.0;
    }
  }
}


/*
 * PrintSelf
 */
template <typename TImage>
void
SingleImageCostFunction<TImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Image: " << m_Image.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "GradientImageFunction: " << m_GradientImageFunction.GetPointer() << std::endl;
}

} // end namespace itk

#endif
