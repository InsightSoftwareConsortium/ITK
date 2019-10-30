/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkSingleImageCostFunction.hxx,v $
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef itkSingleImageCostFunction_hxx
#define itkSingleImageCostFunction_hxx

#include "itkMath.h"
#include "itkSingleImageCostFunction.h"

namespace itk
{

/*
 * Constructor
 */
template <class TImage>
SingleImageCostFunction<TImage>::SingleImageCostFunction()
{
  m_Image = nullptr;                 // Provided by user
  m_Interpolator = nullptr;          // Configured in Initialize()
  m_GradientImageFunction = nullptr; // Configured in Initialize()
  m_DerivativeThreshold = 15.0;      // as in original implementation
}


/*
 * Initialize
 */
template <class TImage>
void
SingleImageCostFunction<TImage>::Initialize(void)
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
template <class TImage>
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
template <class TImage>
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
template <class TImage>
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
