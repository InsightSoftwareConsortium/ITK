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
#ifndef itkGradientNDAnisotropicDiffusionFunction_hxx
#define itkGradientNDAnisotropicDiffusionFunction_hxx

#include "itkNumericTraits.h"

namespace itk
{
template <typename TImage>
double GradientNDAnisotropicDiffusionFunction<TImage>::m_MIN_NORM = 1.0e-10;

template <typename TImage>
GradientNDAnisotropicDiffusionFunction<TImage>::GradientNDAnisotropicDiffusionFunction()
  : m_K(0.0)
{
  RadiusType r;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    r[i] = 1;
  }
  this->SetRadius(r);

  // Dummy neighborhood used to set up the slices.
  Neighborhood<PixelType, ImageDimension> it;
  it.SetRadius(r);

  // Slice the neighborhood
  m_Center = it.Size() / 2;

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    m_Stride[i] = it.GetStride(i);
  }

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    x_slice[i] = std::slice(m_Center - m_Stride[i], 3, m_Stride[i]);
  }

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      // For taking derivatives in the i direction that are offset one
      // pixel in the j direction.
      xa_slice[i][j] = std::slice((m_Center + m_Stride[j]) - m_Stride[i], 3, m_Stride[i]);
      xd_slice[i][j] = std::slice((m_Center - m_Stride[j]) - m_Stride[i], 3, m_Stride[i]);
    }
  }

  // Allocate the derivative operator.
  m_DerivativeOperator.SetDirection(0); // Not relevant, will be applied in a slice-based
                                        // fashion.
  m_DerivativeOperator.SetOrder(1);
  m_DerivativeOperator.CreateDirectional();
}

template <typename TImage>
auto
GradientNDAnisotropicDiffusionFunction<TImage>::ComputeUpdate(const NeighborhoodType & it,
                                                              void *,
                                                              const FloatOffsetType &) -> PixelType
{
  // PixelType is scalar in this context
  PixelRealType delta{};

  // Calculate the centralized derivatives for each dimension.
  PixelRealType dx[ImageDimension];
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    dx[i] = (it.GetPixel(m_Center + m_Stride[i]) - it.GetPixel(m_Center - m_Stride[i])) / 2.0f;
    dx[i] *= this->m_ScaleCoefficients[i];
  }

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    // "Half" directional derivatives
    PixelRealType dx_forward = it.GetPixel(m_Center + m_Stride[i]) - it.GetPixel(m_Center);
    dx_forward *= this->m_ScaleCoefficients[i];
    PixelRealType dx_backward = it.GetPixel(m_Center) - it.GetPixel(m_Center - m_Stride[i]);
    dx_backward *= this->m_ScaleCoefficients[i];

    // Calculate the conductance terms.  Conductance varies with each
    // dimension because the gradient magnitude approximation is different
    // along each  dimension.
    double accum = 0.0;
    double accum_d = 0.0;
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      if (j != i)
      {
        PixelRealType dx_aug =
          (it.GetPixel(m_Center + m_Stride[i] + m_Stride[j]) - it.GetPixel(m_Center + m_Stride[i] - m_Stride[j])) /
          2.0f;
        dx_aug *= this->m_ScaleCoefficients[j];
        PixelRealType dx_dim =
          (it.GetPixel(m_Center - m_Stride[i] + m_Stride[j]) - it.GetPixel(m_Center - m_Stride[i] - m_Stride[j])) /
          2.0f;
        dx_dim *= this->m_ScaleCoefficients[j];
        accum += 0.25f * itk::Math::sqr(dx[j] + dx_aug);
        accum_d += 0.25f * itk::Math::sqr(dx[j] + dx_dim);
      }
    }

    double Cx = 0.0;
    double Cxd = 0.0;
    if (m_K != 0.0)
    {
      Cx = std::exp((itk::Math::sqr(dx_forward) + accum) / m_K);
      Cxd = std::exp((itk::Math::sqr(dx_backward) + accum_d) / m_K);
    }

    // Conductance modified first order derivatives.
    dx_forward = dx_forward * Cx;
    dx_backward = dx_backward * Cxd;

    // Conductance modified second order derivative.
    delta += dx_forward - dx_backward;
  }

  return static_cast<PixelType>(delta);
}
} // end namespace itk

#endif
