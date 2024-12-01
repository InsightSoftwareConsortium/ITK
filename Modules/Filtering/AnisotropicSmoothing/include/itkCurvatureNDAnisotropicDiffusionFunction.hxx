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
#ifndef itkCurvatureNDAnisotropicDiffusionFunction_hxx
#define itkCurvatureNDAnisotropicDiffusionFunction_hxx


namespace itk
{
template <typename TImage>
double CurvatureNDAnisotropicDiffusionFunction<TImage>::m_MIN_NORM = 1.0e-10;

template <typename TImage>
CurvatureNDAnisotropicDiffusionFunction<TImage>::CurvatureNDAnisotropicDiffusionFunction()
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
CurvatureNDAnisotropicDiffusionFunction<TImage>::ComputeUpdate(const NeighborhoodType & it,
                                                               void *                   itkNotUsed(globalData),
                                                               const FloatOffsetType &  itkNotUsed(offset)) -> PixelType
{
  // Calculate the partial derivatives for each dimension
  double dx_forward[ImageDimension];
  double dx_backward[ImageDimension];
  double dx[ImageDimension];
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    // "Half" derivatives
    dx_forward[i] = it.GetPixel(m_Center + m_Stride[i]) - it.GetPixel(m_Center);
    dx_forward[i] *= this->m_ScaleCoefficients[i];
    dx_backward[i] = it.GetPixel(m_Center) - it.GetPixel(m_Center - m_Stride[i]);
    dx_backward[i] *= this->m_ScaleCoefficients[i];

    // Centralized differences
    dx[i] = m_InnerProduct(x_slice[i], it, m_DerivativeOperator);
    dx[i] *= this->m_ScaleCoefficients[i];
  }

  double speed = 0.0;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    // Gradient magnitude approximations
    double grad_mag_sq = dx_forward[i] * dx_forward[i];
    double grad_mag_sq_d = dx_backward[i] * dx_backward[i];
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      if (j != i)
      {
        double dx_aug = m_InnerProduct(xa_slice[j][i], it, m_DerivativeOperator);
        dx_aug *= this->m_ScaleCoefficients[j];
        double dx_dim = m_InnerProduct(xd_slice[j][i], it, m_DerivativeOperator);
        dx_dim *= this->m_ScaleCoefficients[j];
        grad_mag_sq += 0.25f * (dx[j] + dx_aug) * (dx[j] + dx_aug);
        grad_mag_sq_d += 0.25f * (dx[j] + dx_dim) * (dx[j] + dx_dim);
      }
    }
    const double grad_mag = std::sqrt(m_MIN_NORM + grad_mag_sq);
    const double grad_mag_d = std::sqrt(m_MIN_NORM + grad_mag_sq_d);

    double Cx;
    double Cxd;
    // Conductance Terms
    if (m_K == 0.0)
    {
      Cx = 0.0;
      Cxd = 0.0;
    }
    else
    {
      Cx = std::exp(grad_mag_sq / m_K);
      Cxd = std::exp(grad_mag_sq_d / m_K);
    }
    // First order normalized finite-difference conductance products
    const double dx_forward_Cn = (dx_forward[i] / grad_mag) * Cx;
    const double dx_backward_Cn = (dx_backward[i] / grad_mag_d) * Cxd;

    // Second order conductance-modified curvature
    speed += (dx_forward_Cn - dx_backward_Cn);
  }
  // "Upwind" gradient magnitude term
  double propagation_gradient = 0.0;
  if (speed > 0)
  {
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      propagation_gradient +=
        itk::Math::sqr(std::min(dx_backward[i], 0.0)) + itk::Math::sqr(std::max(dx_forward[i], 0.0));
    }
  }
  else
  {
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      propagation_gradient +=
        itk::Math::sqr(std::max(dx_backward[i], 0.0)) + itk::Math::sqr(std::min(dx_forward[i], 0.0));
    }
  }
  return static_cast<PixelType>(std::sqrt(propagation_gradient) * speed);
}
} // end namespace itk

#endif
