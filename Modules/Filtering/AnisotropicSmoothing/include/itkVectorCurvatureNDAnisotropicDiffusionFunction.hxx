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
#ifndef itkVectorCurvatureNDAnisotropicDiffusionFunction_hxx
#define itkVectorCurvatureNDAnisotropicDiffusionFunction_hxx

#include "itkVectorCurvatureNDAnisotropicDiffusionFunction.h"

namespace itk
{
template< typename TImage >
double VectorCurvatureNDAnisotropicDiffusionFunction< TImage >
::m_MIN_NORM = 1.0e-10;

template< typename TImage >
VectorCurvatureNDAnisotropicDiffusionFunction< TImage >
::VectorCurvatureNDAnisotropicDiffusionFunction():
  m_K( 0.0 )
{
  unsigned int i, j;
  RadiusType   r;

  for ( i = 0; i < ImageDimension; ++i )
    {
    r[i] = 1;
    }
  this->SetRadius(r);

  // Dummy neighborhood used to set up the slices.
  Neighborhood< PixelType, ImageDimension > it;
  it.SetRadius(r);

  // Slice the neighborhood
  m_Center =  it.Size() / 2;

  for ( i = 0; i < ImageDimension; ++i )
    {
    m_Stride[i] = it.GetStride(i);
    }

  for ( i = 0; i < ImageDimension; ++i )
    {
    x_slice[i]  = std::slice(m_Center - m_Stride[i], 3, m_Stride[i]);
    }

  for ( i = 0; i < ImageDimension; ++i )
    {
    for ( j = 0; j < ImageDimension; ++j )
      {
      // For taking derivatives in the i direction that are offset one
      // pixel in the j direction.
      xa_slice[i][j] =
        std::slice( ( m_Center + m_Stride[j] ) - m_Stride[i], 3, m_Stride[i] );
      xd_slice[i][j] =
        std::slice( ( m_Center - m_Stride[j] ) - m_Stride[i], 3, m_Stride[i] );
      }
    }

  // Allocate the derivative operator.
  dx_op.SetDirection(0); // Not relelevant, we'll apply in a slice-based
                         // fashion
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
}

template< typename TImage >
typename VectorCurvatureNDAnisotropicDiffusionFunction< TImage >::PixelType
VectorCurvatureNDAnisotropicDiffusionFunction< TImage >
::ComputeUpdate(const NeighborhoodType & it, void *,
                const FloatOffsetType &)
{
  unsigned int i, j, k;
  double       speed;
  double       dx_forward_Cn[ImageDimension][VectorDimension];
  double       dx_backward_Cn[ImageDimension][VectorDimension];
  double       propagation_gradient;
  double       grad_mag_sq[VectorDimension];
  double       grad_mag_sq_d[VectorDimension];
  double       grad_mag[VectorDimension];
  double       grad_mag_d[VectorDimension];
  double       Cx[ImageDimension];
  double       Cxd[ImageDimension];

  const ScalarValueType ScalarValueTypeZero = NumericTraits< ScalarValueType >::ZeroValue();

  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug;
  PixelType dx_dim;
  PixelType ans;

  // Calculate the partial derivatives for each dimension
  for ( i = 0; i < ImageDimension; i++ )
    {
    // "Half" derivatives
    dx_forward[i] = it.GetPixel(m_Center + m_Stride[i])
                    - it.GetPixel(m_Center);
    dx_forward[i] = dx_forward[i] * this->m_ScaleCoefficients[i];
    dx_backward[i] = it.GetPixel(m_Center)
                     - it.GetPixel(m_Center - m_Stride[i]);
    dx_backward[i] = dx_backward[i] * this->m_ScaleCoefficients[i];

    // Centralized differences
    dx[i]         = m_InnerProduct(x_slice[i], it, dx_op);
    dx[i] = dx[i] * this->m_ScaleCoefficients[i];
    }

  for ( k = 0; k < VectorDimension; k++ )
    {
    grad_mag_sq[k]   = 0.0;
    grad_mag_sq_d[k] = 0.0;
    for ( i = 0; i < ImageDimension; i++ )
      {
      // Gradient magnitude approximations
      grad_mag_sq[k] += dx_forward[i][k]  * dx_forward[i][k];
      grad_mag_sq_d[k] += dx_backward[i][k] * dx_backward[i][k];
      for ( j = 0; j < ImageDimension; j++ )
        {
        if ( j != i )
          {
          dx_aug = m_InnerProduct(xa_slice[j][i], it, dx_op);
          dx_aug = dx_aug * this->m_ScaleCoefficients[j];
          dx_dim = m_InnerProduct(xd_slice[j][i], it, dx_op);
          dx_dim = dx_dim * this->m_ScaleCoefficients[j];
          grad_mag_sq[k] += 0.25f * ( dx[j][k] + dx_aug[k] ) * ( dx[j][k] + dx_aug[k] );
          grad_mag_sq_d[k] += 0.25f * ( dx[j][k] + dx_dim[k] ) * ( dx[j][k] + dx_dim[k] );
          }
        }
      }

    grad_mag[k]   = std::sqrt(m_MIN_NORM + grad_mag_sq[k]);
    grad_mag_d[k] = std::sqrt(m_MIN_NORM + grad_mag_sq_d[k]);
    // this grad mag should depend only on the current k
    for ( i = 0; i < ImageDimension; i++ )
      {
      dx_forward_Cn[i][k] = dx_forward[i][k] / grad_mag[k];
      dx_backward_Cn[i][k] = dx_backward[i][k] / grad_mag_d[k];
      }
    }

  double grad_mag_sq_tmp = 0.0;
  double grad_mag_sq_d_tmp = 0.0;

  for ( k = 0; k < VectorDimension; k++ )
    {
    grad_mag_sq_tmp += grad_mag_sq[k];
    grad_mag_sq_d_tmp += grad_mag_sq_d[k];
    }

  // this grad mag should depend on the sum over k's
  // Conductance Terms

  for ( i = 0; i < ImageDimension; i++ )
    {
    if ( m_K == 0.0 )
      {
      Cx[i] = 0.0;
      Cxd[i] = 0.0;
      }
    else
      {
      Cx[i]  = std::exp(grad_mag_sq_tmp   / m_K);
      Cxd[i] = std::exp(grad_mag_sq_d_tmp / m_K);
      }
    }

  for ( k = 0; k < VectorDimension; k++ )
    {
    // First order normalized finite-difference conductance products
    speed = 0.0;
    for ( i = 0; i < ImageDimension; i++ )
      {
      dx_forward_Cn[i][k] *= Cx[i];
      dx_backward_Cn[i][k] *= Cxd[i];

      // Second order conductance-modified curvature
      speed += ( dx_forward_Cn[i][k] - dx_backward_Cn[i][k] );
      }

    // "Upwind" gradient magnitude term
    propagation_gradient = 0.0;
    if ( speed > 0.0 )
      {
      for ( i = 0; i < ImageDimension; i++ )
        {
        propagation_gradient +=
          itk::Math::sqr( std::min(dx_backward[i][k], ScalarValueTypeZero) )
          + itk::Math::sqr( std::max(dx_forward[i][k],  ScalarValueTypeZero) );
        }
      }
    else
      {
      for ( i = 0; i < ImageDimension; i++ )
        {
        propagation_gradient +=
          itk::Math::sqr( std::max(dx_backward[i][k], ScalarValueTypeZero) )
          + itk::Math::sqr( std::min(dx_forward[i][k],  ScalarValueTypeZero) );
        }
      }

    ans[k] = std::sqrt(propagation_gradient) * speed;
    }

  return ans;
}
} // end namespace itk

#endif
