/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureNDAnisotropicDiffusionFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkCurvatureNDAnisotropicDiffusionFunction_txx_
#define __itkCurvatureNDAnisotropicDiffusionFunction_txx_

namespace itk {

template<class TImage>
double CurvatureNDAnisotropicDiffusionFunction<TImage>
::m_MIN_NORM = 1.0e-10;
 
template<class TImage>
CurvatureNDAnisotropicDiffusionFunction<TImage>
::CurvatureNDAnisotropicDiffusionFunction()
{
  unsigned int i, j;
  RadiusType r;

  for (i = 0; i < ImageDimension; ++i)
    {
      r[i] = 1;
    }
  this->SetRadius(r);

  // Dummy neighborhood used to set up the slices.
  Neighborhood<PixelType, ImageDimension> it;
  it.SetRadius(r);
  
 // Slice the neighborhood
  m_Center =  it.Size() / 2;

  for (i = 0; i< ImageDimension; ++i)
    { m_Stride[i] = it.GetStride(i); }

  for (i = 0; i< ImageDimension; ++i)
    { x_slice[i]  = std::slice( m_Center - m_Stride[i], 3, m_Stride[i]); }
  
  for (i = 0; i< ImageDimension; ++i)
    {
      for (j = 0; j < ImageDimension; ++j)
        {
          // For taking derivatives in the i direction that are offset one
          // pixel in the j direction.
          xa_slice[i][j]
            = std::slice((m_Center + m_Stride[j])-m_Stride[i], 3, m_Stride[i]); 
          xd_slice[i][j]
            = std::slice((m_Center - m_Stride[j])-m_Stride[i], 3, m_Stride[i]);
        }
    }

  // Allocate the derivative operator.
  dx_op.SetDirection(0);  // Not relevant, will be applied in a slice-based
                          // fashion.
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
}

template<class TImage>
CurvatureNDAnisotropicDiffusionFunction<TImage>::PixelType
CurvatureNDAnisotropicDiffusionFunction<TImage>
::ComputeUpdate(const NeighborhoodType &it, void *globalData,
                const FloatOffsetType& offset) const
{
  unsigned int i, j;
  double speed, dx_forward_Cn, dx_backward_Cn, propagation_gradient;
  double grad_mag_sq, grad_mag_sq_d, grad_mag, grad_mag_d;
  double Cx, Cxd;
  double dx_forward[ImageDimension];
  double dx_backward[ImageDimension];
  double dx[ImageDimension];
  double dx_aug;
  double dx_dim;

  // Calculate the partial derivatives for each dimension
  for (i = 0; i < ImageDimension; i++)
    {
      // ``Half'' derivatives
      dx_forward[i] = it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]= it.GetPixel(m_Center)
        - it.GetPixel(m_Center - m_Stride[i]);

      // Centralized differences
      dx[i]         = m_InnerProduct(x_slice[i], it, dx_op);
    }

  speed = 0.0;
  for (i = 0; i < ImageDimension; i++)
    {
      // Gradient magnitude approximations
      grad_mag_sq   = dx_forward[i]  * dx_forward[i];
      grad_mag_sq_d = dx_backward[i] * dx_backward[i];
      for (j = 0; j < ImageDimension; j++)
        {
          if (j != i)
            {
              dx_aug     = m_InnerProduct(xa_slice[j][i], it, dx_op);
              dx_dim     = m_InnerProduct(xd_slice[j][i], it, dx_op);
              grad_mag_sq   += 0.25f * (dx[j]+dx_aug) * (dx[j]+dx_aug);
              grad_mag_sq_d += 0.25f * (dx[j]+dx_dim) * (dx[j]+dx_dim);
            }
        }
      grad_mag   = ::sqrt(m_MIN_NORM + grad_mag_sq);
      grad_mag_d = ::sqrt(m_MIN_NORM + grad_mag_sq_d);

      // Conductance Terms
      Cx  = ::exp( grad_mag_sq   / m_k );
      Cxd = ::exp( grad_mag_sq_d / m_k );

      // First order normalized finite-difference conductance products
      dx_forward_Cn  = (dx_forward[i]  / grad_mag) * Cx;
      dx_backward_Cn = (dx_backward[i] / grad_mag_d) * Cxd; 
      
      // Second order conductance-modified curvature
      speed += (dx_forward_Cn - dx_backward_Cn);
    }

  // ``Upwind'' gradient magnitude term
  propagation_gradient = 0.0;
  if (speed > 0)
    {  
      for (i = 0; i < ImageDimension; i++)
        {
          propagation_gradient +=
            vnl_math_sqr( vnl_math_min(dx_backward[i], 0.0) )
            + vnl_math_sqr( vnl_math_max(dx_forward[i],  0.0) );
        }
    }
  else
    {
      for (i = 0; i < ImageDimension; i++)
        {
          propagation_gradient +=
            vnl_math_sqr( vnl_math_max(dx_backward[i], 0.0) )
            + vnl_math_sqr( vnl_math_min(dx_forward[i],  0.0) );
        }
    }

  
  return ( ::sqrt(propagation_gradient) * speed );
}

template<class TImage>
CurvatureNDAnisotropicDiffusionFunction<TImage>::PixelType
CurvatureNDAnisotropicDiffusionFunction<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void *globalData,
                const FloatOffsetType& offset) const
{
  unsigned int i, j;
  double speed, dx_forward_Cn, dx_backward_Cn, propagation_gradient;
  double grad_mag_sq, grad_mag_sq_d, grad_mag, grad_mag_d;
  double Cx, Cxd;
  double dx_forward[ImageDimension];
  double dx_backward[ImageDimension];
  double dx[ImageDimension];
  double dx_aug;
  double dx_dim;

  // Calculate the partial derivatives for each dimension
  for (i = 0; i < ImageDimension; i++)
    {
      // ``Half'' derivatives
      dx_forward[i] = it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]= it.GetPixel(m_Center)
        - it.GetPixel(m_Center - m_Stride[i]);

      // Centralized differences
      dx[i]         = m_SmartInnerProduct(x_slice[i], it, dx_op);
    }

  speed = 0.0;
  for (i = 0; i < ImageDimension; i++)
    {
      // Gradient magnitude approximations
      grad_mag_sq   = dx_forward[i]  * dx_forward[i];
      grad_mag_sq_d = dx_backward[i] * dx_backward[i];
      for (j = 0; j < ImageDimension; j++)
        {
          if (j != i)
            {
              dx_aug     = m_SmartInnerProduct(xa_slice[j][i], it, dx_op);
              dx_dim     = m_SmartInnerProduct(xd_slice[j][i], it, dx_op);
              grad_mag_sq   += 0.25f * (dx[j]+dx_aug) * (dx[j]+dx_aug);
              grad_mag_sq_d += 0.25f * (dx[j]+dx_dim) * (dx[j]+dx_dim);
            }
        }
      grad_mag   = ::sqrt(m_MIN_NORM + grad_mag_sq);
      grad_mag_d = ::sqrt(m_MIN_NORM + grad_mag_sq_d);

      // Conductance Terms
      Cx  = ::exp( grad_mag_sq   / m_k );
      Cxd = ::exp( grad_mag_sq_d / m_k );

      // First order normalized finite-difference conductance products
      dx_forward_Cn  = (dx_forward[i]  / grad_mag) * Cx;
      dx_backward_Cn = (dx_backward[i] / grad_mag_d) * Cxd; 
      
      // Second order conductance-modified curvature
      speed += (dx_forward_Cn - dx_backward_Cn);
    }

  // ``Upwind'' gradient magnitude term
  propagation_gradient = 0.0;
  if (speed > 0)
    {  
      for (i = 0; i < ImageDimension; i++)
        {
          propagation_gradient +=
            vnl_math_sqr( vnl_math_min(dx_backward[i], 0.0) )
            + vnl_math_sqr( vnl_math_max(dx_forward[i],  0.0) );
        }
    }
  else
    {
      for (i = 0; i < ImageDimension; i++)
        {
          propagation_gradient +=
            vnl_math_sqr( vnl_math_max(dx_backward[i], 0.0) )
            + vnl_math_sqr( vnl_math_min(dx_forward[i],  0.0) );
        }
    }

  
  return ( ::sqrt(propagation_gradient) * speed );
}

} // end namespace itk

#endif
