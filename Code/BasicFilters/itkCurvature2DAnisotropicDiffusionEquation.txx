/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvature2DAnisotropicDiffusionEquation.txx
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
#ifndef __itkCurvature2DAnisotropicDiffusionEquation_txx_
#define __itkCurvature2DAnisotropicDiffusionEquation_txx_

namespace itk {

template<class TImage>
Curvature2DAnisotropicDiffusionEquation<TImage>
::Curvature2DAnisotropicDiffusionEquation()
{
  RadiusType r;
  r[0] = 2;
  r[1] = 1;
  this->SetRadius(r);

  x_slice = std::slice(6,3,1);
  y_slice = std::slice(2,3,5);
  xa_slice = std::slice(7,3,1);
  ya_slice = std::slice(3,3,5);
  xd_slice = std::slice(5,3,1);
  yd_slice = std::slice(1,3,5);
  
  dx_op.SetDirection(0);
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
  
  dy_op.SetDirection(1);
  dy_op.SetOrder(1);
  dy_op.CreateDirectional();
}

template<class TImage>
Curvature2DAnisotropicDiffusionEquation<TImage>::PixelType
Curvature2DAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
  const PixelType Zero = NumericTraits<PixelType>::Zero;
  const PixelType One  = NumericTraits<PixelType>::One;
  
  PixelType Cx, Cy, Cxd, Cyd;
  PixelType dx_forward, dx_backward, dy_forward, dy_backward;
  PixelType dx_forward_Cn  = One;
  PixelType dx_backward_Cn = One;
  PixelType dy_forward_Cn  = One;
  PixelType dy_backward_Cn = One;
  PixelType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;
  PixelType grad_mag_x, grad_mag_y, grad_mag_xd, grad_mag_yd;
  PixelType grad_mag_x_sq, grad_mag_y_sq, grad_mag_xd_sq, grad_mag_yd_sq;
  PixelType speed, propagation_gradient;

  // Centralized differences
  dx_forward = it.GetPixel(6)  - it.GetPixel(7);
  dx_backward= it.GetPixel(8)  - it.GetPixel(7);
  dy_forward = it.GetPixel(12) - it.GetPixel(7);
  dy_backward= it.GetPixel(2)  - it.GetPixel(7);

  dx         = m_InnerProduct(x_slice, it, dx_op);
  dy         = m_InnerProduct(y_slice, it, dy_op);
  dx_aug     = m_InnerProduct(xa_slice,it, dx_op);
  dy_aug     = m_InnerProduct(ya_slice,it, dy_op);
  dx_dim     = m_InnerProduct(xd_slice,it, dx_op);
  dy_dim     = m_InnerProduct(yd_slice,it, dy_op);

  // Gradient magnitude approximations
  grad_mag_x_sq = dx_forward*dx_forward + 0.25f*(dy+dy_aug)*(dy+dy_aug);
  grad_mag_y_sq = dy_forward*dy_forward + 0.25f*(dx+dx_aug)*(dx+dx_aug);
  grad_mag_xd_sq= dx_backward*dx_backward + 0.25f*(dy+dy_dim)*(dy+dy_dim);
  grad_mag_yd_sq= dy_backward*dy_backward + 0.25f*(dx+dx_dim)*(dx+dx_dim);
  grad_mag_x    = ::sqrt(grad_mag_x_sq);
  grad_mag_y    = ::sqrt(grad_mag_y_sq);
  grad_mag_xd   = ::sqrt(grad_mag_xd_sq);
  grad_mag_yd   = ::sqrt(grad_mag_yd_sq);

  // Conductance terms
  Cx = exp( grad_mag_x_sq / m_k );
  Cy = exp( grad_mag_y_sq / m_k );
  Cxd= exp( grad_mag_xd_sq / m_k );
  Cyd= exp( grad_mag_yd_sq / m_k );

  // Normalized finite-difference, conductance products (1st order)
  if ( grad_mag_x != Zero )
    {   dx_forward_Cn  = (dx_forward / grad_mag_x) * Cx; }
  if ( grad_mag_y != Zero )
    {   dy_forward_Cn  = (dy_forward / grad_mag_y) * Cy; }
  if ( grad_mag_xd != Zero )
    { dx_backward_Cn = (dx_backward/ grad_mag_xd)* Cxd;  }
  if ( grad_mag_yd != Zero )
    { dy_backward_Cn = (dy_backward/ grad_mag_yd)* Cyd;  }

  // Conductance-modified curvature (2nd order, speed_x + speed_y)
  speed = dx_forward_Cn + dx_backward_Cn + dy_forward_Cn + dy_backward_Cn;

  // ``Upwind'' gradient magnitude term
  if (speed > 0)
    {
      propagation_gradient = vnl_math_sqr( vnl_math_max(dx_backward, Zero) )
        + vnl_math_sqr( vnl_math_min(dx_forward,  Zero) )
        + vnl_math_sqr( vnl_math_max(dy_backward, Zero) )
        + vnl_math_sqr( vnl_math_min(dy_forward,  Zero) );
    }
  else
    {
      propagation_gradient = vnl_math_sqr( vnl_math_min(dx_backward, Zero) )
        + vnl_math_sqr( vnl_math_max(dx_forward,  Zero) )
        + vnl_math_sqr( vnl_math_min(dy_backward, Zero) )
        + vnl_math_sqr( vnl_math_max(dy_forward,  Zero) );
    }
  
    
  // Final product
  return ( vnl_math_sqrt(propagation_gradient) * speed );
}

template<class TImage>
Curvature2DAnisotropicDiffusionEquation<TImage>::PixelType
Curvature2DAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
  const PixelType Zero = NumericTraits<PixelType>::Zero;
  const PixelType One  = NumericTraits<PixelType>::One;
  
  PixelType Cx, Cy, Cxd, Cyd;
  PixelType dx_forward, dx_backward, dy_forward, dy_backward;
  PixelType dx_forward_Cn  = One;
  PixelType dx_backward_Cn = One;
  PixelType dy_forward_Cn  = One;
  PixelType dy_backward_Cn = One;
  PixelType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;
  PixelType grad_mag_x, grad_mag_y, grad_mag_xd, grad_mag_yd;
  PixelType grad_mag_x_sq, grad_mag_y_sq, grad_mag_xd_sq, grad_mag_yd_sq;
  PixelType speed, propagation_gradient;

  // Centralized differences
  dx_forward = it.GetPixel(6)  - it.GetPixel(7);
  dx_backward= it.GetPixel(8)  - it.GetPixel(7);
  dy_forward = it.GetPixel(12) - it.GetPixel(7);
  dy_backward= it.GetPixel(2)  - it.GetPixel(7);

  dx         = m_SmartInnerProduct(x_slice, it, dx_op);
  dy         = m_SmartInnerProduct(y_slice, it, dy_op);
  dx_aug     = m_SmartInnerProduct(xa_slice,it, dx_op);
  dy_aug     = m_SmartInnerProduct(ya_slice,it, dy_op);
  dx_dim     = m_SmartInnerProduct(xd_slice,it, dx_op);
  dy_dim     = m_SmartInnerProduct(yd_slice,it, dy_op);

  // Gradient magnitude approximations
  grad_mag_x_sq = dx_forward*dx_forward + 0.25f*(dy+dy_aug)*(dy+dy_aug);
  grad_mag_y_sq = dy_forward*dy_forward + 0.25f*(dx+dx_aug)*(dx+dx_aug);
  grad_mag_xd_sq= dx_backward*dx_backward + 0.25f*(dy+dy_dim)*(dy+dy_dim);
  grad_mag_yd_sq= dy_backward*dy_backward + 0.25f*(dx+dx_dim)*(dx+dx_dim);
  grad_mag_x    = ::sqrt(grad_mag_x_sq);
  grad_mag_y    = ::sqrt(grad_mag_y_sq);
  grad_mag_xd   = ::sqrt(grad_mag_xd_sq);
  grad_mag_yd   = ::sqrt(grad_mag_yd_sq);

  // Conductance terms
  Cx = exp( grad_mag_x_sq / m_k );
  Cy = exp( grad_mag_y_sq / m_k );
  Cxd= exp( grad_mag_xd_sq / m_k );
  Cyd= exp( grad_mag_yd_sq / m_k );

  // Normalized finite-difference, conductance products (1st order)
  if ( grad_mag_x != Zero )
    {   dx_forward_Cn  = (dx_forward / grad_mag_x) * Cx; }
  if ( grad_mag_y != Zero )
    {   dy_forward_Cn  = (dy_forward / grad_mag_y) * Cy; }
  if ( grad_mag_xd != Zero )
    { dx_backward_Cn = (dx_backward/ grad_mag_xd)* Cxd;  }
  if ( grad_mag_yd != Zero )
    { dy_backward_Cn = (dy_backward/ grad_mag_yd)* Cyd;  }

  // Conductance-modified curvature (2nd order, speed_x + speed_y)
  speed = dx_forward_Cn + dx_backward_Cn + dy_forward_Cn + dy_backward_Cn;
  
  // Upwind first derivatives
 // ``Upwind'' gradient magnitude term
  if (speed > 0)
    {
      propagation_gradient = vnl_math_sqr( vnl_math_max(dx_backward, Zero) )
        + vnl_math_sqr( vnl_math_min(dx_forward,  Zero) )
        + vnl_math_sqr( vnl_math_max(dy_backward, Zero) )
        + vnl_math_sqr( vnl_math_min(dy_forward,  Zero) );
    }
  else
    {
      propagation_gradient = vnl_math_sqr( vnl_math_min(dx_backward, Zero) )
        + vnl_math_sqr( vnl_math_max(dx_forward,  Zero) )
        + vnl_math_sqr( vnl_math_min(dy_backward, Zero) )
        + vnl_math_sqr( vnl_math_max(dy_forward,  Zero) );
    }
    
  // Final product
  return ( vnl_math_sqrt(propagation_gradient) * speed);
}

} // end namespace itk

#endif
