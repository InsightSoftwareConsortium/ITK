/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradient2DAnisotropicDiffusionEquation.txx
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
#ifndef __itkGradient2DAnisotropicDiffusionEquation_txx_
#define __itkGradient2DAnisotropicDiffusionEquation_txx_

namespace itk {

template<class TImage>
Gradient2DAnisotropicDiffusionEquation<TImage>
::Gradient2DAnisotropicDiffusionEquation()
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
Gradient2DAnisotropicDiffusionEquation<TImage>::PixelType
Gradient2DAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const NeighborhoodType &it, TimeStepType &dt) const
{
  PixelType Cx, Cy, Cxd, Cyd;
  PixelType dx_forward, dx_backward, dy_forward, dy_backward;
  PixelType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;
   
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
  
  Cx = ::exp( (dx_forward*dx_forward + 0.25f*(dy+dy_aug)*(dy+dy_aug))
             / m_k );
  
  Cy = ::exp( (dy_forward*dy_forward + 0.25f*(dx+dx_aug)*(dx+dx_aug))
             / m_k );
  
  Cxd= ::exp( (dx_backward*dx_backward + 0.25f*(dy+dy_dim)*(dy+dy_dim))
             / m_k );
  
  Cyd= ::exp( (dy_backward*dy_backward + 0.25f*(dx+dx_dim)*(dx+dx_dim))
             / m_k );

  dx_forward  *= Cx;
  dy_forward  *= Cy;
  dx_backward *= Cxd;
  dy_backward *= Cyd;
  
  return ( dx_forward  + dy_forward + dx_backward + dy_backward );
}

template<class TImage>
Gradient2DAnisotropicDiffusionEquation<TImage>::PixelType
Gradient2DAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, TimeStepType &dt) const
{
  PixelType Cx, Cy, Cxd, Cyd;
  PixelType dx_forward, dx_backward, dy_forward, dy_backward;
  PixelType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;
   
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
  
  Cx = ::exp( (dx_forward*dx_forward + 0.25f*(dy+dy_aug)*(dy+dy_aug))
             / m_k );
  
  Cy = ::exp( (dy_forward*dy_forward + 0.25f*(dx+dx_aug)*(dx+dx_aug))
             / m_k );
  
  Cxd= ::exp( (dx_backward*dx_backward + 0.25f*(dy+dy_dim)*(dy+dy_dim))
             / m_k );
  
  Cyd= ::exp( (dy_backward*dy_backward + 0.25f*(dx+dx_dim)*(dx+dx_dim))
             / m_k );

  dx_forward  *= Cx;
  dy_forward  *= Cy;
  dx_backward *= Cxd;
  dy_backward *= Cyd;
  

  return ( dx_forward  + dy_forward + dx_backward + dy_backward );

}

} // end namespace itk

#endif
