/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAcosImageAdaptor.h
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
#ifndef __itkVectorGradient2DAnisotropicDiffusionEquation_txx_
#define __itkVectorGradient2DAnisotropicDiffusionEquation_txx_

namespace itk {

template<class TImage>
VectorGradient2DAnisotropicDiffusionEquation<TImage>
::VectorGradient2DAnisotropicDiffusionEquation()
{
  RadiusType r;
  r[0] = 2;
  r[1] = 1;
  this->SetRadius(r);

  x_slice  = std::slice(6,3,1);
  y_slice  = std::slice(2,3,5);
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
VectorGradient2DAnisotropicDiffusionEquation<TImage>::PixelType
VectorGradient2DAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const NeighborhoodType &it, TimeStepType &dt) const
{
  unsigned int j;
  double Cx, Cy, Cxd, Cyd;
  double Cx_gradmag, Cy_gradmag, Cxd_gradmag, Cyd_gradmag;
  PixelType dx_forward, dx_backward, dy_forward, dy_backward;
  PixelType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;

  Cx_gradmag  = 0.0;
  Cy_gradmag  = 0.0;
  Cxd_gradmag = 0.0;
  Cyd_gradmag = 0.0;

  // Take derivatives
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

  // Approximate gradient magnitudes
  for (j = 0; j < VectorDimension; ++j)
    {   
      Cx_gradmag  += (dx_forward[j]*dx_forward[j] +
                     0.25f*(dy[j]+dy_aug[j])*(dy[j]+dy_aug[j]));
      Cy_gradmag  += (dy_forward[j]*dy_forward[j] +
                     0.25f*(dx[j]+dx_aug[j])*(dx[j]+dx_aug[j])); 
      Cxd_gradmag += (dx_backward[j]*dx_backward[j] +
                     0.25f*(dy[j]+dy_dim[j])*(dy[j]+dy_dim[j]));
      Cyd_gradmag += (dy_backward[j]*dy_backward[j] +
                     0.25f*(dx[j]+dx_dim[j])*(dx[j]+dx_dim[j]));
    }

  // Calculate conductance terms
  Cx = exp( Cx_gradmag  / m_k );
  Cy = exp( Cy_gradmag  / m_k );
  Cxd= exp( Cxd_gradmag / m_k );
  Cyd= exp( Cyd_gradmag / m_k );


  // Compute diffusion updates
  for (j = 0; j < VectorDimension; ++j)
    {
      dx_forward[j]  *= Cx;
      dy_forward[j]  *= Cy;
      dx_backward[j] *= Cxd;
      dy_backward[j] *= Cyd;
    }

  return ( dx_forward  + dy_forward + dx_backward + dy_backward );
}

template<class TImage>
VectorGradient2DAnisotropicDiffusionEquation<TImage>::PixelType
VectorGradient2DAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, TimeStepType &dt) const
{
  unsigned int j;
  double Cx, Cy, Cxd, Cyd;
  double Cx_gradmag, Cy_gradmag, Cxd_gradmag, Cyd_gradmag;
  PixelType dx_forward, dx_backward, dy_forward, dy_backward;
  PixelType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;

  Cx_gradmag  = 0.0;
  Cy_gradmag  = 0.0;
  Cxd_gradmag = 0.0;
  Cyd_gradmag = 0.0;

  // Take derivatives
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

  // Approximate gradient magnitudes
  for (j = 0; j < VectorDimension; ++j)
    {   
      Cx_gradmag  += (dx_forward[j]*dx_forward[j] +
                     0.25f*(dy[j]+dy_aug[j])*(dy[j]+dy_aug[j]));
      Cy_gradmag  += (dy_forward[j]*dy_forward[j] +
                     0.25f*(dx[j]+dx_aug[j])*(dx[j]+dx_aug[j])); 
      Cxd_gradmag += (dx_backward[j]*dx_backward[j] +
                     0.25f*(dy[j]+dy_dim[j])*(dy[j]+dy_dim[j]));
      Cyd_gradmag += (dy_backward[j]*dy_backward[j] +
                     0.25f*(dx[j]+dx_dim[j])*(dx[j]+dx_dim[j]));
    }

  // Calculate conductance terms
  Cx = exp( Cx_gradmag  / m_k );
  Cy = exp( Cy_gradmag  / m_k );
  Cxd= exp( Cxd_gradmag / m_k );
  Cyd= exp( Cyd_gradmag / m_k );


  // Compute diffusion updates
  for (j = 0; j < VectorDimension; ++j)
    {
      dx_forward[j]  *= Cx;
      dy_forward[j]  *= Cy;
      dx_backward[j] *= Cxd;
      dy_backward[j] *= Cyd;
    }

  return ( dx_forward  + dy_forward + dx_backward + dy_backward );
}

} // end namespace itk

#endif
