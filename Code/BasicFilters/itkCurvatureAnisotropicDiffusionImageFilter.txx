/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureAnisotropicDiffusionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include "itkNeighborhoodAlgorithm.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkDerivativeOperator.h"
#include "itkImageRegionIterator.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include <vnl/vnl_math.h>

namespace itk
{
  
template< class TInnerProduct,  class TIterator >
void AnisoDiffuseCurve2D<TInnerProduct, TIterator>
::operator()(void *d1, void *d2)
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::ScalarValueType ScalarValueType;
  enum { ImageDimension = ImageType::ImageDimension };
  enum { X=0, Y=1 };
  TInnerProduct IP;
  
  typename ImageType::Pointer input = static_cast<ImageType*>(d1);
  typename ImageType::Pointer output= static_cast<ImageType*>(d2);

  AvgGradMagSquared<ScalarValueType, ImageDimension> GradMag;
  const float k = GradMag(input, input->GetRequestedRegion())
    * this->m_ConductanceTerm * -1.0f;

  // set up the iterator
  Size<ImageType::ImageDimension> hR;
  hR[0] = 2;
  hR[1] = 1;
  TIterator it(hR, input, input->GetRequestedRegion());
  it.SetOutputBuffer(output->GetBufferPointer()
                     + output->ComputeOffset(it.GetStartIndex()));
  
  // process
  const ScalarValueType Zero =  NumericTraits<ScalarValueType>::Zero;
  const ScalarValueType One =   NumericTraits<ScalarValueType>::One;
  
  ScalarValueType Cx, Cy, Cxd, Cyd;
  ScalarValueType dx_forward, dx_backward, dy_forward, dy_backward;
  ScalarValueType dx_forward_Cn = One, dx_backward_Cn = One,
    dy_forward_Cn = One, dy_backward_Cn = One;
  ScalarValueType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;
  ScalarValueType grad_mag_x, grad_mag_y, grad_mag_xd, grad_mag_yd;
  ScalarValueType grad_mag_x_sq, grad_mag_y_sq,
    grad_mag_xd_sq, grad_mag_yd_sq;
  ScalarValueType speed_x, speed_y;

  DerivativeOperator<ScalarValueType, 2> dx_op;
   dx_op.SetDirection(X);
   dx_op.SetOrder(1);
   dx_op.CreateDirectional();
   
  DerivativeOperator<ScalarValueType, 2> dy_op;
   dy_op.SetDirection(Y);
   dy_op.SetOrder(1);
   dy_op.CreateDirectional();

  // Slice the neighborhood
  // 0  1  2  3  4
  // 5  6 *7* 8  9
  // 10 11 12 13 14
  std::slice  x_slice(6, 3, 1);
  std::slice  y_slice(2, 3, 5);
  std::slice xa_slice(7, 3, 1);
  std::slice ya_slice(3, 3, 5);
  std::slice xd_slice(5, 3, 1);
  std::slice yd_slice(1, 3, 5);

  // Process the image
  const TIterator it_end = it.End();
  for (it = it.Begin(); it < it_end; ++it)
    {
      // Centralized differences
      dx_forward = it.GetPixel(6) - it.GetPixel(7);
      dx_backward= it.GetPixel(8) - it.GetPixel(7);
      dy_forward = it.GetPixel(12)- it.GetPixel(7);
      dy_backward= it.GetPixel(2) - it.GetPixel(7);
      dx         = IP(x_slice,  it, dx_op);
      dy         = IP(y_slice,  it, dy_op);
      dx_aug     = IP(xa_slice, it, dx_op);
      dy_aug     = IP(ya_slice, it, dy_op);
      dx_dim     = IP(xd_slice, it, dx_op);
      dy_dim     = IP(yd_slice, it, dy_op);

      // Gradient magnitude approximations
      grad_mag_x_sq = dx_forward*dx_forward + 0.25f*(dy+dy_aug)*(dy+dy_aug);
      grad_mag_y_sq = dy_forward*dy_forward + 0.25f*(dx+dx_aug)*(dx+dx_aug);
      grad_mag_xd_sq= dx_backward*dx_backward + 0.25f*(dy+dy_dim)*(dy+dy_dim);
      grad_mag_yd_sq= dy_backward*dy_backward + 0.25f*(dx+dx_dim)*(dx+dx_dim);
      grad_mag_x    = sqrt(grad_mag_x_sq);
      grad_mag_y    = sqrt(grad_mag_y_sq);
      grad_mag_xd   = sqrt(grad_mag_xd_sq);
      grad_mag_yd   = sqrt(grad_mag_yd_sq);
      
      // Conductance terms
      Cx = exp( grad_mag_x_sq / k );
      Cy = exp( grad_mag_y_sq / k );
      Cxd= exp( grad_mag_xd_sq / k );
      Cyd= exp( grad_mag_yd_sq / k );

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
      speed_x = dx_forward_Cn + dx_backward_Cn;
      speed_y = dy_forward_Cn + dy_backward_Cn;

      // Upwind first derivatives
      dx = ::vnl_math_max(Zero, speed_x) * dx_backward
         + ::vnl_math_min(Zero, speed_x) * dx_forward; 
      dy = ::vnl_math_max(Zero, speed_y) * dy_backward
         + ::vnl_math_min(Zero, speed_y) * dy_forward;

      // Final product
      (*it.GetOutputBuffer())
        = sqrt(dx*dx + dy*dy) * ( speed_x + speed_y);
    }
}


template<class TInnerProduct,  class TIterator >
void AnisoDiffuseCurveND<TInnerProduct, TIterator>
::operator()(void *d1, void *d2)
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::ScalarValueType ScalarValueType;
  enum { ImageDimension = ImageType::ImageDimension };

  ImageType *input = static_cast<ImageType*>(d1);
  ImageType *output= static_cast<ImageType*>(d2);


  TInnerProduct IP;
  
  AvgGradMagSquared<ScalarValueType, ImageDimension> GradMag;
  const float k = GradMag(input, input->GetRequestedRegion())
    * this->m_ConductanceTerm * -1.0f;
  
  // set up the iterator
  Size<ImageDimension> hR;
  hR[0] = 2;
  for (int i = 1; i < ImageDimension; ++i) hR[i] = 1;

  TIterator it(hR, input, input->GetRequestedRegion());
  it.SetOutputBuffer(output->GetBufferPointer()
                     + output->ComputeOffset(it.GetStartIndex()));



  //******* UNIMPLEMENTED **************//
  
}


} // end namespace itk
