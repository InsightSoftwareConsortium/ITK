/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRecursiveGaussianImageFilter_txx
#define _itkRecursiveGaussianImageFilter_txx

#include "itkRecursiveGaussianImageFilter.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIterator.h"
#include <new>


namespace itk
{
  
template <class TInputImage, class TOutputImage, class TComputation>
RecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
::RecursiveGaussianImageFilter()
{
  this->SetSigma( 1.0 );
}




/**
 *   Compute filter for Gaussian kernel
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
::SetUp(void)
{
  
  this->a0 = TComputation(  1.680  );
  this->a1 = TComputation(  3.735  );
  this->b0 = TComputation(  1.783  );
  this->b1 = TComputation(  1.723  );
  this->c0 = TComputation( -0.6803 );
  this->c1 = TComputation( -0.2598 );
  this->w0 = TComputation(  0.6318 );
  this->w1 = TComputation(  1.9970 );
  
  if( m_Spacing < TComputation( 0.0001 ) ) return;
  
  const TComputation sigmad = m_Sigma/m_Spacing;

//K = 1.0/(sigmad*sigmad*sqrt(2.0*(4.0*atan(1.0))));
  K = 1.0 / ( sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0 ) ) ) );
  
  const bool symmetric = true;
  ComputeFilterCoefficients(symmetric);

}



/**
 * Compute Recursive Filter Coefficients 
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage, TComputation>
::ComputeFilterCoefficients(bool symmetric) 
{

  const TComputation sigmad = m_Sigma/m_Spacing;
  
  n00  = a0 + c0;
  n11  = exp(-b1/sigmad)*(c1*sin(w1/sigmad)-(c0+2*a0)*cos(w1/sigmad)); 
  n11 += exp(-b0/sigmad)*(a1*sin(w0/sigmad)-(a0+2*c0)*cos(w0/sigmad)); 
  n22  = ((a0+c0)*cos(w1/sigmad)*cos(w0/sigmad));
  n22	-= (a1*cos(w1/sigmad)*sin(w0/sigmad)+c1*cos(w0/sigmad)*sin(w1/sigmad));
  n22	*= 2*exp(-(b0+b1)/sigmad);
  n22	+= c0*exp(-2*b0/sigmad) + a0*exp(-2*b1/sigmad);
  n33  = exp(-(b1+2*b0)/sigmad)*(c1*sin(w1/sigmad)-c0*cos(w1/sigmad));
  n33 += exp(-(b0+2*b1)/sigmad)*(a1*sin(w0/sigmad)-a0*cos(w0/sigmad));
  
  d44  = exp(-2*(b0+b1)/sigmad);
  d33  = -2*cos(w0/sigmad)*exp(-(b0+2*b1)/sigmad);
  d33 += -2*cos(w1/sigmad)*exp(-(b1+2*b0)/sigmad);
  d22  =  4*cos(w1/sigmad)*cos(w0/sigmad)*exp(-(b0+b1)/sigmad);
  d22 +=  exp(-2*b1/sigmad)+exp(-2*b0/sigmad);
  d11  =  -2*exp(-b1/sigmad)*cos(w1/sigmad)-2*exp(-b0/sigmad)*cos(w0/sigmad);
	
  if( symmetric )
    {
    m11 = n11 - d11 * n00;
    m22 = n22 - d22 * n00;
    m33 = n33 - d33 * n00;
    m44 =     - d44 * n00;
    }
  else
    {
    m11 = -( n11 - d11 * n00 );
    m22 = -( n22 - d22 * n00 );
    m33 = -( n33 - d33 * n00 );
    m44 =          d44 * n00;
    }

}




} // end namespace itk

#endif
