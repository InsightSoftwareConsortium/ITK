/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGaussianOperator_txx
#define _itkGaussianOperator_txx
#include "itkGaussianOperator.h"
#include "itkOutputWindow.h"
#include "itkMacro.h"
namespace itk
{

template<class TPixel,unsigned int VDimension, class TAllocator>
typename GaussianOperator<TPixel,VDimension, TAllocator>
::CoefficientVector
GaussianOperator<TPixel,VDimension, TAllocator>
::GenerateCoefficients()
{
  CoefficientVector coeff;
  double sum;
  int i;
  int j;
  typename CoefficientVector::iterator it;

  const double et           = ::exp(-m_Variance);
  const double cap          = 1.0 - m_MaximumError;
  
 // Create the kernel coefficients as a std::vector
  sum = 0.0f;
  coeff.push_back(et * ModifiedBesselI0(m_Variance));
  sum += coeff[0];
  coeff.push_back(et * ModifiedBesselI1(m_Variance));
  sum += coeff[1]* 2.0;

  for (i=2; sum < cap; i++)
    {
    coeff.push_back(et* ModifiedBesselI(i, m_Variance));
    sum += coeff[i] *2.0;
    if (coeff[i] <= 0.0) break;  // failsafe
    if (coeff.size() > m_MaximumKernelWidth )
      {
      itkWarningMacro("Kernel size has exceeded the specified maximum width of " << m_MaximumKernelWidth << " and has been truncated to " << static_cast<unsigned long>( coeff.size() ) << " elements.  You can raise the maximum width using the SetMaximumKernelWidth method.");
      break;
      }  
    }
  // Normalize the coefficients so that their sum is one.
  for (it = coeff.begin(); it < coeff.end(); ++it)
    {
      *it /= sum;
    }

  // Make symmetric
  j = static_cast<int>( coeff.size() ) - 1;
  coeff.insert(coeff.begin(), j, 0);
  for (i=0, it = coeff.end()-1; i < j; --it, ++i)
    {
      coeff[i] = *it;
    }
  
  return coeff;
}

template<class TPixel,unsigned int VDimension, class TAllocator>
double
GaussianOperator<TPixel,VDimension, TAllocator>
::ModifiedBesselI0(double y)
{
  double d, accumulator;
  double m;

  if ((d=fabs(y)) < 3.75)
  {
    m=y/3.75;
    m*=m;
    accumulator = 1.0 + m *(3.5156229+m*(3.0899424+m*(1.2067492
      + m*(0.2659732+m*(0.360768e-1 +m*0.45813e-2)))));
  }
  else
  {
    m=3.5/d;
    accumulator =(::exp(d)/::sqrt(d))*(0.39894228+m*(0.1328592e-1
   +m*(0.225319e-2+m*(-0.157565e-2+m*(0.916281e-2
     +m*(-0.2057706e-1+m*(0.2635537e-1+m*(-0.1647633e-1   
     +m*0.392377e-2))))))));
  }
  return accumulator;
}
 

template<class TPixel,unsigned int VDimension, class TAllocator>
double
GaussianOperator<TPixel,VDimension, TAllocator>
::ModifiedBesselI1(double y)
{
  double d, accumulator;
  double m;

  if ((d=fabs(y)) < 3.75)
  {
    m=y/3.75;
    m*=m;
    accumulator = d*(0.5+m*(0.87890594+m*(0.51498869+m*(0.15084934
            +m*(0.2658733e-1+m*(0.301532e-2+m*0.32411e-3))))));
   }
  else
  {
    m=3.75/d;
    accumulator = 0.2282967e-1+m*(-0.2895312e-1+m*(0.1787654e-1
         -m*0.420059e-2));
    accumulator = 0.39894228+m*(-0.3988024e-1+m*(-0.362018e-2
      +m*(0.163801e-2+m*(-0.1031555e-1+m*accumulator))));

    accumulator *= (::exp(d)/::sqrt(d));
  }

  if (y<0.0) return -accumulator;
  else return accumulator;
  
}

template<class TPixel,unsigned int VDimension, class TAllocator>
double
GaussianOperator<TPixel,VDimension, TAllocator>
::ModifiedBesselI(int n, double y)
{
  const double ACCURACY = 40.0f;
  int j;
  double qim, qi, qip, toy;
  double accumulator;

  if (n<2)
  {
      throw ExceptionObject(__FILE__, __LINE__);  // placeholder
  }
  if (y==0.0) return 0.0;
  else
  {
    toy=2.0/fabs(y);
    qip=accumulator=0.0;
    qi=1.0;
    for (j=2*(n+(int)::sqrt(ACCURACY*n)); j>0 ; j--)
    {
      qim=qip+j*toy*qi;
      qip=qi;
      qi=qim;
      if (fabs(qi) > 1.0e10)
      {
        accumulator*=1.0e-10;
        qi *=1.0e-10;
        qip*=1.0e-10;
      }
      if (j==n) accumulator=qip;
    }
    accumulator *= ModifiedBesselI0(y)/qi;
    if (y<0.0 && (n&1)) return -accumulator;
    else return accumulator;
  }
}

}// end namespace itk

#endif
