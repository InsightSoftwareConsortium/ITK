/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFuzzyConnectednessImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFuzzyConnectednessImageFilter_txx
#define _itkFuzzyConnectednessImageFilter_txx

#include "vnl/vnl_math.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"

namespace itk{

template <class TInputImage, class TOutputImage>
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::FuzzyConnectednessImageFilter()
{

}

template <class TInputImage, class TOutputImage>
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::~FuzzyConnectednessImageFilter()
{
}


template <class TInputImage, class TOutputImage>
void 
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetParameters
(const double inmean,const double invar,const double indifmean,
 const double indifvar, const double inweight)
{
  m_Mean = inmean;
  m_Var = invar;
  m_Diff_Mean = indifmean;
  m_Diff_Var = indifvar;

  if(inweight < 0)
  {
    m_Weight = 0;
  }
  else if(inweight > 1)
  {
  m_Weight = 1;
  }
  else 
  {
  m_Weight = inweight;
  }
}

template <class TInputImage, class TOutputImage>
double 
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::FuzzyAffinity(const PixelType f1,const PixelType f2)
{
  double tmp1 = 0.5 * (f1 + f2) - m_Mean;
  if(m_Weight == 1)
  {
    return( (NumericTraits<unsigned short>::max())* 
     (exp(-0.5 * tmp1 * tmp1 / m_Var)));
  }
  else{
    double tmp2 = fabs(static_cast<double>(f1) - static_cast<double>(f2)) - m_Diff_Mean;
  return( (NumericTraits<unsigned short>::max()) *
    (m_Weight * exp(-0.5 * tmp1 * tmp1 / m_Var) + 
     (1 - m_Weight) * exp(-0.5 * tmp2 * tmp2 / m_Diff_Var)));
  }
}

} /** end namespace itk. */


#endif
