/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFuzzyConnectednessRGBImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimpleFuzzyConnectednessRGBImageFilter_txx
#define __itkSimpleFuzzyConnectednessRGBImageFilter_txx
#include "itkSimpleFuzzyConnectednessRGBImageFilter.h"

#include "vnl/vnl_math.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"

namespace itk{


template <class TInputImage, class TOutputImage>
SimpleFuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::SimpleFuzzyConnectednessRGBImageFilter()
{
}

template <class TInputImage, class TOutputImage>
SimpleFuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::~SimpleFuzzyConnectednessRGBImageFilter()
{
}


template <class TInputImage, class TOutputImage>
double 
SimpleFuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::FuzzyAffinity(const PixelType f1, const PixelType f2)
{
  double save[3];
  save[0] = 0.5 * (f1[0]+f2[0]) - m_Mean[0];
  save[1] = 0.5 * (f1[1]+f2[1]) - m_Mean[1];
  save[2] = 0.5 * (f1[2]+f2[2]) - m_Mean[2];

  double s00 = save[0]*save[0];
  double s01 = save[0]*save[1];
  double s02 = save[0]*save[2];
  double s11 = save[1]*save[1];
  double s12 = save[1]*save[2];
  double s22 = save[2]*save[2];

  double tmp1 = s00*(m_VarianceInverse[0][0])
    + s11*(m_VarianceInverse[1][1])
    + s22*(m_VarianceInverse[2][2])
    + s01*(m_VarianceInverse[0][1]+m_VarianceInverse[1][0])
    + s02*(m_VarianceInverse[0][2]+m_VarianceInverse[2][0])
    + s12*(m_VarianceInverse[1][2]+m_VarianceInverse[2][1]);

  if(m_Weight == 1)
    {
    return( (NumericTraits<unsigned short>::max())*(exp(-0.5*tmp1)) );
    }
  else
    {
    save[0] = f1[0]-f2[0];
    save[1] = f1[1]-f2[1];
    save[2] = f1[2]-f2[2];
    if(save[0] < 0)
      save[0]=-save[0];
    if(save[1] < 0)
      save[1]=-save[1];
    if(save[2] < 0)
      save[2]=-save[2];
    save[0] = save[0] - m_Diff_Mean[0];
    save[1] = save[1] - m_Diff_Mean[1];
    save[2] = save[2] - m_Diff_Mean[2];

    s00 = save[0]*save[0];
    s01 = save[0]*save[1];
    s02 = save[0]*save[2];
    s11 = save[1]*save[1];
    s12 = save[1]*save[2];
    s22 = save[2]*save[2];

    double tmp3 = s00*(m_Diff_VarianceInverse[0][0])
      + s11*(m_Diff_VarianceInverse[1][1])
      + s22*(m_Diff_VarianceInverse[2][2])
      + s01*(m_Diff_VarianceInverse[0][1]+m_Diff_VarianceInverse[1][0])
      + s02*(m_Diff_VarianceInverse[0][2]+m_Diff_VarianceInverse[2][0])
      + s12*(m_Diff_VarianceInverse[1][2]+m_Diff_VarianceInverse[2][1]);

    return( (NumericTraits<unsigned short>::max())*(m_Weight*exp(-0.5*tmp1)  
                                                    +(1-m_Weight)*exp(-0.5*tmp3)) );
    }
}


template <class TInputImage, class TOutputImage>
void 
SimpleFuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::GenerateData()
{

/* Compute the inverse of the Varianceiance Matrices. */
  m_VarianceDet = m_Variance[0][0]*m_Variance[1][1]*m_Variance[2][2]
             +m_Variance[1][0]*m_Variance[2][1]*m_Variance[0][2]
       +m_Variance[0][1]*m_Variance[1][2]*m_Variance[2][0]
       -m_Variance[2][0]*m_Variance[1][1]*m_Variance[0][2]
       -m_Variance[0][1]*m_Variance[1][0]*m_Variance[2][2]
       -m_Variance[0][0]*m_Variance[1][2]*m_Variance[2][1];
  m_VarianceInverse[0][0]=(m_Variance[1][1]*m_Variance[2][2]-m_Variance[2][1]*m_Variance[1][2])
                      /m_VarianceDet;  
  m_VarianceInverse[0][1]=-(m_Variance[1][0]*m_Variance[2][2]-m_Variance[2][0]*m_Variance[1][2])
                      /m_VarianceDet;  
  m_VarianceInverse[0][2]=(m_Variance[1][0]*m_Variance[2][1]-m_Variance[2][0]*m_Variance[1][1])
                      /m_VarianceDet;  
  m_VarianceInverse[1][0]=-(m_Variance[0][1]*m_Variance[2][2]-m_Variance[2][1]*m_Variance[0][2])
                      /m_VarianceDet;  
  m_VarianceInverse[1][1]=(m_Variance[0][0]*m_Variance[2][2]-m_Variance[2][0]*m_Variance[0][2])
                      /m_VarianceDet;  
  m_VarianceInverse[1][2]=-(m_Variance[0][0]*m_Variance[2][1]-m_Variance[2][0]*m_Variance[0][1])
                      /m_VarianceDet;  
  m_VarianceInverse[2][0]=(m_Variance[0][1]*m_Variance[1][2]-m_Variance[1][1]*m_Variance[0][2])
                      /m_VarianceDet;  
  m_VarianceInverse[2][1]=-(m_Variance[0][0]*m_Variance[1][2]-m_Variance[1][0]*m_Variance[0][2])
                      /m_VarianceDet;  
  m_VarianceInverse[2][2]=(m_Variance[0][0]*m_Variance[1][1]-m_Variance[1][0]*m_Variance[0][1])
                      /m_VarianceDet;  
  if((int)(m_Weight*100+0.5) > 1){ //need to use the difference information.

  m_Diff_VarianceDet = m_Diff_Variance[0][0]*m_Diff_Variance[1][1]*m_Diff_Variance[2][2]
    +m_Diff_Variance[1][0]*m_Diff_Variance[2][1]*m_Diff_Variance[0][2]
    +m_Diff_Variance[0][1]*m_Diff_Variance[1][2]*m_Diff_Variance[2][0]
    -m_Diff_Variance[2][0]*m_Diff_Variance[1][1]*m_Diff_Variance[0][2]
    -m_Diff_Variance[0][1]*m_Diff_Variance[1][0]*m_Diff_Variance[2][2]
    -m_Diff_Variance[0][0]*m_Diff_Variance[1][2]*m_Diff_Variance[2][1];
  m_Diff_VarianceInverse[0][0]=(m_Diff_Variance[1][1]*m_Diff_Variance[2][2]-m_Diff_Variance[2][1]*m_Diff_Variance[1][2])
                      /m_Diff_VarianceDet;  
  m_Diff_VarianceInverse[0][1]=-(m_Diff_Variance[1][0]*m_Diff_Variance[2][2]-m_Diff_Variance[2][0]*m_Diff_Variance[1][2])
                      /m_Diff_VarianceDet;  
  m_Diff_VarianceInverse[0][2]=(m_Diff_Variance[1][0]*m_Diff_Variance[2][1]-m_Diff_Variance[2][0]*m_Diff_Variance[1][1])
                      /m_Diff_VarianceDet;  
  m_Diff_VarianceInverse[1][0]=-(m_Diff_Variance[0][1]*m_Diff_Variance[2][2]-m_Diff_Variance[2][1]*m_Diff_Variance[0][2])
                      /m_Diff_VarianceDet;  
  m_Diff_VarianceInverse[1][1]=(m_Diff_Variance[0][0]*m_Diff_Variance[2][2]-m_Diff_Variance[2][0]*m_Diff_Variance[0][2])
                      /m_Diff_VarianceDet;  
  m_Diff_VarianceInverse[1][2]=-(m_Diff_Variance[0][0]*m_Diff_Variance[2][1]-m_Diff_Variance[2][0]*m_Diff_Variance[0][1])
                      /m_Diff_VarianceDet;  
  m_Diff_VarianceInverse[2][0]=(m_Diff_Variance[0][1]*m_Diff_Variance[1][2]-m_Diff_Variance[1][1]*m_Diff_Variance[0][2])
                      /m_Diff_VarianceDet;  
  m_Diff_VarianceInverse[2][1]=-(m_Diff_Variance[0][0]*m_Diff_Variance[1][2]-m_Diff_Variance[1][0]*m_Diff_Variance[0][2])
                      /m_Diff_VarianceDet;  
  m_Diff_VarianceInverse[2][2]=(m_Diff_Variance[0][0]*m_Diff_Variance[1][1]-m_Diff_Variance[1][0]*m_Diff_Variance[0][1])
                      /m_Diff_VarianceDet;  
  }
  
  Superclass::GenerateData();            
}

template <class TInputImage, class TOutputImage>
void
SimpleFuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Mean = " << m_Mean << std::endl;
  os << indent << "Diff_Mean = " << m_Diff_Mean << std::endl;
}
} /* end namespace itk. */

#endif
