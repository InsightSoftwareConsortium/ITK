/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAntiAliasBinaryImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAntiAliasBinaryImageFilter_txx_
#define __itkAntiAliasBinaryImageFilter_txx_

#include "itkAntiAliasBinaryImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"

namespace itk {

template <class TInputImage, class TOutputImage>
void
AntiAliasBinaryImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_MaximumRMSError = "  << m_MaximumRMSError  << std::endl;
  os << indent << "m_UpperBinaryValue = " << m_UpperBinaryValue << std::endl;
  os << indent << "m_LowerBinaryValue = " << m_LowerBinaryValue << std::endl;
  os << indent << "m_MaximumIterations = " << m_MaximumIterations << std::endl;
}


template <class TInputImage, class TOutputImage>
typename AntiAliasBinaryImageFilter<TInputImage, TOutputImage>::ValueType
AntiAliasBinaryImageFilter<TInputImage, TOutputImage>
::CalculateUpdateValue(const IndexType &idx,   const TimeStepType &dt,
                       const ValueType &value, const ValueType &change)
{
  // This method introduces the constraint on the flow of the surface.
  const BinaryValueType binary_val = this->GetInput()->GetPixel(idx);
  const ValueType new_value = value + dt * change;

  if (binary_val == m_UpperBinaryValue)
    {   return ( vnl_math_max(new_value, m_ValueZero) ); }
  else
    {   return ( vnl_math_min(new_value, m_ValueZero) ); }
  
}

template <class TInputImage, class TOutputImage>
AntiAliasBinaryImageFilter<TInputImage, TOutputImage>
::AntiAliasBinaryImageFilter()
{
  m_CurvatureFunction = CurvatureFunctionType::New();
    this->SetDifferenceFunction(m_CurvatureFunction);

  if (ImageDimension == 2) this->SetNumberOfLayers(2);
  else if (ImageDimension == 3) this->SetNumberOfLayers(3);
  else
    {
      this->SetNumberOfLayers(3);
    }
    
  m_MaximumRMSError = 0.07;
  m_UpperBinaryValue = NumericTraits<BinaryValueType>::One;
  m_LowerBinaryValue = - NumericTraits<BinaryValueType>::One;
  m_MaximumIterations = 1000;
}


template <class TInputImage, class TOutputImage>
void
AntiAliasBinaryImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  this->InterpolateSurfaceLocationOff(); // no need for interpolation here
  if (ImageDimension > 3 && this->GetNumberOfLayers() < 4)
    {
      itkWarningMacro("Only 3 layers are being used in the solver.  You should consider using at least as many layers as dimensions of your input.  This value can be set by calling SetNumberOfLayers(n) on this filter.");
    }
  
  // Find the minimum and maximum of the input image and use these values to
  // set m_UpperBinaryValue, m_LowerBinaryValue, and m_IsoSurfaceValue in the
  // parent class.
  typename itk::MinimumMaximumImageCalculator<InputImageType>::Pointer
    minmax = itk::MinimumMaximumImageCalculator<InputImageType>::New();
  minmax->SetImage( this->GetInput() );
  minmax->ComputeMinimum();
  minmax->ComputeMaximum();

  m_UpperBinaryValue = minmax->GetMaximum();
  m_LowerBinaryValue = minmax->GetMinimum();

  ValueType min = static_cast<ValueType>(minmax->GetMinimum());
  ValueType max = static_cast<ValueType>(minmax->GetMaximum());

  // IsoSurface value is halfway between minimum and maximum.  In a binary
  // image, this places the zero level sets correctly no matter what the binary
  // values may be.
  
  this->SetIsoSurfaceValue( max - ( (max - min)/2.0 ) );
  
  // Start the solver.
  Superclass::GenerateData();
}


} // end namespace itk


#endif
