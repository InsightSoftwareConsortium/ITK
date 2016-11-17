/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkAntiAliasBinaryImageFilter_hxx
#define itkAntiAliasBinaryImageFilter_hxx

#include "itkAntiAliasBinaryImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkMath.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
AntiAliasBinaryImageFilter< TInputImage, TOutputImage >
::AntiAliasBinaryImageFilter() :
  m_UpperBinaryValue( NumericTraits< BinaryValueType >::OneValue() ),
  m_LowerBinaryValue( NumericTraits< BinaryValueType >::ZeroValue() ),
  m_InputImage( ITK_NULLPTR )
{
  m_CurvatureFunction = CurvatureFunctionType::New();
  this->SetDifferenceFunction(m_CurvatureFunction);

  if ( TInputImage::ImageDimension == 2 )
    {
    this->SetNumberOfLayers(2);
    }
  else
    {
    if ( TInputImage::ImageDimension == 3 )
      {
      this->SetNumberOfLayers(3);
      }
    else
      {
      this->SetNumberOfLayers(TInputImage::ImageDimension);
      }
    }

  this->SetMaximumRMSError(0.07);
  this->SetNumberOfIterations(1000);
  this->SetUseImageSpacing(false);
}

template< typename TInputImage, typename TOutputImage >
typename AntiAliasBinaryImageFilter< TInputImage, TOutputImage >::ValueType
AntiAliasBinaryImageFilter< TInputImage, TOutputImage >
::CalculateUpdateValue(const IndexType & idx,   const TimeStepType & dt,
                       const ValueType & value, const ValueType & change)
{
  // This method introduces the constraint on the flow of the surface.

  const BinaryValueType binary_val = m_InputImage->GetPixel(idx);
  const ValueType       new_value = value + dt * change;

  if ( Math::ExactlyEquals(binary_val, m_UpperBinaryValue) )
    {
    return ( std::max( new_value, this->GetValueZero() ) );
    }
  else
    {
    return ( std::min( new_value, this->GetValueZero() ) );
    }
}

template< typename TInputImage, typename TOutputImage >
void
AntiAliasBinaryImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  this->InterpolateSurfaceLocationOff(); // no need for interpolation here
  if ( TInputImage::ImageDimension > 3 && this->GetNumberOfLayers() < 4 )
    {
    itkWarningMacro("Only 3 layers are being used in the solver."
                    << "  You should consider using at least as many layers as dimensions of your input."
                    << "  This value can be set by calling SetNumberOfLayers(n) on this filter.");
    }

  m_InputImage = this->GetInput();

  // Find the minimum and maximum of the input image and use these values to
  // set m_UpperBinaryValue, m_LowerBinaryValue, and m_IsoSurfaceValue in the
  // parent class.
  typename itk::MinimumMaximumImageCalculator< InputImageType >::Pointer
  minmax = itk::MinimumMaximumImageCalculator< InputImageType >::New();
  minmax->SetImage(m_InputImage);
  minmax->ComputeMinimum();
  minmax->ComputeMaximum();

  m_UpperBinaryValue = minmax->GetMaximum();
  m_LowerBinaryValue = minmax->GetMinimum();

  ValueType min = static_cast< ValueType >( minmax->GetMinimum() );
  ValueType max = static_cast< ValueType >( minmax->GetMaximum() );

  // IsoSurface value is halfway between minimum and maximum.  In a binary
  // image, this places the zero level sets correctly no matter what the binary
  // values may be.

  this->SetIsoSurfaceValue( max - ( ( max - min ) / 2.0 ) );

  // Start the solver
  Superclass::GenerateData();

  // Release the pointer
  m_InputImage = ITK_NULLPTR;
}

template< typename TInputImage, typename TOutputImage >
void
AntiAliasBinaryImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "m_UpperBinaryValue = " << m_UpperBinaryValue << std::endl;
  os << indent << "m_LowerBinaryValue = " << m_LowerBinaryValue << std::endl;

  itkPrintSelfObjectMacro( InputImage );
}
} // end namespace itk

#endif
