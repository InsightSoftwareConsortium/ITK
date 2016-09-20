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
#ifndef itkConvolutionImageFilterBase_hxx
#define itkConvolutionImageFilterBase_hxx

#include "itkConvolutionImageFilterBase.h"

namespace itk
{
template< typename TInputImage, typename TKernelImage, typename TOutputImage >
ConvolutionImageFilterBase< TInputImage, TKernelImage, TOutputImage >
::ConvolutionImageFilterBase() :
  m_Normalize( false ),
  m_OutputRegionMode( Self::SAME )
{
  this->AddRequiredInputName("KernelImage");

  m_BoundaryCondition = &m_DefaultBoundaryCondition;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
void
ConvolutionImageFilterBase< TInputImage, TKernelImage, TOutputImage >
::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method. Default
  // behavior corresponds to SAME output region mode.
  Superclass::GenerateOutputInformation();

  if ( m_OutputRegionMode == Self::VALID )
    {
    OutputRegionType validRegion = this->GetValidRegion();

    typename OutputImageType::Pointer outputPtr = this->GetOutput();
    outputPtr->SetLargestPossibleRegion( validRegion );
    }
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
typename ConvolutionImageFilterBase< TInputImage, TKernelImage, TOutputImage >::OutputRegionType
ConvolutionImageFilterBase< TInputImage, TKernelImage, TOutputImage >
::GetValidRegion() const
{
  typename InputImageType::ConstPointer inputPtr  = this->GetInput();

  InputRegionType inputLargestPossibleRegion = inputPtr->GetLargestPossibleRegion();

  OutputIndexType validIndex = inputLargestPossibleRegion.GetIndex();
  OutputSizeType validSize = inputLargestPossibleRegion.GetSize();

  // Shrink the output largest possible region by the kernel radius.
  KernelSizeType kernelSize = this->GetKernelImage()->GetLargestPossibleRegion().GetSize();
  KernelSizeType radius;
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    radius[i] = kernelSize[i] / 2;
    if ( validSize[i] < 2*radius[i] )
      {
      validIndex[i] = 0;
      validSize[i] = 0;
      }
    else
      {
      validIndex[i] = validIndex[i] + static_cast< typename OutputIndexType::IndexValueType >
        ( radius[i] );
      validSize[i] = validSize[i] - 2*radius[i];

      // If the kernel is has an even size in one dimension, then we
      // need to expand the image size by one and subtract one from
      // the index in that dimension to account for the zero-padding
      // on the low-index side of the image.
      if ( kernelSize[i] % 2 == 0 )
        {
        validIndex[i] -= 1;
        validSize[i] += 1;
        }
      }
    }

  OutputRegionType validRegion( validIndex, validSize );

  return validRegion;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
void
ConvolutionImageFilterBase< TInputImage, TKernelImage, TOutputImage >
::SetOutputRegionModeToSame()
{
  this->SetOutputRegionMode( Self::SAME );
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
void
ConvolutionImageFilterBase< TInputImage, TKernelImage, TOutputImage >
::SetOutputRegionModeToValid()
{
  this->SetOutputRegionMode( Self::VALID );
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
void
ConvolutionImageFilterBase< TInputImage, TKernelImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Normalize: "  << m_Normalize << std::endl;
  os << indent << "BoundaryCondition: "
     << m_BoundaryCondition->GetNameOfClass() << std::endl;
  os << indent << "OutputRegionMode: ";
  switch ( m_OutputRegionMode )
    {
    case SAME:
      os << "SAME";
      break;

    case VALID:
      os << "VALID";
      break;

    default:
      os << "unknown";
      break;
    }
  os << std::endl;
}
}
#endif
