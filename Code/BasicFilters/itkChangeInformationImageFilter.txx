/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChangeInformationImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkChangeInformationImageFilter_txx
#define _itkChangeInformationImageFilter_txx

#include "itkChangeInformationImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TInputImage>
ChangeInformationImageFilter<TInputImage>
::ChangeInformationImageFilter()
{
  m_ReferenceImage = 0;

  m_ChangeSpacing = false;
  m_ChangeOrigin = false;
  m_ChangeRegion = false;

  m_CenterImage = false;
  m_UseReferenceImage = false;
  
  m_OutputSpacing.Fill(1.0);
  m_OutputOrigin.Fill(0.0);

  for (unsigned int i = 0; i < ImageDimension; i++)
    {
    m_OutputOffset[i] = 0;
    }
}

template <class TInputImage>
void 
ChangeInformationImageFilter<TInputImage>
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  unsigned int i;

  typename TInputImage::RegionType outputRegion;
  typename TInputImage::SizeType  inputSize;
  typename TInputImage::SizeType  outputSize;
  typename TInputImage::IndexType outputIndex;
  typename TInputImage::IndexType inputIndex;
  double origin[ImageDimension];
  double spacing[ImageDimension];

  itkDebugMacro("GenerateOutputInformation Start");

  // Get pointers to the input and output
  typename Superclass::OutputImagePointer output = this->GetOutput();
  typename Superclass::InputImagePointer input = 
      const_cast< TInputImage * >( this->GetInput() );

  inputIndex = input->GetLargestPossibleRegion().GetIndex();

  // Default is to copy input's information
  output->CopyInformation(input);
  
  // Output size is always the same as input size
  inputSize = input->GetLargestPossibleRegion().GetSize();
  outputSize = inputSize;

  // Establish the source of the image information
  if (m_UseReferenceImage && m_ReferenceImage)
    {
    outputIndex = m_ReferenceImage->GetLargestPossibleRegion().GetIndex();
    for (i = 0; i < ImageDimension; i++)
      {
      origin[i] = m_ReferenceImage->GetOrigin()[i];
      spacing[i] = m_ReferenceImage->GetSpacing()[i];
      }
    m_Shift = outputIndex - inputIndex;
    }
  else
    {
    outputIndex = input->GetLargestPossibleRegion().GetIndex();
    for (i = 0; i < ImageDimension; i++)
      {
      origin[i] = m_OutputOrigin[i];
      spacing[i] = m_OutputSpacing[i];
      m_Shift[i] = m_OutputOffset[i];
      }
    }

  // Change the output spacing
  if (m_ChangeSpacing)
    {
    output->SetSpacing(spacing);
    }

  // Change the output origin
  if (m_ChangeOrigin)
    {
    output->SetOrigin(origin);
    }

  // Center the image by changing its origin
  if (m_CenterImage)  
    {
    for (i = 0; i < ImageDimension; i++)
      {
      origin[i] = -output->GetSpacing()[i] * static_cast<double>(outputSize[i] - 1) / 2.0;
      }
    output->SetOrigin(origin);
    }

  // Change the output's largest possible region
  if (m_ChangeRegion)
    {
    outputRegion.SetSize(outputSize);
    outputRegion.SetIndex(outputIndex + m_Shift);
    output->SetLargestPossibleRegion(outputRegion);
    }
  else
    {
    m_Shift.Fill(0);
    }
  itkDebugMacro("GenerateOutputInformation End");
}

template <class TInputImage>
void 
ChangeInformationImageFilter<TInputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  if ( this->GetInput() )
    {
    typename TInputImage::RegionType region;
    region.SetSize(this->GetOutput()->GetRequestedRegion().GetSize());
    region.SetIndex(this->GetOutput()->GetRequestedRegion().GetIndex() - m_Shift);
    InputImagePointer input = 
                const_cast< TInputImage * >( this->GetInput() );
    input->SetRequestedRegion (region);
    }
}

template <class TInputImage>
void 
ChangeInformationImageFilter<TInputImage>
::GenerateData()
{
  // Get pointers to the input and output
  InputImagePointer output = this->GetOutput();
  InputImagePointer input = 
            const_cast< TInputImage * >( this->GetInput());
  
  // No need to copy the bulk data
  output->SetPixelContainer(input->GetPixelContainer());

  // Shift the output's buffer region
  typename TInputImage::RegionType region;
  region.SetSize(this->GetInput()->GetBufferedRegion().GetSize());
  region.SetIndex(this->GetOutput()->GetBufferedRegion().GetIndex() + m_Shift);

  output->SetBufferedRegion(region);
}

/**
 *
 */
template <class TInputImage>
void 
ChangeInformationImageFilter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "CenterImage: " << (m_CenterImage ? "On" : "Off") << std::endl;
  os << indent << "ChangeSpacing: " << (m_ChangeSpacing ? "On" : "Off") << std::endl;
  os << indent << "ChangeOrigin: " << (m_ChangeOrigin ? "On" : "Off") << std::endl;
  os << indent << "ChangeRegion: " << (m_ChangeRegion ? "On" : "Off") << std::endl;
  os << indent << "UseReferenceImage: " << (m_UseReferenceImage ? "On" : "Off") << std::endl;
  if (m_ReferenceImage)
    {
    os << indent << "ReferenceImage: " << m_ReferenceImage.GetPointer() << std::endl;
    }
  else
    {
    os << indent << "ReferenceImage: 0" << std::endl;
    }
  os << indent << "OutputSpacing: [";
  if (ImageDimension >= 1) 
    {
    os << m_OutputSpacing[0];
    }
  for( unsigned int j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_OutputSpacing[j];
    } 
  os << "]" << std::endl;
 
  os << indent << "OutputOrigin: [";
  if (ImageDimension >= 1) 
    {
    os << m_OutputOrigin[0];
    }
  for( unsigned int j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_OutputOrigin[j];
    } 
  os << "]" << std::endl;

  os << indent << "OutputOffset: [";
  if (ImageDimension >= 1) 
    {
    os << m_OutputOffset[0];
    }
  for( unsigned int j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_OutputOffset[j];
    } 
  os << "]" << std::endl;

}


} // end namespace itk

#endif
