/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKappaSigmaThresholdImageCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKappaSigmaThresholdImageCalculator_txx
#define __itkKappaSigmaThresholdImageCalculator_txx
#include "itkKappaSigmaThresholdImageCalculator.h"

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
template< class TInputImage, class TMaskImage >
KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >
::KappaSigmaThresholdImageCalculator(void)
{
  this->m_Valid = false;
  this->m_Image = NULL;
  this->m_Mask = NULL;
  this->m_MaskValue = NumericTraits< MaskPixelType >::max();
  this->m_Output = NumericTraits< InputPixelType >::Zero;
  this->m_SigmaFactor = 2;
  this->m_NumberOfIterations = 2;
}

template< class TInputImage, class TMaskImage >
void
KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Input: " << this->m_Image.GetPointer() << std::endl;
  os << indent << "Mask: " << this->m_Mask.GetPointer() << std::endl;
  os << indent << "Valid: " << this->m_Valid << std::endl;
  os << indent << "MaskValue: " << this->m_MaskValue << std::endl;
  os << indent << "SigmaFactor: " << this->m_SigmaFactor << std::endl;
  os << indent << "NumberOfIterations: " << this->m_NumberOfIterations << std::endl;
  os << indent << "Output: " << this->m_Output << std::endl;
}

template< class TInputImage, class TMaskImage >
void
KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >
::Compute()
{
  typedef typename InputImageType::IndexType IndexType;

  if ( !this->m_Image )
    {
    return;
    }

  // init the values
  InputPixelType threshold = NumericTraits< InputPixelType >::max(); // use all
                                                                     // the
                                                                     // pixels
                                                                     // to begin

  for ( unsigned int iteration = 0; iteration < this->m_NumberOfIterations; iteration++ )
    {
    ImageRegionConstIteratorWithIndex< InputImageType > iIt( this->m_Image,
                                                             this->m_Image->GetRequestedRegion() );

    // compute the mean
    iIt.GoToBegin();
    unsigned long count = 0;
    double        mean = 0;
    while ( !iIt.IsAtEnd() )
      {
      if ( !this->m_Mask || this->m_Mask->GetPixel( iIt.GetIndex() ) == this->m_MaskValue )
        {
        const InputPixelType & v = iIt.Get();
        if ( v <= threshold )
          {
          mean += v;
          count++;
          }
        }
      ++iIt;
      }
    mean = mean / count;

    // compute sigma
    iIt.GoToBegin();
    double sigma = 0;
    while ( !iIt.IsAtEnd() )
      {
      if ( !this->m_Mask || this->m_Mask->GetPixel( iIt.GetIndex() ) == this->m_MaskValue )
        {
        const InputPixelType & v = iIt.Get();
        if ( v <= threshold )
          {
          sigma += vnl_math_sqr(v - mean);
          }
        }
      ++iIt;
      }
    sigma = vcl_sqrt( sigma / ( count - 1 ) );

    // compute the threshold for the next iteration
    InputPixelType newThreshold = static_cast< InputPixelType >( mean + this->m_SigmaFactor * sigma );
    if ( newThreshold == threshold )
      {
      // no need to continue - the threshold is the same and will produce the
      // same result
      break;
      }
    threshold = newThreshold;
    }

  this->m_Output = threshold;
  this->m_Valid = true;
}

template< class TInputImage, class TMaskImage >
const typename KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >::InputPixelType &
KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >
::GetOutput() const
{
  if ( !this->m_Valid )
    {
    itkExceptionMacro(<< "GetOutput() invoked, but the output have not been computed. Call Compute() first.");
    }
  return this->m_Output;
}
} // end namespace itk

#endif
