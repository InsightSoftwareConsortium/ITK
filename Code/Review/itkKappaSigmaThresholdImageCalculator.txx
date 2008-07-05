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


template < class TInputImage, class TMaskImage >
KappaSigmaThresholdImageCalculator<TInputImage, TMaskImage>
::KappaSigmaThresholdImageCalculator(void) 
{
  m_Valid = false;
  m_Input = NULL;
  m_Mask = NULL;
  m_MaskValue = NumericTraits< MaskPixelType >::max();
  m_Output = NumericTraits< InputPixelType >::Zero;
  m_SigmaFactor = 2;
  m_NumberOfIterations = 2;
}


template < class TInputImage, class TMaskImage >
void
KappaSigmaThresholdImageCalculator<TInputImage, TMaskImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Input: " << m_Input.GetPointer() << std::endl;
  os << indent << "Mask: " << m_Mask.GetPointer() << std::endl;
  os << indent << "Valid: " << m_Valid << std::endl;
  os << indent << "MaskValue: " << m_MaskValue << std::endl;
  os << indent << "SigmaFactor: " << m_SigmaFactor << std::endl;
  os << indent << "NumberOfIterations: " << m_NumberOfIterations << std::endl;
  os << indent << "Output: " << m_Output << std::endl;
}


template < class TInputImage, class TMaskImage >
void
KappaSigmaThresholdImageCalculator<TInputImage, TMaskImage>
::Compute()
{

  typedef typename InputImageType::IndexType IndexType;

  if( !m_Input ) 
    {
    return;
    }
    
  // init the values
  InputPixelType threshold = NumericTraits< InputPixelType >::max(); // use all the pixels to begin
  unsigned long count0 = 0;

  for( unsigned int iteration = 0; iteration < m_NumberOfIterations; iteration++ )
    {
    ImageRegionConstIteratorWithIndex< InputImageType > iIt( m_Input,
                                                      m_Input->GetRequestedRegion() ); 

    // compute the mean
    iIt.GoToBegin();
    unsigned long count = 0;
    double mean = 0;
    while( !iIt.IsAtEnd() )
      {
      if( !m_Mask || m_Mask->GetPixel( iIt.GetIndex() ) == m_MaskValue )
        {
        const InputPixelType & v = iIt.Get();
        if( v <= threshold )
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
    while( !iIt.IsAtEnd() )
      {
      if( !m_Mask || m_Mask->GetPixel( iIt.GetIndex() ) == m_MaskValue )
        {
        const InputPixelType & v = iIt.Get();
        if( v <= threshold )
          {
          sigma += vnl_math_sqr( v - mean );
          }
        }
      ++iIt;
      }
    sigma = vcl_sqrt( sigma / ( count - 1) );

    // compute the threshold for the next iteration
    InputPixelType newThreshold = static_cast< InputPixelType >( mean + m_SigmaFactor * sigma );
    if( newThreshold == threshold )
      {
      // no need to continue - the threshold is the same and will produce the same result
      break;
      }
    threshold = newThreshold;

    if( iteration == 0 )
      {
      count0 = count;
      }
  
    // std::cout << "ratio: " << count/(float)count0 << "  mean: " << mean << "  sigma: " << sigma << "  threshold: " << threshold+0.0 << std::endl;
    }

  m_Output = threshold;
  m_Valid = true;

}


template < class TInputImage, class TMaskImage >
const typename KappaSigmaThresholdImageCalculator<TInputImage, TMaskImage>::InputPixelType &
KappaSigmaThresholdImageCalculator<TInputImage, TMaskImage>
::GetOutput() const
{
  if (!m_Valid)
    {
    itkExceptionMacro( << "GetOutput() invoked, but the output have not been computed. Call Compute() first.");
    }
  return m_Output;
}

} // end namespace itk


#endif
