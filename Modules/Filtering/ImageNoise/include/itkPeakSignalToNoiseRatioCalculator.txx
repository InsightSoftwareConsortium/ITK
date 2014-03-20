/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkPeakSignalToNoiseRatioCalculator.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/10 16:32:57 $
  Version:   $Revision: 1.52 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPeakSignalToNoiseRatioCalculator_txx
#define __itkPeakSignalToNoiseRatioCalculator_txx
#include "itkPeakSignalToNoiseRatioCalculator.h"

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{ 


template < class TInputImage>
PeakSignalToNoiseRatioCalculator<TInputImage>
::PeakSignalToNoiseRatioCalculator(void) 
{
  m_Valid = false;
  m_Image = NULL;
  m_NoisyImage = NULL;
  m_Output = NumericTraits< InputPixelType >::Zero;
}


template < class TInputImage >
void
PeakSignalToNoiseRatioCalculator<TInputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Image: " << m_Image.GetPointer() << std::endl;
  os << indent << "NoisyImage: " << m_NoisyImage.GetPointer() << std::endl;
  os << indent << "Valid: " << m_Valid << std::endl;
  os << indent << "Output: " << m_Output << std::endl;
}


template < class TInputImage >
void
PeakSignalToNoiseRatioCalculator<TInputImage>
::Compute()
{

  typedef typename InputImageType::IndexType IndexType;

  if( !m_Image || !m_NoisyImage ) 
    {
    return;
    }

  ImageRegionConstIteratorWithIndex< InputImageType > iIt( m_Image,
                                                     m_Image->GetRequestedRegion() ); 
  iIt.GoToBegin();
  ImageRegionConstIteratorWithIndex< InputImageType > nIt( m_NoisyImage,
                                                     m_NoisyImage->GetRequestedRegion() ); 
  nIt.GoToBegin();

  // init the values
  double mse = 0;
  InputPixelType max = NumericTraits<InputPixelType>::NonpositiveMin();
  
  while( !iIt.IsAtEnd() )
    {
    mse += pow( (double)nIt.Get() - (double)iIt.Get(), 2 );
    max = std::max( iIt.Get(), max );
    ++iIt;
    ++nIt;
    }
  mse /= m_Image->GetRequestedRegion().GetNumberOfPixels();
  
  m_Output = 10 * vcl_log10( max * max / mse );
  m_Valid = true;

}


template < class TInputImage >
const double &
PeakSignalToNoiseRatioCalculator<TInputImage>
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
