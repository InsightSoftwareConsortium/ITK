/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMaskImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPolylineMaskImageFilter_txx
#define _itkPolylineMaskImageFilter_txx

#include "itkPolylineMaskImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::PolylineMaskImageFilter()
{
  this->SetNumberOfRequiredInputs( 3 );
}

/**
 *
 */
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  void PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::SetInput(const InputImageType *input)

  {
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0,
                                   const_cast< InputImageType * >( input ) );
  }

/**
 *
 */
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  void PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::SetInput(const PolylineType *input)
  {
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1,
                                   const_cast< PolylineType * >( input ) );
  }

/**
 *
 */
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  void PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::GenerateData(void)
  {
    
  }
  
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  void PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::PrintSelf(std::ostream& os, Indent indent) const
  {
  Superclass::PrintSelf(os,indent);
  os << indent << "Viewing direction: "
     << static_cast<typename NumericTraits<VectorType>::PrintType>(m_Vector)
     << std::endl;
  }
} // end namespace itk
#endif
