/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShiftScaleImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkShiftScaleImageFilter_txx
#define _itkShiftScaleImageFilter_txx
#include "itkShiftScaleImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"

namespace itk {

template<class TInputImage, class TOutputImage>
ShiftScaleImageFilter<TInputImage, TOutputImage>
::ShiftScaleImageFilter()
{
  m_Shift = NumericTraits<RealType>::Zero;
  m_Scale = NumericTraits<RealType>::One;
  m_UnderflowCount = 0;
  m_OverflowCount = 0;
  m_ThreadUnderflow.resize(1);
  m_ThreadOverflow.resize(1);
}

template<class TInputImage, class TOutputImage>
ShiftScaleImageFilter<TInputImage, TOutputImage>
::~ShiftScaleImageFilter()
{
}

template<class TInputImage, class TOutputImage>
void
ShiftScaleImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData ()
{
  unsigned int i;
  int numberOfThreads = this->GetNumberOfThreads();

  //  Allocate and initialize the thread temporaries
  m_ThreadUnderflow.resize(numberOfThreads);
  m_ThreadUnderflow.Fill(0);
  m_ThreadOverflow.resize(numberOfThreads);
  m_ThreadOverflow.Fill(0);
}

template<class TInputImage, class TOutputImage>
void
ShiftScaleImageFilter<TInputImage, TOutputImage>
::AfterThreadedGenerateData ()
{
  int numberOfThreads = this->GetNumberOfThreads();

  m_UnderflowCount = 0;
  m_OverflowCount = 0;

  // Accumulate counts for each thread
  for( unsigned int i = 0; i < numberOfThreads; i++)
    {
    m_UnderflowCount += m_ThreadUnderflow[i];
    m_OverflowCount += m_ThreadOverflow[i];
    }
}

template<class TInputImage, class TOutputImage>
void
ShiftScaleImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& regionForThread,
                       int threadId) 
{
  unsigned long count = 0;;
  RealType value;
  ImageRegionIterator<TInputImage>  it (this->GetInput(), regionForThread);
  ImageRegionIterator<TOutputImage> ot (this->GetOutput(), regionForThread);
  
  // support progress methods/callbacks
  unsigned long updateVisits = 0, i=0;
  if ( threadId == 0 )
    {
    updateVisits = regionForThread.GetNumberOfPixels()/10;
    if ( updateVisits < 1 ) updateVisits = 1;
    }
        
  // shift and scale the input pixels
  while (!it.IsAtEnd())
    {
    value = (static_cast<RealType>(it.Get()) + m_Shift) * m_Scale;
    if (value < NumericTraits<OutputImagePixelType>::NonpositiveMin())
      {
      ot.Set (NumericTraits<OutputImagePixelType>::NonpositiveMin());
      m_ThreadUnderflow[threadId]++;
      }
    else if (value > NumericTraits<OutputImagePixelType>::max())
      {
      ot.Set (NumericTraits<OutputImagePixelType>::max());      
      m_ThreadOverflow[threadId]++;
      }
    else
      {
      ot.Set(value);
      }
    ++it;
    ++ot;

    if ( threadId == 0 && !(i % updateVisits ) )
      {
      this->UpdateProgress( static_cast<float>(i) / 
                            static_cast<float>(updateVisits * 10.0) );
      }
    ++i;
    }
}

template <class TInputImage, class TOutputImage>
void 
ShiftScaleImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Shift: "  << m_Shift << std::endl;
  os << indent << "Scale: "  << m_Scale << std::endl;
  os << indent << "Computed values follow:" << std::endl;
  os << indent << "UnderflowCount: "  << m_UnderflowCount << std::endl;
  os << indent << "OverflowCount: "  << m_OverflowCount << std::endl;
}


}// end namespace itk
#endif
