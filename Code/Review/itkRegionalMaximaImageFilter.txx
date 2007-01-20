/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionalMaximaImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegionalMaximaImageFilter_txx
#define __itkRegionalMaximaImageFilter_txx

#include "itkRegionalMaximaImageFilter.h"
#include "itkValuedRegionalMaximaImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkNumericTraits.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIterator.h"

namespace itk {

template <class TInputImage, class TOutputImage>
RegionalMaximaImageFilter<TInputImage, TOutputImage>
::RegionalMaximaImageFilter()
{
  m_FullyConnected = false;
  m_FlatIsMaxima = true;
  m_ForegroundValue = NumericTraits<OutputImagePixelType>::max();
  m_BackgroundValue = NumericTraits<OutputImagePixelType>::NonpositiveMin();
}

template <class TInputImage, class TOutputImage>
void 
RegionalMaximaImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if ( !input )
    { return; }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage>
void 
RegionalMaximaImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
RegionalMaximaImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to the valued filter to find the minima
  typename ValuedRegionalMaximaImageFilter<TInputImage, TInputImage>::Pointer
    rmax = ValuedRegionalMaximaImageFilter<TInputImage, TInputImage>::New();
  rmax->SetInput( this->GetInput() );
  rmax->SetFullyConnected( m_FullyConnected );
  progress->RegisterInternalFilter( rmax, 0.67f );
  rmax->Update();

  if( rmax->GetFlat() )
    {
    ProgressReporter progress2(this, 0, this->GetOutput()->GetRequestedRegion().GetNumberOfPixels(), 33, 0.67, 0.33);
    ImageRegionIterator< TOutputImage > outIt(this->GetOutput(), this->GetOutput()->GetRequestedRegion() );
    if( m_FlatIsMaxima )
      {
      for( outIt.Begin(); !outIt.IsAtEnd(); ++outIt )
        {
        outIt.Set( m_ForegroundValue );
        progress2.CompletedPixel();
        }
      }
    else
      {
      for( outIt.Begin(); !outIt.IsAtEnd(); ++outIt )
        {
        outIt.Set( m_BackgroundValue );
        progress2.CompletedPixel();
        }
      }
    }
  else
    {
    typedef BinaryThresholdImageFilter< InputImageType, OutputImageType > ThresholdType;
    typename ThresholdType::Pointer th = ThresholdType::New();
    th->SetInput( rmax->GetOutput() );
    th->SetUpperThreshold( rmax->GetMarkerValue() );
    th->SetLowerThreshold( rmax->GetMarkerValue() );
    th->SetOutsideValue( m_ForegroundValue );
    th->SetInsideValue( m_BackgroundValue );
    progress->RegisterInternalFilter( th, 0.33f );

    th->GraftOutput( this->GetOutput() );
    th->Update();
    this->GraftOutput( th->GetOutput() );
    }

}


template<class TInputImage, class TOutputImage>
void
RegionalMaximaImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "FlatIsMaxima: "  << m_FlatIsMaxima << std::endl;
  os << indent << "ForegroundValue: " << m_ForegroundValue << std::endl;
  os << indent << "BackgroundValue: " << m_BackgroundValue << std::endl;
}
  
}// end namespace itk
#endif
