/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionalMinimaImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegionalMinimaImageFilter_txx
#define __itkRegionalMinimaImageFilter_txx

#include "itkRegionalMinimaImageFilter.h"
#include "itkValuedRegionalMinimaImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkNumericTraits.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIterator.h"

namespace itk {

template <class TInputImage, class TOutputImage>
RegionalMinimaImageFilter<TInputImage, TOutputImage>
::RegionalMinimaImageFilter()
{
  m_FullyConnected = false;
  m_FlatIsMinima = true;
  m_ForegroundValue = NumericTraits<OutputImagePixelType>::max();
  m_BackgroundValue = NumericTraits<OutputImagePixelType>::NonpositiveMin();
}

template <class TInputImage, class TOutputImage>
void 
RegionalMinimaImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if ( !input )
    {
    return; 
    }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage>
void 
RegionalMinimaImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
RegionalMinimaImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to the valued filter to find the minima
  typename ValuedRegionalMinimaImageFilter<TInputImage, TInputImage>::Pointer
    rmin = ValuedRegionalMinimaImageFilter<TInputImage, TInputImage>::New();
  rmin->SetInput( this->GetInput() );
  rmin->SetFullyConnected( m_FullyConnected );
  progress->RegisterInternalFilter( rmin, 0.67f );
  rmin->Update();

  OutputImageType * outputImage = this->GetOutput();

  if( rmin->GetFlat() )
    {
    ProgressReporter progress2(this, 0, 
     outputImage->GetRequestedRegion().GetNumberOfPixels(), 
     33, 0.67, 0.33);

    typedef ImageRegionIterator< TOutputImage > IteratorType;

    IteratorType outIt(outputImage, outputImage->GetRequestedRegion() );

    if( m_FlatIsMinima )
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
    typedef BinaryThresholdImageFilter< 
      InputImageType, OutputImageType > ThresholdType;

    typename ThresholdType::Pointer threshold = ThresholdType::New();

    threshold->SetInput( rmin->GetOutput() );
    threshold->SetUpperThreshold( rmin->GetMarkerValue() );
    threshold->SetLowerThreshold( rmin->GetMarkerValue() );
    threshold->SetOutsideValue( m_ForegroundValue );
    threshold->SetInsideValue( m_BackgroundValue );
    progress->RegisterInternalFilter( threshold, 0.33f );

    threshold->GraftOutput( outputImage );
    threshold->Update();
    this->GraftOutput( threshold->GetOutput() );
    }

}


template<class TInputImage, class TOutputImage>
void
RegionalMinimaImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "FlatIsMinima: "  << m_FlatIsMinima << std::endl;
  // FIXME : missing member variables here
}
  
}// end namespace itk
#endif
