/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDoubleThresholdImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDoubleThresholdImageFilter_txx
#define __itkDoubleThresholdImageFilter_txx

#include "itkDoubleThresholdImageFilter.h"
#include "itkGrayscaleGeodesicDilateImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template <class TInputImage, class TOutputImage>
DoubleThresholdImageFilter<TInputImage, TOutputImage>
::DoubleThresholdImageFilter()
  : m_NumberOfIterationsUsed( 0 )
{
  m_Threshold1 = NumericTraits<InputPixelType>::NonpositiveMin();
  m_Threshold2 = NumericTraits<InputPixelType>::NonpositiveMin();
  m_Threshold3 = NumericTraits<InputPixelType>::max();
  m_Threshold4 = NumericTraits<InputPixelType>::max();

  m_OutsideValue   = NumericTraits<OutputPixelType>::Zero;
  m_InsideValue    = NumericTraits<OutputPixelType>::max();
}

template <class TInputImage, class TOutputImage>
void 
DoubleThresholdImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage>
void 
DoubleThresholdImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
DoubleThresholdImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  // Build a mini-pipeline that involves two thresholds filters and a
  // geodesic dilation.
  typedef BinaryThresholdImageFilter<TInputImage, TOutputImage> ThresholdFilterType;
  typedef GrayscaleGeodesicDilateImageFilter<TOutputImage, TOutputImage> DilationFilterType;

  typename ThresholdFilterType::Pointer narrowThreshold = ThresholdFilterType::New();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);


  narrowThreshold->SetLowerThreshold( m_Threshold2 );
  narrowThreshold->SetUpperThreshold( m_Threshold3 );
  narrowThreshold->SetInsideValue( m_InsideValue );
  narrowThreshold->SetOutsideValue( m_OutsideValue );
  narrowThreshold->SetInput( this->GetInput() );

  typename ThresholdFilterType::Pointer wideThreshold = ThresholdFilterType::New();
  wideThreshold->SetLowerThreshold( m_Threshold1 );
  wideThreshold->SetUpperThreshold( m_Threshold4 );
  wideThreshold->SetInsideValue( m_InsideValue );
  wideThreshold->SetOutsideValue( m_OutsideValue );
  wideThreshold->SetInput( this->GetInput() );
  
  typename DilationFilterType::Pointer dilate = DilationFilterType::New();
  dilate->SetMarkerImage( narrowThreshold->GetOutput() );
  dilate->SetMaskImage( wideThreshold->GetOutput() );
  dilate->RunOneIterationOff();   // run to convergence
  
  progress->RegisterInternalFilter(narrowThreshold,.1f);
  progress->RegisterInternalFilter(wideThreshold,.1f);
  progress->RegisterInternalFilter(dilate,.8f);

  // graft our output to the dilate filter to force the proper regions
  // to be generated
  dilate->GraftOutput( this->GetOutput() );

  // reconstruction by dilation
  dilate->Update();

  // graft the output of the dilate filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( dilate->GetOutput() );

  // copy the number of iterations used
  m_NumberOfIterationsUsed = dilate->GetNumberOfIterationsUsed();
}


template<class TInputImage, class TOutputImage>
void
DoubleThresholdImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold1: "
     << static_cast<NumericTraits<InputPixelType>::PrintType>(m_Threshold1)
     << std::endl;
  os << indent << "Threshold2: "
     << static_cast<NumericTraits<InputPixelType>::PrintType>(m_Threshold2)
     << std::endl;
  os << indent << "Threshold3: "
     << static_cast<NumericTraits<InputPixelType>::PrintType>(m_Threshold3)
     << std::endl;
  os << indent << "Threshold4: "
     << static_cast<NumericTraits<InputPixelType>::PrintType>(m_Threshold4)
     << std::endl;
  os << indent << "InsideValue: "
     << static_cast<NumericTraits<OutputPixelType>::PrintType>(m_InsideValue)
     << std::endl;
  os << indent << "OutsideValue: "
     << static_cast<NumericTraits<OutputPixelType>::PrintType>(m_OutsideValue)
     << std::endl;
  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
}
  
}// end namespace itk
#endif
