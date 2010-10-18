/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkBinaryReconstructionByDilationImageFilter.txx,v $
  Language:  C++
  Date:      $Date: 2006/08/01 19:16:18 $
  Version:   $Revision: 1.7 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryReconstructionByDilationImageFilter_txx
#define __itkBinaryReconstructionByDilationImageFilter_txx

#include "itkBinaryReconstructionByDilationImageFilter.h"
#include "itkProgressAccumulator.h"


namespace itk {

template<class TInputImage>
BinaryReconstructionByDilationImageFilter<TInputImage>
::BinaryReconstructionByDilationImageFilter()
{
  m_BackgroundValue = NumericTraits<OutputImagePixelType>::NonpositiveMin();
  m_ForegroundValue = NumericTraits<OutputImagePixelType>::max();
  m_FullyConnected = false;
  this->SetNumberOfRequiredInputs(2);
}

template<class TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetMarkerImage());
  if( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }

  input = const_cast<InputImageType *>(this->GetMaskImage());
  if( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}


template<class TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  typename LabelizerType::Pointer labelizer = LabelizerType::New();
  labelizer->SetInput( this->GetMaskImage() );
  labelizer->SetInputForegroundValue( m_ForegroundValue );
  labelizer->SetOutputBackgroundValue( m_BackgroundValue );
  labelizer->SetFullyConnected( m_FullyConnected );
  labelizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(labelizer, .25f);

  typename ReconstructionType::Pointer reconstruction = ReconstructionType::New();
  reconstruction->SetInput( labelizer->GetOutput() );
  reconstruction->SetMarkerImage( this->GetMarkerImage() );
  reconstruction->SetForegroundValue( m_ForegroundValue );
  reconstruction->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(reconstruction, .25f);

  typename OpeningType::Pointer opening = OpeningType::New();
  opening->SetInput( reconstruction->GetOutput() );
  opening->SetLambda( true );
  opening->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(opening, .25f);

  typename BinarizerType::Pointer binarizer = BinarizerType::New();
  binarizer->SetInput( opening->GetOutput() );
  binarizer->SetForegroundValue( m_ForegroundValue );
  binarizer->SetBackgroundValue( m_BackgroundValue );
  binarizer->SetBackgroundImage( this->GetMaskImage() );
  binarizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(binarizer, .5f);

  binarizer->GraftOutput( this->GetOutput() );
  binarizer->Update();
  this->GraftOutput( binarizer->GetOutput() );
}


template<class TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "BackgroundValue: "  << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue) << std::endl;
  os << indent << "ForegroundValue: "  << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ForegroundValue) << std::endl;
}

}// end namespace itk
#endif
