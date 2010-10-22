/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkBinaryGrindPeakImageFilter.txx,v $
  Language:  C++
  Date:      $Date: 2006/12/15 21:41:24 $
  Version:   $Revision: 1.10 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryGrindPeakImageFilter_txx
#define __itkBinaryGrindPeakImageFilter_txx

#include "itkBinaryGrindPeakImageFilter.h"
#include "itkBinaryImageToShapeLabelMapFilter.h"
#include "itkShapeOpeningLabelMapFilter.h"
#include "itkLabelMapToBinaryImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template <class TInputImage>
BinaryGrindPeakImageFilter<TInputImage>
::BinaryGrindPeakImageFilter()
{
  m_FullyConnected = false;
  m_ForegroundValue = NumericTraits<InputImagePixelType>::max();
  m_BackgroundValue = NumericTraits<InputImagePixelType>::Zero;
}

template <class TInputImage>
void
BinaryGrindPeakImageFilter<TInputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}


template <class TInputImage>
void
BinaryGrindPeakImageFilter<TInputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage>
void
BinaryGrindPeakImageFilter<TInputImage>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  typedef typename itk::BinaryImageToShapeLabelMapFilter< InputImageType > LabelizerType;
  typename LabelizerType::Pointer labelizer = LabelizerType::New();
  labelizer->SetInput( this->GetInput() );
  labelizer->SetInputForegroundValue( m_ForegroundValue );
  labelizer->SetOutputBackgroundValue( m_BackgroundValue );
  labelizer->SetFullyConnected( m_FullyConnected );
  labelizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(labelizer, .6f);

  typedef typename LabelizerType::OutputImageType                  LabelMapType;
  typedef typename itk::ShapeOpeningLabelMapFilter< LabelMapType > OpeningType;
  typename OpeningType::Pointer opening = OpeningType::New();
  opening->SetInput( labelizer->GetOutput() );
  opening->SetAttribute( LabelMapType::LabelObjectType::NUMBER_OF_PIXELS_ON_BORDER );
  opening->SetLambda( 0 );
  opening->SetReverseOrdering(true);
  opening->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(opening, .1f);

  typedef typename itk::LabelMapToBinaryImageFilter< LabelMapType, OutputImageType > BinarizerType;
  typename BinarizerType::Pointer binarizer = BinarizerType::New();
  binarizer->SetInput( opening->GetOutput() );
  binarizer->SetForegroundValue( m_ForegroundValue );
  binarizer->SetBackgroundValue( m_BackgroundValue );
  binarizer->SetBackgroundImage( this->GetInput() );
  binarizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(binarizer, .2f);

  binarizer->GraftOutput( this->GetOutput() );
  binarizer->Update();
  this->GraftOutput( binarizer->GetOutput() );
}


template<class TInputImage>
void
BinaryGrindPeakImageFilter<TInputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: " << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_ForegroundValue) << std::endl;
  os << indent << "BackgroundValue: " << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_BackgroundValue) << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}

}// end namespace itk
#endif
