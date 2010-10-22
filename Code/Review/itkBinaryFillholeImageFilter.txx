/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkBinaryFillholeImageFilter.txx,v $
  Language:  C++
  Date:      $Date: 2006/12/15 21:41:24 $
  Version:   $Revision: 1.10 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryFillholeImageFilter_txx
#define __itkBinaryFillholeImageFilter_txx

#include "itkBinaryFillholeImageFilter.h"
#include "itkBinaryNotImageFilter.h"
#include "itkBinaryImageToShapeLabelMapFilter.h"
#include "itkShapeOpeningLabelMapFilter.h"
#include "itkLabelMapMaskImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template <class TInputImage>
BinaryFillholeImageFilter<TInputImage>
::BinaryFillholeImageFilter()
{
  m_FullyConnected = false;
  m_ForegroundValue = NumericTraits<InputImagePixelType>::max();
}

template <class TInputImage>
void
BinaryFillholeImageFilter<TInputImage>
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
BinaryFillholeImageFilter<TInputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage>
void
BinaryFillholeImageFilter<TInputImage>
::GenerateData()
{
  // let choose a background value. Background value should not be given by user
  // because closing is extensive so no background pixels will be added
  // it is just needed for internal erosion filter and constant padder
  InputImagePixelType backgroundValue = NumericTraits<InputImagePixelType>::Zero;
  if ( m_ForegroundValue == backgroundValue )
    {
    // current background value is already used for foreground value
    // choose another one
    backgroundValue = NumericTraits<InputImagePixelType>::max();
    }

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  typedef BinaryNotImageFilter< InputImageType > NotType;
  typename NotType::Pointer notInput = NotType::New();
  notInput->SetInput( this->GetInput() );
  notInput->SetForegroundValue( m_ForegroundValue );
  notInput->SetBackgroundValue( backgroundValue );
  notInput->SetNumberOfThreads( this->GetNumberOfThreads() );
  notInput->SetReleaseDataFlag( true );
  progress->RegisterInternalFilter(notInput, .2f);

  typedef typename itk::BinaryImageToShapeLabelMapFilter< InputImageType > LabelizerType;
  typename LabelizerType::Pointer labelizer = LabelizerType::New();
  labelizer->SetInput( notInput->GetOutput() );
  labelizer->SetInputForegroundValue( m_ForegroundValue );
  labelizer->SetOutputBackgroundValue( backgroundValue );
  labelizer->SetFullyConnected( m_FullyConnected );
  labelizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(labelizer, .5f);

  typedef typename LabelizerType::OutputImageType                  LabelMapType;
  typedef typename itk::ShapeOpeningLabelMapFilter< LabelMapType > OpeningType;
  typename OpeningType::Pointer opening = OpeningType::New();
  opening->SetInput( labelizer->GetOutput() );
  opening->SetAttribute( LabelMapType::LabelObjectType::NUMBER_OF_PIXELS_ON_BORDER );
  opening->SetLambda( 1 );
  opening->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(opening, .1f);

  // invert the image during the binarization
  typedef typename itk::LabelMapMaskImageFilter< LabelMapType, OutputImageType > BinarizerType;
  typename BinarizerType::Pointer binarizer = BinarizerType::New();
  binarizer->SetInput( opening->GetOutput() );
  binarizer->SetLabel( backgroundValue );
  binarizer->SetNegated( true );
  binarizer->SetBackgroundValue( m_ForegroundValue );
  binarizer->SetFeatureImage( this->GetInput() );
  binarizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(binarizer, .2f);

  binarizer->GraftOutput( this->GetOutput() );
  binarizer->Update();
  this->GraftOutput( binarizer->GetOutput() );
}


template<class TInputImage>
void
BinaryFillholeImageFilter<TInputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: " << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_ForegroundValue) << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}

}// end namespace itk
#endif
