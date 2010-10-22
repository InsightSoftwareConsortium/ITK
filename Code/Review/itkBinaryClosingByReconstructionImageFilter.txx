/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkBinaryAttributeOpeningImageFilter.h,v $
  Language:  C++
  Date:      $Date: 2006/03/28 19:59:05 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryClosingByReconstructionImageFilter_txx
#define __itkBinaryClosingByReconstructionImageFilter_txx

#include "itkBinaryClosingByReconstructionImageFilter.h"
#include "itkBinaryReconstructionByErosionImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkCropImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkProgressReporter.h"

namespace itk {

template<class TInputImage, class TKernel>
BinaryClosingByReconstructionImageFilter<TInputImage, TKernel>
::BinaryClosingByReconstructionImageFilter()
{
  m_ForegroundValue = NumericTraits<InputPixelType>::max();
  m_FullyConnected = false;
}

template <class TInputImage, class TKernel>
void
BinaryClosingByReconstructionImageFilter<TInputImage, TKernel>
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

template<class TInputImage, class TKernel>
void
BinaryClosingByReconstructionImageFilter<TInputImage, TKernel>
::GenerateData()
{
  // Allocate the outputs
  this->AllocateOutputs();

  // let choose a background value. Background value should not be given by user
  // because closing is extensive so no background pixels will be added
  // it is just needed for internal erosion filter and constant padder
  InputPixelType backgroundValue = NumericTraits<InputPixelType>::Zero;
  if ( m_ForegroundValue == backgroundValue )
    {
    // current background value is already used for foreground value
    // choose another one
    backgroundValue = NumericTraits<InputPixelType>::max();
    }

  /** set up erosion and dilation methods */
  typename BinaryDilateImageFilter<TInputImage, TInputImage, TKernel>::Pointer
    dilate = BinaryDilateImageFilter<TInputImage, TInputImage, TKernel>::New();

  typename BinaryReconstructionByErosionImageFilter<OutputImageType>::Pointer
    erode = BinaryReconstructionByErosionImageFilter<OutputImageType>::New();

  // create the pipeline without input and output image
  dilate->ReleaseDataFlagOn();
  dilate->SetKernel( this->GetKernel() );
  dilate->SetDilateValue( m_ForegroundValue );
  dilate->SetBackgroundValue( backgroundValue );
  dilate->SetInput( this->GetInput() );
  dilate->SetNumberOfThreads( this->GetNumberOfThreads() );

  erode->ReleaseDataFlagOn();
  erode->SetForegroundValue( m_ForegroundValue );
  erode->SetBackgroundValue( backgroundValue );
  erode->SetMarkerImage( dilate->GetOutput() );
  erode->SetFullyConnected( m_FullyConnected );
  erode->SetMaskImage( this->GetInput() );
  erode->SetNumberOfThreads( this->GetNumberOfThreads() );


  /** set up the minipipeline */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode, .8f);
  progress->RegisterInternalFilter(dilate, .2f);

  /** execute the minipipeline */
  erode->GraftOutput( this->GetOutput() );
  erode->Update();
  this->GraftOutput( erode->GetOutput() );
}

template<class TInputImage, class TKernel>
void
BinaryClosingByReconstructionImageFilter<TInputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: " << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_ForegroundValue) << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}

}// end namespace itk
#endif
