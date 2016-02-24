/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkBinaryMorphologicalClosingImageFilter_hxx
#define itkBinaryMorphologicalClosingImageFilter_hxx

#include "itkBinaryMorphologicalClosingImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkCropImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkProgressReporter.h"

/*
 * This code was contributed in the Insight Journal paper:
 * "Binary morphological closing and opening image filters"
 * by Lehmann G.
 * https://hdl.handle.net/1926/141
 * http://www.insight-journal.org/browse/publication/58
 *
 */

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
BinaryMorphologicalClosingImageFilter< TInputImage, TOutputImage, TKernel >
::BinaryMorphologicalClosingImageFilter()
{
  m_ForegroundValue = NumericTraits< InputPixelType >::max();
  m_SafeBorder = true;
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
BinaryMorphologicalClosingImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateData()
{
  // Allocate the outputs
  this->AllocateOutputs();

  // let choose a background value. Background value should not be given by user
  // because closing is extensive so no background pixels will be added
  // it is just needed for internal erosion filter and constant padder
  InputPixelType backgroundValue = NumericTraits< InputPixelType >::ZeroValue();
  if ( m_ForegroundValue == backgroundValue )
    {
    // current background value is already used for foreground value
    // choose another one
    backgroundValue = NumericTraits< InputPixelType >::max();
    }

  /** set up erosion and dilation methods */
  typename BinaryDilateImageFilter< TInputImage, TInputImage, TKernel >::Pointer
  dilate = BinaryDilateImageFilter< TInputImage, TInputImage, TKernel >::New();

  typename BinaryErodeImageFilter< TInputImage, TOutputImage, TKernel >::Pointer
  erode = BinaryErodeImageFilter< TInputImage, TOutputImage, TKernel >::New();

  // create the pipeline without input and output image
  dilate->ReleaseDataFlagOn();
  dilate->SetKernel( this->GetKernel() );
  dilate->SetDilateValue(m_ForegroundValue);

  erode->SetKernel( this->GetKernel() );
  erode->ReleaseDataFlagOn();
  erode->SetErodeValue(m_ForegroundValue);
  erode->SetBackgroundValue(backgroundValue);
  erode->SetInput( dilate->GetOutput() );

  // now we have 2 cases:
  // + SafeBorder is true so we need to create a bigger image use it as input
  //   and crop the image to the normal output image size
  // + SafeBorder is false; we just have to connect filters
  if ( m_SafeBorder )
    {
    typedef ConstantPadImageFilter< InputImageType, InputImageType > PadType;
    typename PadType::Pointer pad = PadType::New();
    pad->SetPadLowerBound(this->GetKernel().GetRadius());
    pad->SetPadUpperBound(this->GetKernel().GetRadius());
    pad->SetConstant(backgroundValue);
    pad->SetInput( this->GetInput() );

    dilate->SetInput( pad->GetOutput() );

    typedef CropImageFilter< TOutputImage, TOutputImage > CropType;
    typename CropType::Pointer crop = CropType::New();
    crop->SetInput( erode->GetOutput() );
    crop->SetUpperBoundaryCropSize( this->GetKernel().GetRadius() );
    crop->SetLowerBoundaryCropSize( this->GetKernel().GetRadius() );

    ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
    progress->SetMiniPipelineFilter(this);
    progress->RegisterInternalFilter(pad, .1f);
    progress->RegisterInternalFilter(erode, .35f);
    progress->RegisterInternalFilter(dilate, .35f);
    progress->RegisterInternalFilter(crop, .1f);

    crop->GraftOutput( this->GetOutput() );
    /** execute the minipipeline */
    crop->Update();

    /** graft the minipipeline output back into this filter's output */
    this->GraftOutput( crop->GetOutput() );
    }
  else
    {
    /** set up the minipipeline */
    ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
    progress->SetMiniPipelineFilter(this);
    progress->RegisterInternalFilter(erode, .45f);
    progress->RegisterInternalFilter(dilate, .45f);

    dilate->SetInput( this->GetInput() );
    erode->GraftOutput( this->GetOutput() );

    /** execute the minipipeline */
    erode->Update();

    /** graft the minipipeline output back into this filter's output */
    this->GraftOutput( erode->GetOutput() );
    }

  // finally copy background which should have been eroded
  //
  // iterator on input image
  ImageRegionConstIterator< InputImageType > inIt =
    ImageRegionConstIterator< InputImageType >( this->GetInput(),
                                                this->GetOutput()->GetRequestedRegion() );
  // iterator on output image
  ImageRegionIterator< OutputImageType > outIt =
    ImageRegionIterator< OutputImageType >( this->GetOutput(),
                                            this->GetOutput()->GetRequestedRegion() );
  outIt.GoToBegin();
  inIt.GoToBegin();

  ProgressReporter progress2(this, 0, this->GetOutput()->GetRequestedRegion().GetNumberOfPixels(), 20, 0.9, 0.1);
  while ( !outIt.IsAtEnd() )
    {
    if ( outIt.Get() != m_ForegroundValue )
      {
      outIt.Set( static_cast< OutputPixelType >( inIt.Get() ) );
      }
    ++outIt;
    ++inIt;
    progress2.CompletedPixel();
    }

  // the end !
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
BinaryMorphologicalClosingImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_ForegroundValue ) << std::endl;
  os << indent << "SafeBorder: " << m_SafeBorder << std::endl;
}
} // end namespace itk
#endif
