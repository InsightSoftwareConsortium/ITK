/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientVectorFlowImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGradientVectorFlowImageFilter_txx
#define _itkGradientVectorFlowImageFilter_txx
#include "itkGradientVectorFlowImageFilter.h"

namespace itk
{
template <class TInputImage, class TOutputImage>
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::GradientVectorFlowImageFilter()
{
  for (int i=0; i<ImageDimension; i++) m_Steps[i] = 1.0;
}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::GenerateData()
{

  typename TOutputImage::Pointer output = this->GetOutput();

  output->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  output->SetBufferedRegion( this->GetInput()->GetLargestPossibleRegion() );

  output->Allocate();

  InitInterImage();

  m_TimeStep = 0.002/m_NoiseLevel; 

  int i=0;

  while ( i < 100 ) {
    UpdatePixels();
    UpdateInterImage();
    i++;
  }

}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::InitInterImage()
{
  int i;

  m_IntermediateImage = TInputImage::New();
  m_IntermediateImage->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  m_IntermediateImage->SetRequestedRegionToLargestPossibleRegion();
  m_IntermediateImage->SetBufferedRegion( m_IntermediateImage->GetRequestedRegion() );
  m_IntermediateImage->Allocate();

  for ( i=0; i<ImageDimension; i++ ) {
    m_InternalImages[i] = InternalImageType::New() ;
    m_InternalImages[i]->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
    m_InternalImages[i]->SetRequestedRegionToLargestPossibleRegion();
    m_InternalImages[i]->SetBufferedRegion( m_InternalImages[i]->GetRequestedRegion() );
    m_InternalImages[i]->Allocate();
  }

  InputImageIterator  inputIt(this->GetInput(), 
                                   this->GetInput()->GetBufferedRegion() );

  InputImageIterator  interIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  for (i=0; i<ImageDimension; i++) {
    InternalImageIterator  intIt(m_InternalImages[i], 
                                    m_InternalImages[i]->GetBufferedRegion() );
    intIt.GoToBegin();

    inputIt.GoToBegin();
    interIt.GoToBegin();

    while ( !inputIt.IsAtEnd() ) {
      interIt.Set(inputIt.Get());
      intIt.Set(inputIt.Get()[i]);
      ++intIt;
      ++interIt;
      ++inputIt;
    }
  }
}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::UpdateInterImage()
{
  int i;

  OutputImageIterator  outputIt(this->GetOutput(), 
                                   this->GetOutput()->GetBufferedRegion() );

  InputImageIterator  interIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  for (i=0; i<ImageDimension; i++) {
    InternalImageIterator  intIt(m_InternalImages[i], 
                                    m_InternalImages[i]->GetBufferedRegion() );
    intIt.GoToBegin();
  
    outputIt.GoToBegin();
    interIt.GoToBegin();

    while ( !outputIt.IsAtEnd() ) {
      interIt.Set(outputIt.Get());
      intIt.Set(interIt.Get()[i]);
      ++intIt;
      ++interIt;
      ++outputIt;
    }
  }
}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::UpdatePixels()
{

  OutputImageIterator  outputIt(this->GetOutput(), 
                                   this->GetOutput()->GetBufferedRegion() );

  InputImageIterator  interIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  InputImageIterator  inputIt(this->GetInput(), 
                                    this->GetInput()->GetBufferedRegion() );
  
  PixelType m_vec;

  int i, j;

  double b, c[ImageDimension], r;

  outputIt.GoToBegin();
  interIt.GoToBegin();
  inputIt.GoToBegin();

  while ( !outputIt.IsAtEnd() ) {
    b = 0.0;
    for (i=0; i<ImageDimension; i++) {
      b = b + inputIt.Get()[i]*inputIt.Get()[i];
      c[i] = inputIt.Get()[i];
    }
    for (i=0; i<ImageDimension; i++) {
      m_vec[i] = (1 - b*m_TimeStep)*interIt.Get()[i] + c[i]*m_TimeStep;
    }
    outputIt.Set(m_vec);
    ++interIt;
    ++outputIt;
    ++inputIt;
  }
  
  for ( i=0; i<ImageDimension; i++ ) {
    m_LaplacianFilter->SetInput(m_InternalImages[i]);
    m_LaplacianFilter->UpdateLargestPossibleRegion();
    
    InternalImageIterator  intIt(m_LaplacianFilter->GetOutput(), 
                                    m_LaplacianFilter->GetOutput()->GetBufferedRegion() );

    intIt.GoToBegin();
    outputIt.GoToBegin();

    r = m_NoiseLevel*m_TimeStep;
    for (j=0; j<ImageDimension; j++) r = r/m_Steps[j];

    while ( !outputIt.IsAtEnd() ) {
      m_vec = outputIt.Get();
      m_vec[i] = m_vec[i] + r*intIt.Get();
      outputIt.Set(m_vec);
      ++intIt;
      ++outputIt; 
    }
  }
}

} // namespace itk

#endif

