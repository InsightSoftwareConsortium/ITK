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

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

namespace itk
{
template <class TInputImage, class TOutputImage>
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::GradientVectorFlowImageFilter()
{
  m_TimeStep = 0.001;
  m_NoiseLevel = 200;
  m_IterationNum = 2;
  for (unsigned int i=0; i<ImageDimension; i++) m_Steps[i] = 1.0;
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

  m_TimeStep = 0.2/m_NoiseLevel; 

  int i=0;

  while ( i < m_IterationNum ) {
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
  unsigned int i;
  double b;
  PixelType c_vec, m_vec;

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

  m_BImage = InternalImageType::New() ;
  m_BImage->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  m_BImage->SetRequestedRegionToLargestPossibleRegion();
  m_BImage->SetBufferedRegion( m_BImage->GetRequestedRegion() );
  m_BImage->Allocate();

  m_CImage = InputImageType::New() ;
  m_CImage->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  m_CImage->SetRequestedRegionToLargestPossibleRegion();
  m_CImage->SetBufferedRegion( m_BImage->GetRequestedRegion() );
  m_CImage->Allocate();

  InputImageConstIterator  inputIt(this->GetInput(), 
                                   this->GetInput()->GetBufferedRegion() );

  InputImageIterator  intermediateIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  for (i=0; i<ImageDimension; i++) {
    InternalImageIterator  internalIt(m_InternalImages[i], 
                                    m_InternalImages[i]->GetBufferedRegion() );
    internalIt.GoToBegin();

    inputIt.GoToBegin();
    intermediateIt.GoToBegin();

    while ( !inputIt.IsAtEnd() ) {
      intermediateIt.Set(inputIt.Get());
      internalIt.Set(inputIt.Get()[i]);
      ++internalIt;
      ++intermediateIt;
      ++inputIt;
    }
  }

  InternalImageIterator  BIt(m_BImage, 
                                    m_BImage->GetBufferedRegion() );
  InputImageIterator  CIt(m_CImage, 
                                   m_CImage->GetBufferedRegion() );

  BIt.GoToBegin();
  CIt.GoToBegin();
  inputIt.GoToBegin();

  while ( !inputIt.IsAtEnd() ) {
    b = 0.0;
    m_vec = inputIt.Get();
    for (i=0; i<ImageDimension; i++) {
      b = b + m_vec[i]*m_vec[i];
    }
    for (i=0; i<ImageDimension; i++) {
      c_vec[i] =  b * m_vec[i];
    }
    BIt.Set(b);
    CIt.Set(c_vec);

    ++CIt;
    ++BIt;
    ++inputIt;
  }

}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::UpdateInterImage()
{
  unsigned int i;
  InputImageIterator  intermediateIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  for (i=0; i<ImageDimension; i++) {
    InternalImageIterator  internalIt(m_InternalImages[i], 
                                    m_InternalImages[i]->GetBufferedRegion() );

    internalIt.GoToBegin();
    intermediateIt.GoToBegin();

    while ( !intermediateIt.IsAtEnd() ) {
      internalIt.Set(intermediateIt.Get()[i]);
      ++internalIt;
      ++intermediateIt;
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

  InputImageIterator  intermediateIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  InputImageIterator  CIt(m_CImage, 
                                    m_CImage->GetBufferedRegion() );

  InternalImageIterator  BIt(m_BImage, 
                                    m_BImage->GetBufferedRegion() );
  
  PixelType m_vec, c_vec;

  unsigned int i;
  unsigned int j;

  double b; 
  double r;

  outputIt.GoToBegin();
  intermediateIt.GoToBegin();
  BIt.GoToBegin();
  CIt.GoToBegin();

  while ( !outputIt.IsAtEnd() ) {
    b = BIt.Get();
    c_vec = CIt.Get();
    
    for (i=0; i<ImageDimension; i++) {
      m_vec[i] = (1 - b*m_TimeStep)*intermediateIt.Get()[i] + c_vec[i]*m_TimeStep;
    }
    outputIt.Set(m_vec);
    ++intermediateIt;
    ++outputIt;
    ++CIt;
    ++BIt;
  }
  
  for ( i=0; i<ImageDimension; i++ ) {
    m_LaplacianFilter->SetInput(m_InternalImages[i]);
    m_LaplacianFilter->UpdateLargestPossibleRegion();
    
    InternalImageIterator  internalIt(m_LaplacianFilter->GetOutput(), 
                                    m_LaplacianFilter->GetOutput()->GetBufferedRegion() );

    internalIt.GoToBegin();
    outputIt.GoToBegin();
    intermediateIt.GoToBegin();

    r = m_NoiseLevel*m_TimeStep;
    for (j=0; j<ImageDimension; j++) r = r/m_Steps[j];

    while ( !outputIt.IsAtEnd() ) {
      m_vec = outputIt.Get();
      m_vec[i] = m_vec[i] + r*internalIt.Get();
      outputIt.Set(m_vec);
      intermediateIt.Set(m_vec);
      ++internalIt;
      ++outputIt; 
    }
  }
}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "GradientVectorFlowImageFilter" << std::endl;
}

} // namespace itk

#endif

