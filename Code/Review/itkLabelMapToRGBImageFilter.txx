/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkLabelMapToRGBImageFilter.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelMapToRGBImageFilter_txx
#define __itkLabelMapToRGBImageFilter_txx

#include "itkLabelMapToRGBImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk {

template <class TInputImage, class TOutputImage>
LabelMapToRGBImageFilter<TInputImage, TOutputImage>
::LabelMapToRGBImageFilter()
{
}


template<class TInputImage, class TOutputImage>
void
LabelMapToRGBImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  OutputImageType * output = this->GetOutput();
  const InputImageType * input = this->GetInput();

  FunctorType function;
  function.SetBackgroundValue( input->GetBackgroundValue() );
  output->FillBuffer( function( input->GetBackgroundValue() ) );

  Superclass::BeforeThreadedGenerateData();

}


template<class TInputImage, class TOutputImage>
void
LabelMapToRGBImageFilter<TInputImage, TOutputImage>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  const typename LabelObjectType::LabelType & label = labelObject->GetLabel();

  FunctorType function;
  function.SetBackgroundValue( this->GetInput()->GetBackgroundValue() );

  typename InputImageType::LabelObjectType::LineContainerType::const_iterator lit;
  typename InputImageType::LabelObjectType::LineContainerType & lineContainer = labelObject->GetLineContainer();

  for( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
    {
    IndexType idx = lit->GetIndex();
    unsigned long length = lit->GetLength();
    for( unsigned int i=0; i<length; i++)
      {
      this->GetOutput()->SetPixel( idx, function( label ) );
      idx[0]++;
      }
    }
}

}// end namespace itk
#endif
