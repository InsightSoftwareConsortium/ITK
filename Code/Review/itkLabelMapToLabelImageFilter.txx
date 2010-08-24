/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelMapToLabelImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelMapToLabelImageFilter_txx
#define __itkLabelMapToLabelImageFilter_txx

#include "itkLabelMapToLabelImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
LabelMapToLabelImageFilter< TInputImage, TOutputImage >
::LabelMapToLabelImageFilter()
{}

template< class TInputImage, class TOutputImage >
void
LabelMapToLabelImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  OutputImageType *     output = this->GetOutput();
  const InputImageType *input = this->GetInput();

  output->FillBuffer( input->GetBackgroundValue() );

  Superclass::BeforeThreadedGenerateData();
}

template< class TInputImage, class TOutputImage >
void
LabelMapToLabelImageFilter< TInputImage, TOutputImage >
::ThreadedProcessLabelObject(LabelObjectType *labelObject)
{
  const typename LabelObjectType::LabelType & label = labelObject->GetLabel();

  typename InputImageType::LabelObjectType::LineContainerType::const_iterator lit;
  typename InputImageType::LabelObjectType::LineContainerType & lineContainer = labelObject->GetLineContainer();

  for ( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
    {
    IndexType     idx = lit->GetIndex();
    unsigned long length = lit->GetLength();
    for ( unsigned int i = 0; i < length; i++ )
      {
      this->GetOutput()->SetPixel(idx, label);
      idx[0]++;
      }
    }
}
} // end namespace itk
#endif
