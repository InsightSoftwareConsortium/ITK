/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkLabelMapToAttributeImageFilter.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelMapToAttributeImageFilter_txx
#define __itkLabelMapToAttributeImageFilter_txx

#include "itkLabelMapToAttributeImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk {

template <class TInputImage, class TOutputImage, class TAttributeAccessor>
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::LabelMapToAttributeImageFilter()
{
  m_BackgroundValue = NumericTraits<OutputImagePixelType>::NonpositiveMin();
}

template <class TInputImage, class TOutputImage, class TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if ( !input )
    { return; }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage, class TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage, class TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  OutputImageType * output = this->GetOutput();
  const InputImageType * input = this->GetInput();
  ProgressReporter progress( this, 0, output->GetRequestedRegion().GetNumberOfPixels() );

  AttributeAccessorType accessor;

  output->FillBuffer( m_BackgroundValue );

  typename InputImageType::LabelObjectContainerType::const_iterator it;
  const typename InputImageType::LabelObjectContainerType & labelObjectContainer = input->GetLabelObjectContainer();
  for( it = labelObjectContainer.begin(); it != labelObjectContainer.end(); it++ )
    {
    const typename InputImageType::LabelObjectType * labeObject = it->second;
    const AttributeValueType & attribute = accessor( labeObject );

    typename InputImageType::LabelObjectType::LineContainerType::const_iterator lit;
    const typename InputImageType::LabelObjectType::LineContainerType & lineContainer = labeObject->GetLineContainer();

    for( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
      {
      IndexType idx = lit->GetIndex();
      unsigned long length = lit->GetLength();
      for( unsigned int i=0; i<length; i++)
        {
        output->SetPixel( idx, static_cast<OutputImagePixelType>( attribute ) );
        idx[0]++;
        progress.CompletedPixel();
        }
      }
    }
}


template<class TInputImage, class TOutputImage, class TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "BackgroundValue: "  << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue) << std::endl;
}

}// end namespace itk
#endif
