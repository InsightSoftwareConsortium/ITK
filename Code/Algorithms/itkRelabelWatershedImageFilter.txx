/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRelabelWatershedImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef _itkRelabelWatershedImageFilter_txx
#define _itkRelabelWatershedImageFilter_txx

namespace itk
{

template< class TInputImage, class TOutputImage>
void
RelabelWatershedImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate output
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();
  
  // Relabel the output
  WatershedImageFilter<TInputImage, TOutputImage>
    ::CopyOutputToOutput(output, this->GetInput());
  WatershedImageFilter<TInputImage, TOutputImage>
    ::RelabelImage(output,
                   WatershedImageFilter<TInputImage, TOutputImage>
                   ::ExtractEquivalencyTable(this->GetInput()->GetMergeList(),
                                             m_Level));
}

} // end namespace itk

#endif
