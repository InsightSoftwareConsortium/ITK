/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageWatershedLevelAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
namespace itk
{

template< class TInputImage, class TOutputImage>
void
FilterImageWatershedLevelAdaptor<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate output
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();
  
  // Relabel the output
  FilterImageWatershedSegment<TInputImage, TOutputImage>
    ::CopyOutputToOutput(output, this->GetInput());
  FilterImageWatershedSegment<TInputImage, TOutputImage>
    ::RelabelImage(output,
                   FilterImageWatershedSegment<TInputImage, TOutputImage>
                   ::ExtractEquivalencyTable(this->GetInput()->GetMergeList(),
                                             m_Level));
}

} // end namespace itk
