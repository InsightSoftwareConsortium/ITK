/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationBorder.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
namespace itk
{

template<class TInputImage, class TOutputImage>
SegmentationBorder<TInputImage,TOutputImage>
::SegmentationBorder(void):
  m_BorderLength(0)
{
}

template<class TInputImage, class TOutputImage>
SegmentationBorder<TInputImage,TOutputImage>
::~SegmentationBorder()
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
SegmentationBorder<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Region border object" << std::endl;

}// end PrintSelf

} // namespace itk
