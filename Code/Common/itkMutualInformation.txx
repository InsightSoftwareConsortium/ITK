/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformation.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/

namespace itk
{

/**
 * Default constructor
 */
template <class TRefImage, class TTestImage>
MutualInformation<TRefImage,TTestImage>
::MutualInformation()
{
  
  m_RefImage = NULL;
  m_TestImage = NULL;
  m_MutualInformation = 0.0;

}


/**
 * PrintSelf
 */
template <class TRefImage, class TTestImage>
void
MutualInformation<TRefImage,TTestImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Mutual information" << std::endl;

}

/**
 * Set the input reference image.
 */
template <class TRefImage, class TTestImage>
void
MutualInformation<TRefImage,TTestImage>
::SetReferenceImage(
TRefImage * ptr )
{
  m_RefImage = ptr;
}


/**
 * Set the input test image.
 */
template <class TRefImage, class TTestImage>
void
MutualInformation<TRefImage,TTestImage>
::SetTestImage(
TTestImage * ptr )
{
  m_TestImage = ptr;
}


} // namespace itk