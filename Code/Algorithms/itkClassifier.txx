/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkClassifier_txx
#define _itkClassifier_txx

namespace itk
{

template<class TInputImage, class TClassifiedImage>
Classifier<TInputImage,TClassifiedImage>
::Classifier(void)
{
  m_InputImage      = NULL;
  m_ClassifiedImage = NULL;
}

template<class TInputImage,  class TClassifiedImage>
Classifier<TInputImage,TClassifiedImage>
::~Classifier()
{

}

/**
 * PrintSelf
 */
template <class TInputImage,  class TClassifiedImage>
void
Classifier<TInputImage,TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent )
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Classifier / Clusterer" << std::endl;

}// end PrintSelf


} // namespace itk






















#endif
