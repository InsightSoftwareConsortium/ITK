/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnsupervisedClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
namespace itk
{

template<class TInputImage, class TClassifiedImage>
UnsupervisedClassifier<TInputImage,TClassifiedImage>
::UnsupervisedClassifier( void ):
  m_NumClasses(0)
{
}

template<class TInputImage, class TClassifiedImage>
UnsupervisedClassifier<TInputImage,TClassifiedImage>
::~UnsupervisedClassifier( void )
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TClassifiedImage>
void
UnsupervisedClassifier<TInputImage,TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent )
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Unsupervised Classifier / Clusterer" << std::endl;

}// end PrintSelf



} // namespace itk















