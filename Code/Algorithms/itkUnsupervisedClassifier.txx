/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnsupervisedClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkUnsupervisedClassifier_txx
#define _itkUnsupervisedClassifier_txx

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

/*
 * PrintSelf
 */
template <class TInputImage, class TClassifiedImage>
void
UnsupervisedClassifier<TInputImage,TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

}// end PrintSelf



} // namespace itk
















#endif
