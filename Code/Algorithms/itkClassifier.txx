/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkClassifier_txx
#define _itkClassifier_txx
#include "itkClassifier.h"

#include "itkCommand.h"

namespace itk
{

template<class TInputImage, class TClassifiedImage>
Classifier<TInputImage,TClassifiedImage>
::Classifier(void)
{
  m_InputImage      = NULL;
  m_ClassifiedImage = NULL;
  m_NumberOfClasses  = 0;
}

template<class TInputImage,  class TClassifiedImage>
Classifier<TInputImage,TClassifiedImage>
::~Classifier()
{

}

template<class TInputImage, class TClassifiedImage>
void
Classifier<TInputImage,TClassifiedImage>
::GenerateData()
{
  this->ClassifyImage();
}

/*
 * PrintSelf
 */
template <class TInputImage,  class TClassifiedImage>
void
Classifier<TInputImage,TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Pointer to the classified image: " << m_ClassifiedImage.GetPointer() << std::endl;
  os << indent << "Pointer to the input image     : " << m_InputImage.GetPointer() << std::endl;
  os << indent << "Number of classes              : " << m_NumberOfClasses << std::endl;

}// end PrintSelf


} // namespace itk






















#endif
