/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageMomentsAlignmentCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageMomentsAlignmentCalculator_txx
#define _itkImageToImageMomentsAlignmentCalculator_txx

#include "itkImageToImageMomentsAlignmentCalculator.h"

namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageMomentsAlignmentCalculator<TReference, TTarget>
::ImageToImageMomentsAlignmentCalculator()
{ 

}


/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageMomentsAlignmentCalculator<TReference,  TTarget>
::~ImageToImageMomentsAlignmentCalculator()
{
}


/**
 * Set Reference 
 */
template <class TReference, class TTarget>
void
ImageToImageMomentsAlignmentCalculator<TReference, TTarget>
::SetReference( ReferenceType * reference )
{
  m_Reference   =   reference;
}


/**
 * Set Target 
 */
template <class TReference, class TTarget>
void
ImageToImageMomentsAlignmentCalculator< TReference, TTarget>
::SetTarget( TargetType * target )
{
  m_Target    =   target;
}



/**
 * Execute 
 */
template <class TReference, class TTarget>
void
ImageToImageMomentsAlignmentCalculator< TReference, TTarget>
::Execute( void )
{
  ImageMomentsCalculatorType    ReferenceMoments(m_Reference);
  ImageMomentsCalculatorType    TargetMoments(m_Target);

  AffineTransformType ReferenceTransform;
  AffineTransformType TargetTransform;
  ReferenceTransform = ReferenceMoments.GetPrincipalAxesToPhysicalAxesTransform();
  TargetTransform    = TargetMoments.GetPrincipalAxesToPhysicalAxesTransform();

  m_OutputTransform = TargetTransform;

  m_OutputTransform.Compose( ReferenceTransform.Inverse(),false) ;

}

template <class TReference, class TTarget>
void
ImageToImageMomentsAlignmentCalculator< TReference, TTarget>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Reference: " << m_Reference << std::endl;
  os << indent << "Target: " << m_Target << std::endl;
  os << indent << "OutputTransform: " << m_OutputTransform << std::endl;
}

} // end namespace itk


#endif
