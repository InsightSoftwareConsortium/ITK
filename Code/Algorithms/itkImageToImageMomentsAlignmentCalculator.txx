/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageMomentsAlignmentCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000-2001 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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



} // end namespace itk


#endif
