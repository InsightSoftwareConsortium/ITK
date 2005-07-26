/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformationFieldSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef _itkDeformationFieldSource_txx
#define _itkDeformationFieldSource_txx

#include "itkDeformationFieldSource.h"
#include "itkProgressReporter.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

/**
 * Initialize new instance
 */
template <class TOutputImage>
DeformationFieldSource<TOutputImage>
::DeformationFieldSource()
{
  m_OutputSpacing.Fill(1.0);
  m_OutputOrigin.Fill(0.0);
  
  typedef ThinPlateSplineKernelTransform< 
              double, 
              itkGetStaticConstMacro( ImageDimension ) >  DefaultTransformType;

  m_KernelTransform = DefaultTransformType::New();

}


/**
 * Print out a description of self
 *
 * \todo Add details about this class
 */
template <class TOutputImage>
void 
DeformationFieldSource<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "OutputRegion:    " << m_OutputRegion << std::endl;
  os << indent << "OutputSpacing:   " << m_OutputSpacing << std::endl;
  os << indent << "OutputOrigin:    " << m_OutputOrigin << std::endl;
  os << indent << "KernelTransform: " << m_KernelTransform.GetPointer() << std::endl;
  os << indent << "Source Landmarks: " << m_SourceLandmarks.GetPointer() << std::endl;
  os << indent << "Target Landmarks: " << m_TargetLandmarks.GetPointer() << std::endl;

  return;
}



/**
 * Set the output image spacing.
 */
template <class TOutputImage>
void 
DeformationFieldSource<TOutputImage>
::SetOutputSpacing(
  const double* spacing)
{
  SpacingType s(spacing);
  this->SetOutputSpacing( s );
}


/**
 * Set the output image origin.
 */
template <class TOutputImage>
void 
DeformationFieldSource<TOutputImage>
::SetOutputOrigin(
  const double* origin)
{
  OriginPointType p(origin);
  this->SetOutputOrigin( p );
}



/**
 * Sub-sample the input deformation field and prepare the KernelBase
 * BSpline
 */
template <class TOutputImage>
void 
DeformationFieldSource<TOutputImage>
::PrepareKernelBaseSpline()
{


  // Shameful violation of const-correctness due to the
  // lack of constness in the PointSet. That is, the point
  // set is actually allowed to modify its points containers.

  LandmarkContainer * sources = 
         const_cast< LandmarkContainer * >( 
                          m_SourceLandmarks.GetPointer() );

  LandmarkContainer * targets = 
         const_cast< LandmarkContainer * >( 
                          m_TargetLandmarks.GetPointer() );

  m_KernelTransform->GetTargetLandmarks()->SetPoints( targets );
  m_KernelTransform->GetSourceLandmarks()->SetPoints( sources );

  itkDebugMacro( << "Before ComputeWMatrix() ");
  
  m_KernelTransform->ComputeWMatrix();
  
  itkDebugMacro( << "After ComputeWMatrix() ");

}




/**
 * GenerateData
 */
template <class TOutputImage>
void 
DeformationFieldSource<TOutputImage>
::GenerateData()
{

  // First subsample the input deformation field in order to create
  // the KernelBased spline.
  this->PrepareKernelBaseSpline();

  itkDebugMacro(<<"Actually executing");

  // Get the output pointers
  OutputImageType *  outputPtr = this->GetOutput();

  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create an iterator that will walk the output region for this thread.
  typedef ImageRegionIteratorWithIndex< 
                                  TOutputImage> OutputIterator;

  OutputImageRegionType region = outputPtr->GetRequestedRegion();

  OutputIterator outIt( outputPtr, region );

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  OutputIndexType outputIndex;         // Index to current output pixel

  typedef typename KernelTransformType::InputPointType  InputPointType;
  typedef typename KernelTransformType::OutputPointType OutputPointType;

  InputPointType outputPoint;    // Coordinates of current output pixel

  // Support for progress methods/callbacks
  ProgressReporter progress(this, 0, region.GetNumberOfPixels(), 10);
        
  outIt.GoToBegin();

  // Walk the output region
  while ( !outIt.IsAtEnd() )
    {
    // Determine the index of the current output pixel
    outputIndex = outIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint( outputIndex, outputPoint );


    // Compute corresponding inverse displacement vector
    OutputPointType interpolatedDeformation = 
                        m_KernelTransform->TransformPoint( outputPoint );

    OutputPixelType displacement;
    for( unsigned int i=0; i < ImageDimension; i++)
      {
      displacement[i] = interpolatedDeformation[i] - outputPoint[i];
      }

    outIt.Set( displacement );
    ++outIt;
    progress.CompletedPixel();
    }

  return;
}


/** 
 * Inform pipeline of required output region
 */
template <class TOutputImage>
void 
DeformationFieldSource<TOutputImage>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  OutputImagePointer outputPtr = this->GetOutput();
  if ( !outputPtr )
    {
    return;
    }

  // Set the size of the output region
  outputPtr->SetLargestPossibleRegion( m_OutputRegion );

  // Set spacing and origin
  outputPtr->SetSpacing( m_OutputSpacing );
  outputPtr->SetOrigin( m_OutputOrigin );

  return;
}



/** 
 * Verify if any of the components has been modified.
 */
template <class TOutputImage>
unsigned long 
DeformationFieldSource<TOutputImage>
::GetMTime( void ) const
{
  unsigned long latestTime = Object::GetMTime(); 

  if( m_KernelTransform )
    {
    if( latestTime < m_KernelTransform->GetMTime() )
      {
      latestTime = m_KernelTransform->GetMTime();
      }
    }

  if( m_SourceLandmarks )
    {
    if( latestTime < m_SourceLandmarks->GetMTime() )
      {
      latestTime = m_SourceLandmarks->GetMTime();
      }
    }


  if( m_TargetLandmarks )
    {
    if( latestTime < m_TargetLandmarks->GetMTime() )
      {
      latestTime = m_TargetLandmarks->GetMTime();
      }
    }



  return latestTime;
}



} // end namespace itk

#endif
