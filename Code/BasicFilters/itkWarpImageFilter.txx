/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWarpImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWarpImageFilter_txx
#define __itkWarpImageFilter_txx
#include "itkWarpImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Default constructor.
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::WarpImageFilter()
{
  // Setup the number of required inputs
  this->SetNumberOfRequiredInputs( 2 );  
  
  // Setup default values
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_OutputSpacing[j] = 1.0;
    m_OutputOrigin[j] = 0.0;
    }

  m_EdgePaddingValue = NumericTraits<PixelType>::Zero;

  // Setup default interpolator
  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_Interpolator = 
      static_cast<InterpolatorType*>( interp.GetPointer() );

}

/**
 * Standard PrintSelf method.
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{

  Superclass::PrintSelf(os, indent);

  unsigned int j;
  os << indent << "OutputSpacing: [";

  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_OutputSpacing[j] << ", ";
    }
  os << m_OutputSpacing[j] << "]" << std::endl;

  os << indent << "OutputOrigin: [";
  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_OutputOrigin[j] << ", ";
    }
  os << m_OutputOrigin[j] << "]" << std::endl;

  os << indent << "EdgePaddingValue: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>(m_EdgePaddingValue)
     << std::endl;

  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  
}


/**
 * Set the output image spacing.
 *
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::SetOutputSpacing(
const double spacing[ImageDimension] )
{

  unsigned int j; 
  for ( j = 0; j < ImageDimension; j++)
    {
    if ( spacing[j] != m_OutputSpacing[j] )
      {
      break;
      }
    } 
  if ( j < ImageDimension ) 
    { 
    this->Modified(); 
    for ( j = 0; j < ImageDimension; j++)
      {
      m_OutputSpacing[j] = spacing[j];
      }
    } 

}


/**
 * Set the output image origin.
 *
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::SetOutputOrigin(
const double origin[ImageDimension] )
{

  unsigned int j; 
  for ( j = 0; j < ImageDimension; j++)
    {
    if ( origin[j] != m_OutputOrigin[j] )
      {
      break;
      }
    } 
  if ( j < ImageDimension ) 
    { 
    this->Modified(); 
    for ( j = 0; j < ImageDimension; j++)
      {
      m_OutputOrigin[j] = origin[j];
      }
    } 

}


/**
 * Set deformation field as Inputs[1] for this ProcessObject.
 *
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::SetDeformationField(
DeformationFieldType * field )
{
  this->ProcessObject::SetNthInput( 1, field );
}


/**
 * Return a pointer to the deformation field.
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
typename WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::DeformationFieldType *
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::GetDeformationField(void)
{
  return static_cast<DeformationFieldType *>
    ( this->ProcessObject::GetInput( 1 ));
}


/**
 * Setup state of filter before multi-threading.
 * InterpolatorType::SetInputImage is not thread-safe and hence
 * has to be setup before ThreadedGenerateData
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::BeforeThreadedGenerateData()
{

  if( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator not set");
    throw ExceptionObject(__FILE__,__LINE__ );
    }

  // Connect input image to interpolator
  m_Interpolator->SetInputImage( this->GetInput() );

}


/**
 * Compute the output for the region specified by outputRegionForThread.
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::ThreadedGenerateData(
const OutputImageRegionType& outputRegionForThread,
int threadId )
{

  InputImageConstPointer inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();
  DeformationFieldPointer fieldPtr = this->GetDeformationField();

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
  
  // iterator for the output image
  ImageRegionIteratorWithIndex<OutputImageType> outputIt(
    outputPtr, outputRegionForThread );

  // iterator for the deformation field
  ImageRegionIterator<DeformationFieldType> fieldIt(
    fieldPtr, outputRegionForThread );
  
  IndexType index;
  PointType point;
  DisplacementType displacement;

  while( !outputIt.IsAtEnd() )
    {
    // get the output image index
    index = outputIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint( index, point );

    // get the required displacement
    displacement = fieldIt.Get();

    // compute the required input image point
    for(unsigned int j = 0; j < ImageDimension; j++ )
      {
      point[j] += displacement[j];
      }

    // get the interpolated value
    if( m_Interpolator->IsInsideBuffer( point ) )
      {
      PixelType value = static_cast<PixelType>( 
          m_Interpolator->Evaluate( point ) );
      outputIt.Set( value );
      }
    else
      {
      outputIt.Set( m_EdgePaddingValue );
      }   
    ++outputIt;
    ++fieldIt; 
    progress.CompletedPixel();
    }

}


template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::GenerateInputRequestedRegion()
{

  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // request the largest possible region for the input image
  InputImagePointer inputPtr = 
    const_cast< InputImageType * >( this->GetInput() );

  if( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }

  // just propagate up the output requested region for the 
  // deformation field.
  DeformationFieldPointer fieldPtr = this->GetDeformationField();
  OutputImagePointer outputPtr = this->GetOutput();
  if( fieldPtr )
    {
    fieldPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }

}


template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpImageFilter<TInputImage,TOutputImage,TDeformationField>
::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  OutputImagePointer outputPtr = this->GetOutput();

  outputPtr->SetSpacing( m_OutputSpacing );
  outputPtr->SetOrigin( m_OutputOrigin );

  DeformationFieldPointer fieldPtr = this->GetDeformationField();
  if( fieldPtr )
    {
    outputPtr->SetLargestPossibleRegion( fieldPtr->
      GetLargestPossibleRegion() );
    }

}


} // end namespace itk

#endif
 
