/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWarpVectorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWarpVectorImageFilter_txx
#define __itkWarpVectorImageFilter_txx
#include "itkWarpVectorImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Default constructor.
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
::WarpVectorImageFilter()
{
  // Setup the number of required inputs
  this->SetNumberOfRequiredInputs( 2 );  
  
  // Setup default values
  m_OutputSpacing.Fill( 1.0 );
  m_OutputOrigin.Fill( 0.0 );

  for (unsigned int i=0; i<PixelDimension; i++)
    {
    m_EdgePaddingValue[i] = 0;
    }

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
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{

  Superclass::PrintSelf(os, indent);

  os << indent << "OutputSpacing: " << m_OutputSpacing << std::endl;;
  os << indent << "OutputOrigin: " << m_OutputOrigin << std::endl;
  os << indent << "EdgePaddingValue: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>(m_EdgePaddingValue)
     << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  
}

/** Set the output image spacing.*/
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
::SetOutputSpacing(const double* spacing)
{
  SpacingType s(spacing);
  this->SetOutputSpacing( s );
}

/** Set the output image origin.*/
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
::SetOutputOrigin(const double* origin)
{
  PointType p(origin);
  this->SetOutputOrigin(p);
}


/** Set deformation field as Inputs[1] for this ProcessObject.*/
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
::SetDeformationField(
  DeformationFieldType * field )
{
  this->ProcessObject::SetNthInput( 1, field );
}


/**
 * Return a pointer to the deformation field.
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
typename WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
::DeformationFieldType *
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
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
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
::BeforeThreadedGenerateData()
{

  if( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator not set");
    }

  // Connect input image to interpolator
  m_Interpolator->SetInputImage( this->GetInput() );

}


/**
 * Compute the output for the region specified by outputRegionForThread.
 */
template <class TInputImage,class TOutputImage,class TDeformationField>
void
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
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
  PixelType outputValue;

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
      typedef typename InterpolatorType::OutputType  OutputType;
      const OutputType interpolatedValue = m_Interpolator->Evaluate( point );

      for( unsigned int k = 0; k < PixelDimension; k++ )
        {
        outputValue[k] = static_cast<ValueType>( interpolatedValue[k] );
        }

      outputIt.Set( outputValue );
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
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
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
WarpVectorImageFilter<TInputImage,TOutputImage,TDeformationField>
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
 
