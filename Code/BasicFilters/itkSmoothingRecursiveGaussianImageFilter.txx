/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmoothingRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSmoothingRecursiveGaussianImageFilter_txx
#define _itkSmoothingRecursiveGaussianImageFilter_txx

#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{


/**
 * Constructor
 */
template <typename TInputImage, typename TOutputImage >
SmoothingRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SmoothingRecursiveGaussianImageFilter()
{

  m_NormalizeAcrossScale = false;

  m_ProgressCommand = CommandType::New();
  m_ProgressCommand->SetCallbackFunction( this, & Self::ReportProgress );
  m_Progress  = 0.0f;

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
    {
    m_SmoothingFilters[ i ] = InternalGaussianFilterType::New();
    m_SmoothingFilters[ i ]->SetOrder( InternalGaussianFilterType::ZeroOrder );
    m_SmoothingFilters[ i ]->SetNormalizeAcrossScale( m_NormalizeAcrossScale );
    m_SmoothingFilters[ i ]->SetDirection( i+1 );
    m_SmoothingFilters[ i ]->AddObserver( ProgressEvent(), m_ProgressCommand );
    m_SmoothingFilters[ i ]->ReleaseDataFlagOn();
    }

  m_FirstSmoothingFilter = FirstGaussianFilterType::New();
  m_FirstSmoothingFilter->SetOrder( FirstGaussianFilterType::ZeroOrder );
  m_FirstSmoothingFilter->SetDirection( 0 );
  m_FirstSmoothingFilter->SetNormalizeAcrossScale( m_NormalizeAcrossScale );
  m_FirstSmoothingFilter->AddObserver( ProgressEvent(), m_ProgressCommand );
  
  m_FirstSmoothingFilter->SetInput( this->GetInput() );

  m_SmoothingFilters[0]->SetInput( m_FirstSmoothingFilter->GetOutput() );

  for( unsigned int i = 1; i<ImageDimension-1; i++ )
    {
    m_SmoothingFilters[ i ]->SetInput( 
      m_SmoothingFilters[i-1]->GetOutput() );
    }
  
  m_CastingFilter = CastingFilterType::New();

  m_CastingFilter->SetInput( 
    m_SmoothingFilters[ImageDimension-2]->GetOutput() );

  this->SetSigma( 1.0 );

}



/**
 *  Report progress by weigthing contributions of internal filters
 */
template <typename TInputImage, typename TOutputImage>
void 
SmoothingRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::ReportProgress(const Object * object, const EventObject & event )
{
  const ProcessObject * internalFilter = 
    dynamic_cast<const ProcessObject *>( object );

  if( typeid( event ) == typeid( ProgressEvent() ) )
    {
    const float filterProgress    = internalFilter->GetProgress();
    const float weightedProgress  = filterProgress / ImageDimension;
    m_Progress += weightedProgress;
    this->UpdateProgress( m_Progress );
    }
}


/**
 * Set value of Sigma
 */
template <typename TInputImage, typename TOutputImage>
void 
SmoothingRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SetSigma( RealType sigma )
{

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
    {
    m_SmoothingFilters[ i ]->SetSigma( sigma );
    }
  m_FirstSmoothingFilter->SetSigma( sigma );

  this->Modified();

}



/**
 * Set Normalize Across Scale Space
 */
template <typename TInputImage, typename TOutputImage>
void 
SmoothingRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SetNormalizeAcrossScale( bool normalize )
{

  m_NormalizeAcrossScale = normalize;

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
    {
    m_SmoothingFilters[ i ]->SetNormalizeAcrossScale( normalize );
    }
  m_FirstSmoothingFilter->SetNormalizeAcrossScale( normalize );

  this->Modified();

}


//
//
//
template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  typename SmoothingRecursiveGaussianImageFilter<TInputImage,TOutputImage>::InputImagePointer image = const_cast<InputImageType *>( this->GetInput() );
  image->SetRequestedRegion( this->GetInput()->GetLargestPossibleRegion() );
}


//
//
//
template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  TOutputImage *out = dynamic_cast<TOutputImage*>(output);

  if (out)
    {
    out->SetRequestedRegion( out->GetLargestPossibleRegion() );
    }
}

/**
 * Compute filter for Gaussian kernel
 */
template <typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter<TInputImage,TOutputImage >
::GenerateData(void)
{

  itkDebugMacro(<< "SmoothingRecursiveGaussianImageFilter generating data ");

  m_Progress = 0.0f;

  const typename TInputImage::ConstPointer   inputImage( this->GetInput() );

  m_FirstSmoothingFilter->SetInput( inputImage );
  m_CastingFilter->Update();
  this->GraftOutput(m_CastingFilter->GetOutput());

}


template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
}


} // end namespace itk

#endif
