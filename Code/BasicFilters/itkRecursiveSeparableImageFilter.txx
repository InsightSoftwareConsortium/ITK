/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveSeparableImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRecursiveSeparableImageFilter_txx
#define _itkRecursiveSeparableImageFilter_txx

#include "itkRecursiveSeparableImageFilter.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIterator.h"
#include <new>


namespace itk
{
  
template <class TInputImage, class TOutputImage, class TComputation>
RecursiveSeparableImageFilter<TInputImage,TOutputImage,TComputation>
::RecursiveSeparableImageFilter()
{
  this->SetDirection( 0 );
  this->SetNumberOfRequiredOutputs( 1 );
  this->SetNumberOfRequiredInputs( 1 );
}


/**
 * Set Input Image
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage,TComputation>
::SetInputImage( InputImagePointer input )
{
  ProcessObject::SetNthInput(0, input);
}




/**
 * Get Input Image
 */
template <class TInputImage, class TOutputImage, class TComputation>
TInputImage *
RecursiveSeparableImageFilter<TInputImage,TOutputImage,TComputation>
::GetInputImage( void )
{
  return dynamic_cast<TInputImage *>((ProcessObject::GetInput(0)).GetPointer());
}





/**
 * Apply Recursive Filter 
 * two internal arrays are allocate and destroyed at each time 
 * the function is called, maybe that can be factorized somehow.
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage, TComputation>
::FilterDataArray(TComputation *outs,const TComputation *data,unsigned int ln) 
{

  unsigned int i;

  if( !outs || !data ) return;

  TComputation *s1 = 0;
  TComputation *s2 = 0;

  try 
    {
    s1 = new TComputation[ln];
    }
  catch( std::bad_alloc &) 
    {
    throw ExceptionObject();
    }


  try
    {
    s2 = new TComputation[ln];
    }
  catch( std::bad_alloc &) 
    {
    delete [] s1; 
    s1=0; 
    throw ExceptionObject();
    }
  
  /**
   * Causal direction pass
   */

  /**
   * Initialize borders
   */
  s1[0] = TComputation( n00 * data[0] + n11 * data[1] + n22 * data[2] + n33 * data[3] );
  s1[1] = TComputation( n00 * data[1] + n11 * data[0] + n22 * data[1] + n33 * data[2] );
  s1[2] = TComputation( n00 * data[2] + n11 * data[1] + n22 * data[0] + n33 * data[1] );
  s1[3] = TComputation( n00 * data[3] + n11 * data[2] + n22 * data[1] + n33 * data[0] );

  s1[1] -= TComputation( d11 * s1[0] );
  s1[2] -= TComputation( d11 * s1[1] + d22 * s1[0] );
  s1[3] -= TComputation( d11 * s1[2] + d22 * s1[1] + d33 * s1[0] );

  /**
   * Recursively filter the rest
   */
  for( i=4; i<ln; i++ ) 
    {
    s1[i]  = TComputation( n00 * data[i] + n11 * data[i-1] + n22 * data[i-2] + n33 * data[i-3] );
    s1[i] -= TComputation( d11 * s1[i-1] + d22 *   s1[i-2] + d33 *   s1[i-3] + d44 *   s1[i-4] );
    }

  /**
   * AntiCausal direction pass
   */

  /**
   * Initialize borders
   */
  s2[ln-1] = TComputation( m11 * data[ln-2] + m22 * data[ln-3] + m33 * data[ln-4] );
  s2[ln-2] = TComputation( m11 * data[ln-1] + m22 * data[ln-2] + m33 * data[ln-3] ); 
  s2[ln-3] = TComputation( m11 * data[ln-2] + m22 * data[ln-1] + m33 * data[ln-2] ); 
  s2[ln-4] = TComputation( m11 * data[ln-3] + m22 * data[ln-2] + m33 * data[ln-1] );

  s2[ln-2] -= TComputation( d11 * s2[ln-1] );
  s2[ln-3] -= TComputation( d11 * s2[ln-2] + d22 * s2[ln-1] );
  s2[ln-4] -= TComputation( d11 * s2[ln-3] + d22 * s2[ln-2] + d33 * s2[ln-1] );

  /**
   * Recursively filter the rest
   */
  for( i=ln-4; i>0; i-- ) 
    {
    s2[i-1]  = TComputation( m11 * data[i] + m22 * data[i+1] + m33 * data[i+2] + m44 * data[i+3] );
    s2[i-1] -= TComputation( d11 *   s2[i] + d22 *   s2[i+1] + d33 *   s2[i+2] + d44 *   s2[i+3] );
    }



  /**
   * Combine Causal and AntiCausal parts
   */
  for( i=0; i<ln; i++ ) 
    {
    outs[i] = TComputation( K * ( s1[i] + s2[i] ) );
    }

  delete [] s1;  
  delete [] s2;  
	
}


/**
 * Compute Recursive filter
 * line by line in one of the dimensions
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage, TComputation>
::GenerateData() 
{

  typedef typename TOutputImage::PixelType  TOutputType;
  typedef typename TInputImage::PixelType   TInputType;

  typedef ImageLinearIterator< TInputImage  >  InputIteratorType;
  typedef ImageLinearIterator< TOutputImage >  OutputIteratorType;

  typedef ImageRegion< TInputImage::ImageDimension > RegionType;
    
  const typename TInputImage::Pointer   inputImage(    GetInputImage ()   );
        typename TOutputImage::Pointer  outputImage(   GetOutput()        );
    
 
  const unsigned int imageDimension = inputImage->GetImageDimension();

  if( this->m_Direction >= imageDimension )
    {
    throw ExceptionObject();
    }

  outputImage->SetLargestPossibleRegion( 
      inputImage->GetLargestPossibleRegion() );

  outputImage->SetBufferedRegion( 
      inputImage->GetBufferedRegion() );

  outputImage->SetRequestedRegion( 
      inputImage->GetRequestedRegion() );

  outputImage->Allocate();

  const double * pixelSize = inputImage->GetSpacing();
  m_Spacing   = pixelSize[ this->m_Direction ];
  
  SetUp();
  
  RegionType region = inputImage->GetRequestedRegion();

  InputIteratorType  inputIterator(  inputImage,  region );
  OutputIteratorType outputIterator( outputImage, region );

  inputIterator.SetDirection(  this->m_Direction );
  outputIterator.SetDirection( this->m_Direction );

  
  const unsigned int ln = region.GetSize()[ this->m_Direction ];

  TComputation *inps = 0;
  TComputation *outs = 0;

  try 
    {
    inps = new TComputation[ ln ];
    }
  catch( std::bad_alloc & ) 
    {
    throw ExceptionObject();
    }

  try 
  {
    outs = new TComputation[ ln ];
  }
  catch( std::bad_alloc & ) 
  {
    throw ExceptionObject();
  }


  inputIterator.Begin();
  outputIterator.Begin();

  const unsigned long * offsetTable = inputImage->GetOffsetTable();
  
  float       progress        = 0.0f;

  const float progressAdvance =    
                    (float)ln
                  / (float)offsetTable[ TInputImage::ImageDimension ];

  UpdateProgress( progress );

  while( !inputIterator.IsAtEnd() && !outputIterator.IsAtEnd() )
  {
    
    unsigned int i=0;
    while( !inputIterator.IsAtEndOfLine() )
    {
      inps[i++]      = inputIterator.Get();
      ++inputIterator;
      }

    FilterDataArray( outs, inps, ln );

    unsigned int j=0; 
    while( !outputIterator.IsAtEndOfLine() )
    {
      outputIterator.Set( (TOutputType)( outs[j++] ) );
      ++outputIterator;
    }

    inputIterator.NextLine();
    outputIterator.NextLine();

    progress += progressAdvance;

    UpdateProgress( progress );

  }

  delete [] outs;
  delete [] inps;

}






} // end namespace itk

#endif
