/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveSeparableImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRecursiveSeparableImageFilter_txx
#define _itkRecursiveSeparableImageFilter_txx

#include "itkRecursiveSeparableImageFilter.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include <new>


namespace itk
{
  
template <class TInputImage, class TOutputImage, class TComputation>
RecursiveSeparableImageFilter<TInputImage,TOutputImage,TComputation>
::RecursiveSeparableImageFilter()
{
  m_Direction = 0;
  this->SetNumberOfRequiredOutputs( 1 );
  this->SetNumberOfRequiredInputs( 1 );
}


/**
 * Set Input Image
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage,TComputation>
::SetInputImage( const TInputImage * input )
{
  // ProcessObject is not const_correct so this const_cast is required
  ProcessObject::SetNthInput(0, 
          const_cast< TInputImage * >(input) );
}




/**
 * Get Input Image
 */
template <class TInputImage, class TOutputImage, class TComputation>
const TInputImage *
RecursiveSeparableImageFilter<TInputImage,TOutputImage,TComputation>
::GetInputImage( void )
{
  return dynamic_cast<const TInputImage *>(
          (ProcessObject::GetInput(0)).GetPointer());
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
    itkExceptionMacro("Problem allocating memory for internal computations");
    }


  try
    {
    s2 = new TComputation[ln];
    }
  catch( std::bad_alloc &) 
    {
    delete [] s1; 
    s1=0; 
    itkExceptionMacro("Problem allocating memory for internal computations");
    }
  
  /**
   * Causal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  TComputation outV = data[0];

  /**
   * Initialize borders
   */
  s1[0] = TComputation( m_N00 * outV    + m_N11 * outV    + m_N22 * outV    + m_N33 * outV    );
  s1[1] = TComputation( m_N00 * data[1] + m_N11 * outV    + m_N22 * outV    + m_N33 * outV    );
  s1[2] = TComputation( m_N00 * data[2] + m_N11 * data[1] + m_N22 * outV    + m_N33 * outV    );
  s1[3] = TComputation( m_N00 * data[3] + m_N11 * data[2] + m_N22 * data[1] + m_N33 * outV    );

  // note that the outV value is multiplied by the Boundary coefficients m_BNi
  s1[0] -= TComputation( m_BN1 * outV  + m_BN2 * outV  + m_BN3 * outV   + m_BN4 * outV );
  s1[1] -= TComputation( m_D11 * s1[0] + m_BN2 * outV  + m_BN3 * outV   + m_BN4 * outV );
  s1[2] -= TComputation( m_D11 * s1[1] + m_D22 * s1[0] + m_BN3 * outV   + m_BN4 * outV );
  s1[3] -= TComputation( m_D11 * s1[2] + m_D22 * s1[1] + m_D33 * s1[0]  + m_BN4 * outV );

  /**
   * Recursively filter the rest
   */
  for( i=4; i<ln; i++ ) 
    {
    s1[i]  = TComputation( m_N00 * data[i] + m_N11 * data[i-1] + m_N22 * data[i-2] + m_N33 * data[i-3] );
    s1[i] -= TComputation( m_D11 * s1[i-1] + m_D22 *   s1[i-2] + m_D33 *   s1[i-3] + m_D44 *   s1[i-4] );
    }

  /**
   * AntiCausal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  outV = data[ln-1];

  /**
   * Initialize borders
   */
  s2[ln-1] = TComputation( m_M11 * outV       + m_M22 * outV       + m_M33 * outV       + m_M44 * outV );
  s2[ln-2] = TComputation( m_M11 * data[ln-1] + m_M22 * outV       + m_M33 * outV       + m_M44 * outV ); 
  s2[ln-3] = TComputation( m_M11 * data[ln-2] + m_M22 * data[ln-1] + m_M33 * outV       + m_M44 * outV ); 
  s2[ln-4] = TComputation( m_M11 * data[ln-3] + m_M22 * data[ln-2] + m_M33 * data[ln-1] + m_M44 * outV );

  // note that the outV value is multiplied by the Boundary coefficients m_BMi
  s2[ln-1] -= TComputation( m_BM1 * outV     + m_BM2 * outV     + m_BM3 * outV     + m_BM4 * outV );
  s2[ln-2] -= TComputation( m_D11 * s2[ln-1] + m_BM2 * outV     + m_BM3 * outV     + m_BM4 * outV );
  s2[ln-3] -= TComputation( m_D11 * s2[ln-2] + m_D22 * s2[ln-1] + m_BM3 * outV     + m_BM4 * outV );
  s2[ln-4] -= TComputation( m_D11 * s2[ln-3] + m_D22 * s2[ln-2] + m_D33 * s2[ln-1] + m_BM4 * outV );

  /**
   * Recursively filter the rest
   */
  for( i=ln-4; i>0; i-- ) 
    {
    s2[i-1]  = TComputation( m_M11 * data[i] + m_M22 * data[i+1] + m_M33 * data[i+2] + m_M44 * data[i+3] );
    s2[i-1] -= TComputation( m_D11 *   s2[i] + m_D22 *   s2[i+1] + m_D33 *   s2[i+2] + m_D44 *   s2[i+3] );
    }



  /**
   * Combine Causal and AntiCausal parts
   */
  for( i=0; i<ln; i++ ) 
    {
    outs[i] = TComputation( m_K * ( s1[i] + s2[i] ) );
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

  typedef ImageLinearConstIteratorWithIndex< TInputImage  >  InputConstIteratorType;
  typedef ImageLinearIteratorWithIndex< TOutputImage >  OutputIteratorType;

  typedef ImageRegion< TInputImage::ImageDimension > RegionType;
    
  typename TInputImage::ConstPointer   inputImage(    GetInputImage ()   );
  typename TOutputImage::Pointer       outputImage(   GetOutput()        );
    
 
  const unsigned int imageDimension = inputImage->GetImageDimension();

  if( this->m_Direction >= imageDimension )
    {
    itkExceptionMacro("Direction selected for filtering is greater than ImageDimension");
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

  InputConstIteratorType  inputIterator(  inputImage,  region );
  OutputIteratorType      outputIterator( outputImage, region );

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
    itkExceptionMacro("Problem allocating memory for internal computations");
    }

  try 
    {
    outs = new TComputation[ ln ];
    }
  catch( std::bad_alloc & ) 
    {
    itkExceptionMacro("Problem allocating memory for internal computations");
    }


  inputIterator.GoToBegin();
  outputIterator.GoToBegin();

  const typename TInputImage::OffsetValueType * offsetTable = inputImage->GetOffsetTable();
  
  float       progress        = 0.0f;

  const float progressAdvance =    
                    (float)ln
                  / (float)offsetTable[ TInputImage::ImageDimension ];

  this->UpdateProgress( progress );

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

    this->UpdateProgress( progress );
    
  }

  delete [] outs;
  delete [] inps;

}

template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage, TComputation>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Direction: " << m_Direction << std::endl;
}
} // end namespace itk

#endif
