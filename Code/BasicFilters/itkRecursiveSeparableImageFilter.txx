/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveSeparableImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
#include "itkProgressReporter.h"
#include <new>


namespace itk
{
  
template <typename TInputImage, typename TOutputImage>
RecursiveSeparableImageFilter<TInputImage,TOutputImage>
::RecursiveSeparableImageFilter()
{
  m_Direction = 0;
  this->SetNumberOfRequiredOutputs( 1 );
  this->SetNumberOfRequiredInputs( 1 );
}


/**
 * Set Input Image
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage>
::SetInputImage( const TInputImage * input )
{
  // ProcessObject is not const_correct so this const_cast is required
  ProcessObject::SetNthInput(0, 
                             const_cast< TInputImage * >(input) );
}




/**
 * Get Input Image
 */
template <typename TInputImage, typename TOutputImage>
const TInputImage *
RecursiveSeparableImageFilter<TInputImage,TOutputImage>
::GetInputImage( void )
{
  return dynamic_cast<const TInputImage *>(
    (ProcessObject::GetInput(0)));
}





/**
 * Apply Recursive Filter 
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage>
::FilterDataArray(RealType *outs,const RealType *data,
                  RealType *scratch,unsigned int ln) 
{
  /**
   * Causal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType outV1 = data[0];

  /**
   * Initialize borders
   */
  scratch[0] = RealType( m_N00 * outV1   + m_N11 * outV1   + m_N22 * outV1   + m_N33 * outV1    );
  scratch[1] = RealType( m_N00 * data[1] + m_N11 * outV1   + m_N22 * outV1   + m_N33 * outV1    );
  scratch[2] = RealType( m_N00 * data[2] + m_N11 * data[1] + m_N22 * outV1   + m_N33 * outV1    );
  scratch[3] = RealType( m_N00 * data[3] + m_N11 * data[2] + m_N22 * data[1] + m_N33 * outV1    );

  // note that the outV1 value is multiplied by the Boundary coefficients m_BNi
  scratch[0] -= RealType( m_BN1 * outV1 + m_BN2 * outV1 + m_BN3 * outV1  + m_BN4 * outV1 );
  scratch[1] -= RealType( m_D11 * scratch[0] + m_BN2 * outV1 + m_BN3 * outV1  + m_BN4 * outV1 );
  scratch[2] -= RealType( m_D11 * scratch[1] + m_D22 * scratch[0] + m_BN3 * outV1  + m_BN4 * outV1 );
  scratch[3] -= RealType( m_D11 * scratch[2] + m_D22 * scratch[1] + m_D33 * scratch[0]  + m_BN4 * outV1 );

  /**
   * Recursively filter the rest
   */
  for( unsigned int i=4; i<ln; i++ ) 
    {
    scratch[i]  = RealType( m_N00 * data[i] + m_N11 * data[i-1] + m_N22 * data[i-2] + m_N33 * data[i-3] );
    scratch[i] -= RealType( m_D11 * scratch[i-1] + m_D22 *   scratch[i-2] + m_D33 *   scratch[i-3] + m_D44 *   scratch[i-4] );
    }

  /**
   * Store the causal result
   */
  for( unsigned int i=0; i<ln; i++ ) 
    {
    outs[i] = RealType( m_K * scratch[i] );
    }


  
  /**
   * AntiCausal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType outV2 = data[ln-1];

  /**
   * Initialize borders
   */
  scratch[ln-1] = RealType( m_M11 * outV2      + m_M22 * outV2      + m_M33 * outV2      + m_M44 * outV2);
  scratch[ln-2] = RealType( m_M11 * data[ln-1] + m_M22 * outV2      + m_M33 * outV2      + m_M44 * outV2); 
  scratch[ln-3] = RealType( m_M11 * data[ln-2] + m_M22 * data[ln-1] + m_M33 * outV2      + m_M44 * outV2); 
  scratch[ln-4] = RealType( m_M11 * data[ln-3] + m_M22 * data[ln-2] + m_M33 * data[ln-1] + m_M44 * outV2);

  // note that the outV2value is multiplied by the Boundary coefficients m_BMi
  scratch[ln-1] -= RealType( m_BM1 * outV2    + m_BM2 * outV2    + m_BM3 * outV2    + m_BM4 * outV2);
  scratch[ln-2] -= RealType( m_D11 * scratch[ln-1] + m_BM2 * outV2    + m_BM3 * outV2    + m_BM4 * outV2);
  scratch[ln-3] -= RealType( m_D11 * scratch[ln-2] + m_D22 * scratch[ln-1] + m_BM3 * outV2    + m_BM4 * outV2);
  scratch[ln-4] -= RealType( m_D11 * scratch[ln-3] + m_D22 * scratch[ln-2] + m_D33 * scratch[ln-1] + m_BM4 * outV2);

  /**
   * Recursively filter the rest
   */
  for( unsigned int i=ln-4; i>0; i-- ) 
    {
    scratch[i-1]  = RealType( m_M11 * data[i] + m_M22 * data[i+1] + m_M33 * data[i+2] + m_M44 * data[i+3] );
    scratch[i-1] -= RealType( m_D11 *   scratch[i] + m_D22 *   scratch[i+1] + m_D33 *   scratch[i+2] + m_D44 *   scratch[i+3] );
    }

  /**
   * Roll the antiCausal part into the output
   */
  for( unsigned int i=0; i<ln; i++ ) 
    {
    outs[i] += RealType( m_K * scratch[i] );
    }
}

//
//
//
template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  InputImagePointer image = const_cast<InputImageType *>( this->GetInput() );
  image->SetRequestedRegion( this->GetInput()->GetLargestPossibleRegion() );
}


//
//
//
template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  TOutputImage *out = dynamic_cast<TOutputImage*>(output);

  if (out)
    {
    out->SetRequestedRegion( out->GetLargestPossibleRegion() );
    }
}


/**
 * Compute Recursive filter
 * line by line in one of the dimensions
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage>
::GenerateData() 
{
  typedef typename TOutputImage::PixelType  OutputPixelType;

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

  outputImage->SetBufferedRegion( outputImage->GetRequestedRegion() );
  outputImage->Allocate();

  const typename InputImageType::SpacingType & pixelSize
    = inputImage->GetSpacing();
  
  this->SetUp( pixelSize[m_Direction] );
  
  RegionType region = inputImage->GetRequestedRegion();

  InputConstIteratorType  inputIterator(  inputImage,  region );
  OutputIteratorType      outputIterator( outputImage, region );

  inputIterator.SetDirection(  this->m_Direction );
  outputIterator.SetDirection( this->m_Direction );

  
  const unsigned int ln = region.GetSize()[ this->m_Direction ];

  if( ln == 0 )
    {
    itkExceptionMacro("The number of pixels along direction " << this->m_Direction << " is zero");
    }

  RealType *inps = 0;
  RealType *outs = 0;
  RealType *scratch = 0;

  try 
    {
    inps = new RealType[ ln ];
    }
  catch( std::bad_alloc & ) 
    {
    itkExceptionMacro("Problem allocating memory for internal computations");
    }

  try 
    {
    outs = new RealType[ ln ];
    }
  catch( std::bad_alloc & ) 
    {
    delete [] inps;
    itkExceptionMacro("Problem allocating memory for internal computations");
    }
  
  try 
    {
    scratch = new RealType[ln];
    }
  catch( std::bad_alloc &) 
    {
    delete [] inps;
    delete [] outs;
    itkExceptionMacro("Problem allocating memory for internal computations");
    }

  inputIterator.GoToBegin();
  outputIterator.GoToBegin();

  const typename TInputImage::OffsetValueType * offsetTable = inputImage->GetOffsetTable();
  
  const unsigned int numberOfLinesToProcess = offsetTable[ TInputImage::ImageDimension ] / ln;
  ProgressReporter progress(this,0, numberOfLinesToProcess, 10 );


  try  // this try is intended to catch an eventual AbortException.
    {
    while( !inputIterator.IsAtEnd() && !outputIterator.IsAtEnd() )
      {
      unsigned int i=0;
      while( !inputIterator.IsAtEndOfLine() )
        {
        inps[i++]      = inputIterator.Get();
        ++inputIterator;
        }

      this->FilterDataArray( outs, inps, scratch, ln );

      unsigned int j=0; 
      while( !outputIterator.IsAtEndOfLine() )
        {
        outputIterator.Set( static_cast<OutputPixelType>( outs[j++] ) );
        ++outputIterator;
        }

      inputIterator.NextLine();
      outputIterator.NextLine();

      // Although the method name is CompletedPixel(),
      // this is being called after each line is processed
      progress.CompletedPixel();  
      }
    }
  catch( ProcessAborted  & )
    {
    // User aborted filter excecution Here we catch an exception thrown by the
    // progress reporter and rethrow it with the correct line number and file
    // name. We also invoke AbortEvent in case some observer was interested on
    // it.
    // release locally allocated memory
    delete [] outs;
    delete [] inps;
    delete [] scratch;
    // Throw the final exception.
    throw ProcessAborted(__FILE__,__LINE__);
    }

  delete [] outs;
  delete [] inps;
  delete [] scratch;
}


template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Direction: " << m_Direction << std::endl;
}

} // end namespace itk

#endif



