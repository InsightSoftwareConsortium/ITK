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
  scratch[0] = RealType( outV1   * m_N0 +   outV1 * m_N1 + outV1   * m_N2 + outV1 * m_N3    );
  scratch[1] = RealType( data[1] * m_N0 +   outV1 * m_N1 + outV1   * m_N2 + outV1 * m_N3    );
  scratch[2] = RealType( data[2] * m_N0 + data[1] * m_N1 + outV1   * m_N2 + outV1 * m_N3    );
  scratch[3] = RealType( data[3] * m_N0 + data[2] * m_N1 + data[1] * m_N2 + outV1 * m_N3    );

  // note that the outV1 value is multiplied by the Boundary coefficients m_BNi
  scratch[0] -= RealType( outV1      * m_BN1 + outV1      * m_BN2 + outV1      * m_BN3 + outV1 * m_BN4 );
  scratch[1] -= RealType( scratch[0] * m_D1  + outV1      * m_BN2 + outV1      * m_BN3  + outV1 * m_BN4 );
  scratch[2] -= RealType( scratch[1] * m_D1  + scratch[0] * m_D2  + outV1      * m_BN3  + outV1 * m_BN4 );
  scratch[3] -= RealType( scratch[2] * m_D1  + scratch[1] * m_D2  + scratch[0] * m_D3   + outV1 * m_BN4 );

  /**
   * Recursively filter the rest
   */
  for( unsigned int i=4; i<ln; i++ )
    {
    scratch[i]  = RealType( data[i]      * m_N0 + data[i-1]    * m_N1 + data[i-2]    * m_N2 + data[i-3]    * m_N3 );
    scratch[i] -= RealType( scratch[i-1] * m_D1 + scratch[i-2] * m_D2 + scratch[i-3] * m_D3 + scratch[i-4] * m_D4 );
    }

  /**
   * Store the causal result
   */
  for( unsigned int i=0; i<ln; i++ )
    {
    outs[i] = scratch[i];
    }


  
  /**
   * AntiCausal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType outV2 = data[ln-1];

  /**
   * Initialize borders
   */
  scratch[ln-1] = RealType( outV2      * m_M1 + outV2      * m_M2 + outV2      * m_M3 + outV2 * m_M4);
  scratch[ln-2] = RealType( data[ln-1] * m_M1 + outV2      * m_M2 + outV2      * m_M3 + outV2 * m_M4);
  scratch[ln-3] = RealType( data[ln-2] * m_M1 + data[ln-1] * m_M2 + outV2      * m_M3 + outV2 * m_M4);
  scratch[ln-4] = RealType( data[ln-3] * m_M1 + data[ln-2] * m_M2 + data[ln-1] * m_M3 + outV2 * m_M4);

  // note that the outV2value is multiplied by the Boundary coefficients m_BMi
  scratch[ln-1] -= RealType( outV2         * m_BM1 + outV2         * m_BM2 + outV2         * m_BM3 + outV2 * m_BM4);
  scratch[ln-2] -= RealType( scratch[ln-1] * m_D1  + outV2         * m_BM2 + outV2         * m_BM3 + outV2 * m_BM4);
  scratch[ln-3] -= RealType( scratch[ln-2] * m_D1  + scratch[ln-1] * m_D2  + outV2         * m_BM3 + outV2 * m_BM4);
  scratch[ln-4] -= RealType( scratch[ln-3] * m_D1  + scratch[ln-2] * m_D2  + scratch[ln-1] * m_D3  + outV2 * m_BM4);

  /**
   * Recursively filter the rest
   */
  for( unsigned int i=ln-4; i>0; i-- )
    {
    scratch[i-1]  = RealType( data[i]    * m_M1 + data[i+1]    * m_M2 + data[i+2]    * m_M3 + data[i+3]    * m_M4 );
    scratch[i-1] -= RealType( scratch[i] * m_D1 + scratch[i+1] * m_D2 + scratch[i+2] * m_D3 + scratch[i+3] * m_D4 );
    }

  /**
   * Roll the antiCausal part into the output
   */
  for( unsigned int i=0; i<ln; i++ )
    {
    outs[i] += scratch[i];
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
    
  typename TInputImage::ConstPointer   inputImage(    this->GetInputImage ()   );
  typename TOutputImage::Pointer       outputImage(   this->GetOutput()        );
    
 
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

  if( ln < 4 )
    {
    itkExceptionMacro("The number of pixels along direction " << this->m_Direction << " is less than 4. This filter requires a minimum of four pixels along the dimension to be processed.");
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
    ProcessAborted e(__FILE__,__LINE__);
    e.SetDescription("Process aborted.");
    e.SetLocation(ITK_LOCATION);
    throw e;
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



