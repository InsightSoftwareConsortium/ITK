/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageGaussian.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFilterImageGaussian.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIterator.h"
#include <new>


namespace itk
{
  
template <class TInputImage, class TOutputImage, class TComputation>
FilterImageGaussian<TInputImage,TOutputImage,TComputation>
::FilterImageGaussian()
{
  this->SetSigma( 1.0 );
  this->SetDirection( 0 );
  this->SetNumberOfRequiredOutputs( 1 );
  this->SetNumberOfRequiredInputs( 1 );
}


/**
 * Set Input Image
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
FilterImageGaussian<TInputImage,TOutputImage,TComputation>
::SetInputImage( InputImagePointer input )
{
  ProcessObject::SetNthInput(1, input);
}




/**
 * Get Input Image
 */
template <class TInputImage, class TOutputImage, class TComputation>
TInputImage *
FilterImageGaussian<TInputImage,TOutputImage,TComputation>
::GetInputImage( void )
{
  return dynamic_cast<TInputImage *>((ProcessObject::GetInput(1)).GetPointer());
}





/**
 *   Compute filter for Gaussian kernel
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
FilterImageGaussian<TInputImage,TOutputImage,TComputation>
::SetUp(void)
{
  
  this->a0 = TComputation(  1.680  );
  this->a1 = TComputation(  3.735  );
  this->b0 = TComputation(  1.783  );
  this->b1 = TComputation(  1.723  );
  this->c0 = TComputation( -0.6803 );
  this->c1 = TComputation( -0.2598 );
  this->w0 = TComputation(  0.6318 );
  this->w1 = TComputation(  1.9970 );
  
  if( m_Spacing < TComputation( 0.0001 ) ) return;
  
  const TComputation sigmad = m_Sigma/m_Spacing;

//K = 1.0/(sigmad*sigmad*sqrt(2.0*(4.0*atan(1.0))));
  K = 1.0 / ( sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0 ) ) ) );
  
  const bool symmetric = true;
  ComputeFilterCoefficients(symmetric);

}



/**
 * Compute Recursive Filter Coefficients 
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
FilterImageGaussian<TInputImage,TOutputImage, TComputation>
::ComputeFilterCoefficients(bool symmetric) 
{

  const TComputation sigmad = m_Sigma/m_Spacing;
  
  n00  = a0 + c0;
  n11  = exp(-b1/sigmad)*(c1*sin(w1/sigmad)-(c0+2*a0)*cos(w1/sigmad)); 
  n11 += exp(-b0/sigmad)*(a1*sin(w0/sigmad)-(a0+2*c0)*cos(w0/sigmad)); 
  n22  = ((a0+c0)*cos(w1/sigmad)*cos(w0/sigmad));
  n22	-= (a1*cos(w1/sigmad)*sin(w0/sigmad)+c1*cos(w0/sigmad)*sin(w1/sigmad));
  n22	*= 2*exp(-(b0+b1)/sigmad);
  n22	+= c0*exp(-2*b0/sigmad) + a0*exp(-2*b1/sigmad);
  n33  = exp(-(b1+2*b0)/sigmad)*(c1*sin(w1/sigmad)-c0*cos(w1/sigmad));
  n33 += exp(-(b0+2*b1)/sigmad)*(a1*sin(w0/sigmad)-a0*cos(w0/sigmad));
  
  d44  = exp(-2*(b0+b1)/sigmad);
  d33  = -2*cos(w0/sigmad)*exp(-(b0+2*b1)/sigmad);
  d33 += -2*cos(w1/sigmad)*exp(-(b1+2*b0)/sigmad);
  d22  =  4*cos(w1/sigmad)*cos(w0/sigmad)*exp(-(b0+b1)/sigmad);
  d22 +=  exp(-2*b1/sigmad)+exp(-2*b0/sigmad);
  d11  =  -2*exp(-b1/sigmad)*cos(w1/sigmad)-2*exp(-b0/sigmad)*cos(w0/sigmad);
	
  if( symmetric )
    {
    m11 = n11 - d11 * n00;
    m22 = n22 - d22 * n00;
    m33 = n33 - d33 * n00;
    m44 =     - d44 * n00;
    }
  else
    {
    m11 = -( n11 - d11 * n00 );
    m22 = -( n22 - d22 * n00 );
    m33 = -( n33 - d33 * n00 );
    m44 =          d44 * n00;
    }

}


/**
 * Apply Recursive Filter 
 * two internal arrays are allocate and destroyed at each time 
 * the function is called, maybe that can be factorized somehow.
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
FilterImageGaussian<TInputImage,TOutputImage, TComputation>
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
FilterImageGaussian<TInputImage,TOutputImage, TComputation>
::Execute() 
{

  typedef ImageLinearIterator< TInputImage::PixelType,
							   TInputImage::ImageDimension>  InputIteratorType;

  typedef ImageLinearIterator< TOutputImage::PixelType,
							   TOutputImage::ImageDimension> OutputIteratorType;

  const TInputImage::Pointer   inputImage(    GetInputImage ()   );
        TOutputImage::Pointer  outputImage(   GetOutput()        );
    
 
  bool needToAllocate = false;


  const unsigned long * inputImageSize   = inputImage->GetImageSize();
  const unsigned long * outputImageSize  = outputImage->GetImageSize();
  const unsigned long * inputBufferSize  = inputImage->GetBufferSize();
  const unsigned long * outputBufferSize = outputImage->GetImageSize();

  unsigned int i = 0;

  for( i=0; i < TInputImage::ImageDimension; i++ ) 
  {
    if( inputImageSize[i] != outputImageSize[i] )
    {
	  needToAllocate = true;
	  break;
	}	

	if( inputBufferSize[i] != outputBufferSize[i] )
	{
	  needToAllocate = true;
	  break;
	}	    

  }


  if( needToAllocate )
  {       
    outputImage->SetImageSize( inputImage->GetImageSize()  );
    outputImage->SetBufferSize( inputImage->GetBufferSize() );
    outputImage->Allocate();
    outputImage->SetImageStartIndex( inputImage->GetImageStartIndex() );
    outputImage->SetBufferStartIndex( inputImage->GetBufferStartIndex() );
  }
 
  const unsigned int imageDimension = inputImage->GetImageDimension();

  if( this->m_Direction >= imageDimension )
    {
    throw ExceptionObject();
    }


  const float * pixelSize = inputImage->GetSpacing();
  m_Spacing   = pixelSize[ this->m_Direction ];
  
  SetUp();
  
  InputIteratorType inputIterator( inputImage,
                          inputImage->GetImageStartIndex(),
                          inputImage->GetBufferSize());

  OutputIteratorType outputIterator( outputImage,
                           outputImage->GetImageStartIndex(),
                           outputImage->GetBufferSize());


  inputIterator.SetDirection(  this->m_Direction );
  outputIterator.SetDirection( this->m_Direction );

  const unsigned long *imageSize = inputIterator.GetImageSize();
  const unsigned int ln     = imageSize[ this->m_Direction ];

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

  while( !inputIterator.IsAtEnd() && !outputIterator.IsAtEnd() )
  {
    
    unsigned int i=0;
    while( !inputIterator.IsAtEndOfLine() )
    {
      inps[i++]      = *inputIterator;
      ++inputIterator;
      }

    FilterDataArray( outs, inps, ln );

    unsigned int j=0; 
    while( !outputIterator.IsAtEndOfLine() )
    {
      *outputIterator  = outs[j++];
      ++outputIterator;
    }

    inputIterator.NextLine();
    outputIterator.NextLine();

  }

  delete [] outs;
  delete [] inps;

}






} // end namespace itk
