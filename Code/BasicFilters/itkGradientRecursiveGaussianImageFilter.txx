/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkGradientRecursiveGaussianImageFilter_txx
#define _itkGradientRecursiveGaussianImageFilter_txx

#include "itkGradientRecursiveGaussianImageFilter.h"

namespace itk
{


/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, class TComputation>
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
::GradientRecursiveGaussianImageFilter()
{

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ] = SmoothingFilterType::New();
  }
  m_DerivativeFilter = DerivativeFilterType::New();
  

  m_SmoothingFilters[0]->SetInput( this->GetInput() );
  for( unsigned int i = 1; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ]->SetInput( 
                              m_SmoothingFilters[i-1]->GetOutput() );
  }
  m_DerivativeFilter->SetInput( 
                       m_SmoothingFilters[ImageDimension-2]->GetOutput() );
  

  m_OutputImage   = TOutputImage::New();

  m_OutputAdaptor = OutputAdaptorType::New();

  m_OutputAdaptor->SetImage( m_OutputImage );

  m_DerivativeFilter->SetOutput( m_OutputAdaptor );

  this->SetSigma( 1.0 );

}



/**
 * Set value of Sigma
 */
template <class TInputImage, class TOutputImage, class TComputation>
void 
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
::SetSigma( TComputation sigma )
{

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ]->SetSigma( sigma );
  }
  m_DerivativeFilter->SetSigma( sigma );

  this->Modified();

}




/**
 * Compute filter for Gaussian kernel
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage, TComputation>
::GenerateData(void)
{

  itkDebugMacro(<< "Hi, GradientRecursiveGaussianImageFilter generating data ");

  const typename TInputImage::Pointer   inputImage( GetInput() );

  m_OutputImage->ReleaseData();  // Free any data from previous execution

  m_OutputImage   = TOutputImage::New(); // Allocate a new one
  
  m_OutputAdaptor->SetImage( m_OutputImage );
  
  m_OutputAdaptor->SetLargestPossibleRegion( 
      inputImage->GetLargestPossibleRegion() );

  m_OutputAdaptor->SetBufferedRegion( 
      inputImage->GetBufferedRegion() );

  m_OutputAdaptor->SetRequestedRegion( 
      inputImage->GetRequestedRegion() );

  m_OutputAdaptor->Allocate();

  m_SmoothingFilters[0]->SetInput( inputImage );

  for( unsigned int dim=0; dim < ImageDimension; dim++ )
  {
    unsigned int i=0; 
    unsigned int j=0;
    while(  i< ImageDimension)
    {
      if( i == dim ) 
      {
        j++;
      }
      m_SmoothingFilters[ i ]->SetDirection( j );
    i++;
    j++;
    }
    m_DerivativeFilter->SetDirection( dim );
    m_OutputAdaptor->SelectNthElement( dim );
    m_DerivativeFilter->Update();
  }
  
  // Reconnect the image to the output
  this->SetOutput( m_OutputImage );

}


} // end namespace itk

#endif
