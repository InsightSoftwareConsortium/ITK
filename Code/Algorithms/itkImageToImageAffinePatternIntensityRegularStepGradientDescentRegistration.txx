/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration.txx
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
#ifndef _itkImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration_txx
#define _itkImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration_txx

#include "itkImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration()
{ 
  m_TranslationScale = 100.0; 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration( const Self & other )
  :Superclass( other )
{
  m_Parameters       = other.m_Parameters;
  m_TranslationScale = other.m_TranslationScale;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget> &
ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  Superclass::operator=( other );
  m_Parameters       = other.m_Parameters;
  m_TranslationScale = other.m_TranslationScale;
  return *this;
}




/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
void
ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  
  /* Initialize the rotation / shear */
  unsigned int k = 0;
  for (unsigned int col=0; col<ImageDimension; col++)
  {
    for (unsigned int row=0; row<ImageDimension; row++)
    { 
      if( col == row ) 
      {
        m_Parameters[ k++ ] = 1.0;
      }
      else 
      {
        m_Parameters[ k++ ] = 0.0;
      }
    }
  }

  /* Initialize the Offset */ 
  for (unsigned int coff=0; coff<ImageDimension; coff++)
  {
    m_Parameters[ k++ ] = 0.0;
  }

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransform();

  transformation->SetTranslationScale( m_TranslationScale );

  ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetCostFunction( this->GetMetric() );
  optimizer->SetMinimize();
  optimizer->SetScale( parametersScale );
  optimizer->SetGradientMagnitudeTolerance( 1e-9 );
  optimizer->SetMaximumStepLength( 1e-3  );
  optimizer->SetMinimumStepLength( 1e-6 );
  optimizer->SetMaximumNumberOfIterations( 100 );

  optimizer->SetInitialPosition( m_Parameters );
  optimizer->StartOptimization();

  std::cout << "The Solution is : " ;
  m_Parameters = optimizer->GetCurrentPosition();
  const unsigned int offsetStart = ImageDimension * ImageDimension;
  for(unsigned int k=0; k<ImageDimension; k++)
  {
    m_Parameters[ offsetStart + k ] *= m_TranslationScale;
  }
  std::cout << m_Parameters << std::endl;
  std::cout << std::endl;


}



} // end namespace itk


#endif
