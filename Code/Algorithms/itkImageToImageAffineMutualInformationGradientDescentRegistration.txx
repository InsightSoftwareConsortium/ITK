/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineMutualInformationGradientDescentRegistration.txx
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
#ifndef _itkImageToImageAffineMutualInformationGradientDescentRegistration_txx
#define _itkImageToImageAffineMutualInformationGradientDescentRegistration_txx

#include "itkImageToImageAffineMutualInformationGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffineMutualInformationGradientDescentRegistration()
{

  // initialize the parameter to be the identity transform
  typename ParametersType::Iterator pit = m_Parameters.Begin();
  typename ParametersType::Iterator sit = m_ScalingWeights.Begin();

  // initialize the linear part
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
    {
    for (unsigned int j=0; j<TReference::ImageDimension; j++)
      {
      *pit = 0;
      if(i == j)
	      {
	      *pit = 1;
        }
	    ++pit;

      *sit = 0.001;
      ++sit;
      }
    }

  // initialize the offset part
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
  {
    *pit = 0;
    ++pit;

    *sit = 1.0;
    ++sit;
  }

  // set default parameters
  m_NumberOfIterations = 1000;
  m_LearningRate = 1.0;
  PointType center;
  center.Fill( 0 );
  this->SetTargetTransformationCenter( center );
  this->SetReferenceTransformationCenter( center );

}


/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffineMutualInformationGradientDescentRegistration( const Self & other )
:Superclass( other )
{
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageAffineMutualInformationGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const ImageToImageAffineMutualInformationGradientDescentRegistration< TReference, TTarget> &
ImageToImageAffineMutualInformationGradientDescentRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  Superclass::operator=( other );
  return *this;
}




/**
 * Set Target transformation center
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationGradientDescentRegistration< TReference, TTarget>
::SetTargetTransformationCenter( const PointType& center )
{
  m_TargetTransformationCenter = center;

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransformation();

  transformation->SetDomainTransformationCenter(
                           m_TargetTransformationCenter );

}


/**
 * Set Reference transformation center
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationGradientDescentRegistration< TReference, TTarget>
::SetReferenceTransformationCenter( const PointType& center )
{
  m_ReferenceTransformationCenter = center;

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransformation();

  transformation->SetRangeTransformationCenter(
                           m_ReferenceTransformationCenter );

}


/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
int
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetCostFunction( this->GetMetric() );

  // setup the optimizer
  optimizer->SetMaximize();
  optimizer->SetLearningRate( m_LearningRate );
  optimizer->SetScale( m_ScalingWeights );
  optimizer->SetNumberOfIterations( m_NumberOfIterations );
  optimizer->SetInitialPosition( m_Parameters );

  // do the optimization
  optimizer->StartOptimization();

  // get the results
  m_Parameters = optimizer->GetCurrentPosition();

  return 0;

}


} // end namespace itk


#endif
