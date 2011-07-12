/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkMeanSquaresImageToImageMetric_txx
#define __itkMeanSquaresImageToImageMetric_txx

#include "itkMeanSquaresImageToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageIterator.h"
#include "vnl/vnl_math.h"

namespace itk
{
/**
 * Constructor
 */
template< class TFixedImage, class TMovingImage >
MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::MeanSquaresImageToImageMetric()
{
  this->SetComputeGradient(true);

  m_ThreaderMSE = NULL;
  m_ThreaderMSEDerivatives = NULL;
  this->m_WithinThreadPreProcess = false;
  this->m_WithinThreadPostProcess = false;

  //  For backward compatibility, the default behavior is to use all the pixels
  //  in the fixed image.
  //  This should be fixed in ITKv4 so that this metric behaves as the others.
  this->SetUseAllPixels(true);
}

template< class TFixedImage, class TMovingImage >
MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::~MeanSquaresImageToImageMetric()
{
  if ( m_ThreaderMSE != NULL )
    {
    delete[] m_ThreaderMSE;
    }
  m_ThreaderMSE = NULL;

  if ( m_ThreaderMSEDerivatives != NULL )
    {
    delete[] m_ThreaderMSEDerivatives;
    }
  m_ThreaderMSEDerivatives = NULL;
}

/**
 * Print out internal information about this class
 */
template< class TFixedImage, class TMovingImage  >
void
MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/**
 * Initialize
 */
template< class TFixedImage, class TMovingImage >
void
MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::Initialize(void)
throw ( ExceptionObject )
{
  this->Superclass::Initialize();
  this->Superclass::MultiThreadingInitialize();

  if ( m_ThreaderMSE != NULL )
    {
    delete[] m_ThreaderMSE;
    }
  m_ThreaderMSE = new double[this->m_NumberOfThreads];

  if ( m_ThreaderMSEDerivatives != NULL )
    {
    delete[] m_ThreaderMSEDerivatives;
    }
  m_ThreaderMSEDerivatives = new DerivativeType[this->m_NumberOfThreads];
  for ( ThreadIdType threadID = 0; threadID < this->m_NumberOfThreads; threadID++ )
    {
    m_ThreaderMSEDerivatives[threadID].SetSize(this->m_NumberOfParameters);
    }
}

template< class TFixedImage, class TMovingImage  >
inline bool
MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::GetValueThreadProcessSample(ThreadIdType threadID,
                              SizeValueType fixedImageSample,
                              const MovingImagePointType & itkNotUsed(mappedPoint),
                              double movingImageValue) const
{
  double diff = movingImageValue - this->m_FixedImageSamples[fixedImageSample].value;

  m_ThreaderMSE[threadID] += diff * diff;

  return true;
}

template< class TFixedImage, class TMovingImage  >
typename MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::MeasureType
MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::GetValue(const ParametersType & parameters) const
{
  itkDebugMacro("GetValue( " << parameters << " ) ");

  if ( !this->m_FixedImage )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  memset( m_ThreaderMSE,
          0,
          this->m_NumberOfThreads * sizeof( MeasureType ) );

  // Set up the parameters in the transform
  this->m_Transform->SetParameters(parameters);
  this->m_Parameters = parameters;

  // MUST BE CALLED TO INITIATE PROCESSING
  this->GetValueMultiThreadedInitiate();

  itkDebugMacro("Ratio of voxels mapping into moving image buffer: "
                << this->m_NumberOfPixelsCounted << " / "
                << this->m_NumberOfFixedImageSamples
                << std::endl);

  if ( this->m_NumberOfPixelsCounted <
       this->m_NumberOfFixedImageSamples / 4 )
    {
    itkExceptionMacro("Too many samples map outside moving image buffer: "
                      << this->m_NumberOfPixelsCounted << " / "
                      << this->m_NumberOfFixedImageSamples
                      << std::endl);
    }

  double mse = m_ThreaderMSE[0];
  for ( unsigned int t = 1; t < this->m_NumberOfThreads; t++ )
    {
    mse += m_ThreaderMSE[t];
    }
  mse /= this->m_NumberOfPixelsCounted;

  return mse;
}

template< class TFixedImage, class TMovingImage  >
inline bool
MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeThreadProcessSample(ThreadIdType threadID,
                                           SizeValueType fixedImageSample,
                                           const MovingImagePointType & itkNotUsed(mappedPoint),
                                           double movingImageValue,
                                           const ImageDerivativesType &
                                           movingImageGradientValue) const
{
  double diff = movingImageValue - this->m_FixedImageSamples[fixedImageSample].value;

  m_ThreaderMSE[threadID] += diff * diff;

  FixedImagePointType fixedImagePoint = this->m_FixedImageSamples[fixedImageSample].point;

  // Need to use one of the threader transforms if we're
  // not in thread 0.
  //
  // Use a raw pointer here to avoid the overhead of smart pointers.
  // For instance, Register and UnRegister have mutex locks around
  // the reference counts.
  TransformType *transform;

  if ( threadID > 0 )
    {
    transform = this->m_ThreaderTransform[threadID - 1];
    }
  else
    {
    transform = this->m_Transform;
    }

  // Jacobian should be evaluated at the unmapped (fixed image) point.
  const TransformJacobianType & jacobian = transform
                                           ->GetJacobian(fixedImagePoint);

  for ( unsigned int par = 0; par < this->m_NumberOfParameters; par++ )
    {
    double sum = 0.0;
    for ( unsigned int dim = 0; dim < MovingImageDimension; dim++ )
      {
      sum += 2.0 *diff *jacobian(dim, par) * movingImageGradientValue[dim];
      }
    m_ThreaderMSEDerivatives[threadID][par] += sum;
    }

  return true;
}

/**
 * Get the both Value and Derivative Measure
 */
template< class TFixedImage, class TMovingImage  >
void
MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivative(const ParametersType & parameters,
                        MeasureType & value,
                        DerivativeType & derivative) const
{
  if ( !this->m_FixedImage )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  // Set up the parameters in the transform
  this->m_Transform->SetParameters(parameters);
  this->m_Parameters = parameters;

  // Reset the joint pdfs to zero
  memset( m_ThreaderMSE,
          0,
          this->m_NumberOfThreads * sizeof( MeasureType ) );

  // Set output values to zero
  if ( derivative.GetSize() != this->m_NumberOfParameters )
    {
    derivative = DerivativeType(this->m_NumberOfParameters);
    }
  memset( derivative.data_block(),
          0,
          this->m_NumberOfParameters * sizeof( double ) );

  for ( ThreadIdType threadID = 0; threadID < this->m_NumberOfThreads; threadID++ )
    {
    memset( m_ThreaderMSEDerivatives[threadID].data_block(),
            0,
            this->m_NumberOfParameters * sizeof( double ) );
    }

  // MUST BE CALLED TO INITIATE PROCESSING
  this->GetValueAndDerivativeMultiThreadedInitiate();

  itkDebugMacro("Ratio of voxels mapping into moving image buffer: "
                << this->m_NumberOfPixelsCounted << " / "
                << this->m_NumberOfFixedImageSamples
                << std::endl);

  if ( this->m_NumberOfPixelsCounted <
       this->m_NumberOfFixedImageSamples / 4 )
    {
    itkExceptionMacro("Too many samples map outside moving image buffer: "
                      << this->m_NumberOfPixelsCounted << " / "
                      << this->m_NumberOfFixedImageSamples
                      << std::endl);
    }

  value = 0;
  for ( unsigned int t = 0; t < this->m_NumberOfThreads; t++ )
    {
    value += m_ThreaderMSE[t];
    for ( unsigned int parameter = 0; parameter < this->m_NumberOfParameters;
          parameter++ )
      {
      derivative[parameter] += m_ThreaderMSEDerivatives[t][parameter];
      }
    }

  value /= this->m_NumberOfPixelsCounted;
  for ( unsigned int parameter = 0; parameter < this->m_NumberOfParameters;
        parameter++ )
    {
    derivative[parameter] /= this->m_NumberOfPixelsCounted;
    }
}

/**
 * Get the match measure derivative
 */
template< class TFixedImage, class TMovingImage  >
void
MeanSquaresImageToImageMetric< TFixedImage, TMovingImage >
::GetDerivative(const ParametersType & parameters,
                DerivativeType & derivative) const
{
  if ( !this->m_FixedImage )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  MeasureType value;
  // call the combined version
  this->GetValueAndDerivative(parameters, value, derivative);
}
} // end namespace itk

#endif
