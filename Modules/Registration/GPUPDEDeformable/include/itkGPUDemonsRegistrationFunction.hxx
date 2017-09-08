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
#ifndef itkGPUDemonsRegistrationFunction_hxx
#define itkGPUDemonsRegistrationFunction_hxx

#include "itkGPUDemonsRegistrationFunction.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::GPUDemonsRegistrationFunction()
{
  RadiusType   r;
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 0;
    }
  this->SetRadius(r);

  m_TimeStep = 1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  this->SetMovingImage(ITK_NULLPTR);
  this->SetFixedImage(ITK_NULLPTR);
  //m_FixedImageSpacing.Fill( 1.0 );
  //m_FixedImageOrigin.Fill( 0.0 );
  m_Normalizer = 1.0;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();

  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast< InterpolatorType * >(
    interp.GetPointer() );

  m_Metric = NumericTraits< double >::max();
  m_SumOfSquaredDifference = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_RMSChange = NumericTraits< double >::max();
  m_SumOfSquaredChange = 0.0;

  m_MovingImageGradientCalculator = MovingImageGradientCalculatorType::New();
  m_UseMovingImageGradient = false;

  /*** Prepare GPU opencl program ***/
  m_GPUPixelCounter       = ITK_NULLPTR;
  m_GPUSquaredChange      = ITK_NULLPTR;
  m_GPUSquaredDifference  = ITK_NULLPTR;

  std::ostringstream defines;

  if(TDisplacementField::ImageDimension > 3 || TDisplacementField::ImageDimension < 1)
  {
    itkExceptionMacro("GPUDenseFiniteDifferenceImageFilter supports 1/2/3D image.");
  }

  defines << "#define DIM_" << TDisplacementField::ImageDimension << "\n";

  defines << "#define IMGPIXELTYPE ";
  GetTypenameInString( typeid ( typename TFixedImage::PixelType ), defines );

  defines << "#define BUFPIXELTYPE ";
  GetTypenameInString( typeid ( typename TDisplacementField::PixelType::ValueType ), defines );

  defines << "#define OUTPIXELTYPE ";
  GetTypenameInString( typeid ( typename TDisplacementField::PixelType::ValueType ), defines );
  std::cout << "Defines: " << defines.str() << std::endl;

  const char* GPUSource = GPUDemonsRegistrationFunction::GetOpenCLSource();

  // load and build program
  this->m_GPUKernelManager->LoadProgramFromString( GPUSource, defines.str().c_str() );

  // create kernel
  m_ComputeUpdateGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("ComputeUpdate");

}

/**
 * Standard "PrintSelf" method.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "MovingImageIterpolator: ";
  os << m_MovingImageInterpolator.GetPointer() << std::endl;
  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "DenominatorThreshold: ";
  os << m_DenominatorThreshold << std::endl;
  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;

  os << indent << "UseMovingImageGradient: ";
  os << m_UseMovingImageGradient << std::endl;

  os << indent << "Metric: ";
  os << m_Metric << std::endl;
  os << indent << "SumOfSquaredDifference: ";
  os << m_SumOfSquaredDifference << std::endl;
  os << indent << "NumberOfPixelsProcessed: ";
  os << m_NumberOfPixelsProcessed << std::endl;
  os << indent << "RMSChange: ";
  os << m_RMSChange << std::endl;
  os << indent << "SumOfSquaredChange: ";
  os << m_SumOfSquaredChange << std::endl;
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::SetIntensityDifferenceThreshold(double threshold)
{
  m_IntensityDifferenceThreshold = threshold;
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::GetIntensityDifferenceThreshold() const
{
  return m_IntensityDifferenceThreshold;
}

/**
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  if ( !this->GetMovingImage() || !this->GetFixedImage() || !m_MovingImageInterpolator )
    {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or Interpolator not set");
    }

  // cache fixed image information
  SpacingType fixedImageSpacing    = this->GetFixedImage()->GetSpacing();
  m_ZeroUpdateReturn.Fill(0.0);

  // compute the normalizer
  m_Normalizer      = 0.0;
  for ( unsigned int k = 0; k < ImageDimension; k++ )
    {
    m_Normalizer += fixedImageSpacing[k] * fixedImageSpacing[k];
    }
  m_Normalizer /= static_cast< double >( ImageDimension );

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( this->GetFixedImage() );
  m_MovingImageGradientCalculator->SetInputImage( this->GetMovingImage() );

  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage( this->GetMovingImage() );

  // initialize metric computation variables
  m_SumOfSquaredDifference  = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_SumOfSquaredChange      = 0.0;

}

/**
 * Allocate GPU buffers for computing metric statitics
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::GPUAllocateMetricData(unsigned int numPixels)
{
  // allocate gpu buffers for statistics
  // if (m_GPUPixelCounter == (GPUReduction<int>::Pointer)ITK_NULLPTR)

  m_GPUPixelCounter       = GPUReduction<int>::New();
  m_GPUSquaredChange      = GPUReduction<float>::New();
  m_GPUSquaredDifference  = GPUReduction<float>::New();

  m_GPUPixelCounter->InitializeKernel(numPixels);
  m_GPUSquaredChange->InitializeKernel(numPixels);
  m_GPUSquaredDifference->InitializeKernel(numPixels);

  m_GPUPixelCounter->AllocateGPUInputBuffer();
  m_GPUSquaredChange->AllocateGPUInputBuffer();
  m_GPUSquaredDifference->AllocateGPUInputBuffer();

}

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::GPUReleaseMetricData()
{
  m_GPUPixelCounter->ReleaseGPUInputBuffer( );
  m_GPUSquaredChange->ReleaseGPUInputBuffer( );
  m_GPUSquaredDifference->ReleaseGPUInputBuffer( );
}

/**
 * Compute update at a specify neighbourhood
 */

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::GPUComputeUpdate( DisplacementFieldTypePointer output,
                    DisplacementFieldTypePointer update,
                    void *itkNotUsed(gd)
                    )
{
  TFixedImage * fixedImage  = const_cast< TFixedImage * >( this->GetFixedImage() );
  TMovingImage * movingImage = const_cast< TMovingImage * >( this->GetMovingImage() );
  typename DisplacementFieldType::SizeType outSize = output->GetLargestPossibleRegion().GetSize();

  int imgSize[3];
  imgSize[0] = imgSize[1] = imgSize[2] = 1;

  int ImageDim = (int)DisplacementFieldType::ImageDimension;

  for(int i=0; i<ImageDim; i++)
  {
    imgSize[i] = outSize[i];
  }

  size_t localSize[3], globalSize[3];
  localSize[0] = localSize[1] = localSize[2] = OpenCLGetLocalBlockSize(ImageDim);
  for(int i=0; i<ImageDim; i++)
  {
    globalSize[i] = localSize[i]*(unsigned int)ceil((float)outSize[i]/(float)localSize[i]); // total # of threads
  }

  float normalizer = 1;

  // arguments set up
  int argidx = 0;
  this->m_GPUKernelManager->SetKernelArgWithImage(m_ComputeUpdateGPUKernelHandle, argidx++, fixedImage->GetGPUDataManager());
  this->m_GPUKernelManager->SetKernelArgWithImage(m_ComputeUpdateGPUKernelHandle, argidx++, movingImage->GetGPUDataManager());
  this->m_GPUKernelManager->SetKernelArgWithImage(m_ComputeUpdateGPUKernelHandle, argidx++, output->GetGPUDataManager());
  this->m_GPUKernelManager->SetKernelArgWithImage(m_ComputeUpdateGPUKernelHandle, argidx++, update->GetGPUDataManager());

  this->m_GPUKernelManager->SetKernelArgWithImage(m_ComputeUpdateGPUKernelHandle, argidx++, m_GPUPixelCounter->GetGPUDataManager());
  this->m_GPUKernelManager->SetKernelArgWithImage(m_ComputeUpdateGPUKernelHandle, argidx++, m_GPUSquaredChange->GetGPUDataManager());
  this->m_GPUKernelManager->SetKernelArgWithImage(m_ComputeUpdateGPUKernelHandle, argidx++, m_GPUSquaredDifference->GetGPUDataManager());

  this->m_GPUKernelManager->SetKernelArg(m_ComputeUpdateGPUKernelHandle, argidx++, sizeof(float), &(normalizer));
  for(int i=0; i<ImageDim; i++)
  {
    this->m_GPUKernelManager->SetKernelArg(m_ComputeUpdateGPUKernelHandle, argidx++, sizeof(int), &(imgSize[i]));
  }

  // launch kernel
  this->m_GPUKernelManager->LaunchKernel(m_ComputeUpdateGPUKernelHandle, (int)DisplacementFieldType::ImageDimension, globalSize, localSize );

  // compute statistics
  m_GPUPixelCounter->GPUGenerateData();
  m_GPUSquaredChange->GPUGenerateData();
  m_GPUSquaredDifference->GPUGenerateData();

  m_SumOfSquaredDifference  = m_GPUSquaredDifference->GetGPUResult();
  m_NumberOfPixelsProcessed = m_GPUPixelCounter->GetGPUResult();
  m_SumOfSquaredChange      = m_GPUSquaredChange->GetGPUResult();
  //std::cout << "m_NumberOfPixelsProcessed = " << m_NumberOfPixelsProcessed << std::endl;
  if ( m_NumberOfPixelsProcessed )
    {
    m_Metric = m_SumOfSquaredDifference /
      static_cast<double>( m_NumberOfPixelsProcessed );
    m_RMSChange = std::sqrt( m_SumOfSquaredChange /
                            static_cast<double>( m_NumberOfPixelsProcessed ) );
    }
}

/**
 * Compute update at a specify neighbourhood
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
typename GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PixelType
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ComputeUpdate( const NeighborhoodType & it, void *gd,
                 const FloatOffsetType & itkNotUsed(offset) )
{
  // Get fixed image related information
  // Note: no need to check the index is within
  // fixed image buffer. This is done by the external filter.
  const IndexType index = it.GetIndex();
  const double    fixedValue = (double)this->GetFixedImage()->GetPixel(index);

  // Get moving image related information
  PointType mappedPoint;

  this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedPoint);
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    mappedPoint[j] += it.GetCenterPixel()[j];
    }

  double movingValue;
  if ( m_MovingImageInterpolator->IsInsideBuffer(mappedPoint) )
    {
    movingValue = m_MovingImageInterpolator->Evaluate(mappedPoint);
    }
  else
    {
    return m_ZeroUpdateReturn;
    }

  CovariantVectorType gradient;
  // Compute the gradient of either fixed or moving image
  if ( !m_UseMovingImageGradient )
    {
    gradient = m_FixedImageGradientCalculator->EvaluateAtIndex(index);
    }
  else
    {
    gradient = m_MovingImageGradientCalculator->Evaluate(mappedPoint);
    }

  double gradientSquaredMagnitude = 0;
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    gradientSquaredMagnitude += itk::Math::sqr(gradient[j]);
    }

  /**
   * Compute Update.
   * In the original equation the denominator is defined as (g-f)^2 + grad_mag^2.
   * However there is a mismatch in units between the two terms.
   * The units for the second term is intensity^2/mm^2 while the
   * units for the first term is intensity^2. This mismatch is particularly
   * problematic when the fixed image does not have unit spacing.
   * In this implementation, we normalize the first term by a factor K,
   * such that denominator = (g-f)^2/K + grad_mag^2
   * where K = mean square spacing to compensate for the mismatch in units.
   */
  const double speedValue = fixedValue - movingValue;
  const double sqr_speedValue = itk::Math::sqr(speedValue);

  // update the metric
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;
  if ( globalData )
    {
    globalData->m_SumOfSquaredDifference += sqr_speedValue;
    globalData->m_NumberOfPixelsProcessed += 1;
    }

  const double denominator = sqr_speedValue / m_Normalizer
                             + gradientSquaredMagnitude;

  if ( itk::Math::abs(speedValue) < m_IntensityDifferenceThreshold
       || denominator < m_DenominatorThreshold )
    {
    return m_ZeroUpdateReturn;
    }

  PixelType update;
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    update[j] = speedValue * gradient[j] / denominator;
    if ( globalData )
      {
      globalData->m_SumOfSquaredChange += itk::Math::sqr(update[j]);
      }
    }
  return update;
}

/**
 * Update the metric and release the per-thread-global data.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
GPUDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ReleaseGlobalDataPointer(void *gd) const
{
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;

  m_MetricCalculationLock.Lock();
  m_SumOfSquaredDifference += globalData->m_SumOfSquaredDifference;
  m_NumberOfPixelsProcessed += globalData->m_NumberOfPixelsProcessed;
  m_SumOfSquaredChange += globalData->m_SumOfSquaredChange;
  if ( m_NumberOfPixelsProcessed )
    {
    m_Metric = m_SumOfSquaredDifference
               / static_cast< double >( m_NumberOfPixelsProcessed );
    m_RMSChange = std::sqrt( m_SumOfSquaredChange
                            / static_cast< double >( m_NumberOfPixelsProcessed ) );
    }
  m_MetricCalculationLock.Unlock();

  delete globalData;
}

} // end namespace itk

#endif
