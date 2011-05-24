/*=========================================================================
 *
 *  Copyright Kitware Inc.
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

#ifndef __itkRegisterImageTranslationFilter_txx
#define __itkRegisterImageTranslationFilter_txx

#include <string>
#include <math.h>

namespace itk
{
//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
itk::RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::RegisterImageTranslationFilter()
{
  this->m_FixedImage  = NULL;
  this->m_MovingImage = NULL;

  this->m_Metric        = MeanSquaresMetricType::New();
  this->m_Optimizer     = OptimizerType::New();
  this->m_Interpolator  = InterpolatorType::New();
  this->m_Registration  = RegistrationType::New();

  this->m_ObserveCommand = SimpleCommandType::New();
  this->m_ObserveCommand->SetCallbackFunction( this, &Self::ObserveIteration );

  this->m_Registration->SetMetric(        this->m_Metric        );
  this->m_Registration->SetOptimizer(     this->m_Optimizer     );
  this->m_Registration->SetInterpolator(  this->m_Interpolator  );

  this->m_Optimizer->AddObserver( itk::IterationEvent(), this->m_ObserveCommand );

  this->m_SpatialObjectMask = NULL;

  this->m_RigidTransform = RigidTransformType::New();

  this->m_UseMask = false;

  this->m_NumberOfIterations = 0;

  this->m_ManualInitialTransformSet = false;
  this->m_ManualInitialTransform = RigidTransformType::New();

  this->m_ConvergenceThreshold         = 0;

  this->m_StdOutputFlag = false;

  this->ResetRegistrationConvergenceInformation();
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::~RegisterImageTranslationFilter()
{
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
void RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::ResetRegistrationConvergenceInformation()
{
  this->m_PreviousMetricValue = std::numeric_limits<double>::quiet_NaN();
  this->m_NumberOfIterationsOfCurrentRegistration = 0;
  this->m_PreviousMetricValue = std::numeric_limits<double>::quiet_NaN();
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
void RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::ObserveIteration()
{
  // Output registration convergence information in five fields:
  // Type name | Iteration # | Metric Value | Change in metric value  | change percentage

  std::string typeName = "Rigid";

  // Iteration #
  this->m_NumberOfIterationsOfCurrentRegistration++;

  // metric value
  double metricValue = this->m_Metric->GetValue(this->m_Optimizer->GetCurrentPosition());

  // metric value change
  double metricValueChange = std::numeric_limits<double>::quiet_NaN();
  if (this->m_PreviousMetricValue != std::numeric_limits<double>::quiet_NaN())
    {
    metricValueChange = metricValue - this->m_PreviousMetricValue;
    }

  // metric value change percentage
  double metricValueChangePercentage = std::numeric_limits<double>::quiet_NaN();
  if (metricValueChange != std::numeric_limits<double>::quiet_NaN())
    {
    metricValueChangePercentage = metricValueChange / this->m_PreviousMetricValue * 100.0;
    }

  this->m_PreviousMetricValue = metricValue;

  if(this->m_StdOutputFlag)
    {
    ParametersType parameters = this->m_Optimizer->GetCurrentPosition();
    std::cout << parameters[0] << " " << parameters[1] << " " << parameters[2] << std::endl;
    std::cout << typeName << " " << this->m_NumberOfIterationsOfCurrentRegistration
      << " " << metricValue << " " << metricValueChange << " "
      << metricValueChangePercentage << "%" << std::endl;
    }

  // stop registration when percentage change of the metric value is less than
  // a threshold.
  if (metricValueChangePercentage > 0)
    {
    return;
    }

  metricValueChangePercentage = -metricValueChangePercentage;
  if (metricValueChangePercentage < this->GetConvergenceThreshold())
    {
    this->m_Optimizer->StopOptimization();
    }
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
void
RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::SetMask( const MaskImageType * mask )
{
  this->m_SpatialObjectMask = SpatialObjectMaskType::New();
  this->m_SpatialObjectMask->SetImage( mask );
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
void
RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::Update()
{
  this->ResetRegistrationConvergenceInformation();

  this->m_Registration->SetFixedImage(  this->m_FixedImage  );
  this->m_Registration->SetMovingImage( this->m_MovingImage );
  this->m_Metric->ReinitializeSeed( 76926294 );

  try
    {
    this->PerformRigidTransform();
    }
  catch( itk::ExceptionObject & excep )
    {
    itkExceptionMacro("Exception caught during initialization of registration filter."
              << excep);
    }
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
void
RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::PerformRigidTransform()
{
  // if initial transform has been manually set, use it
  if (this->GetManualInitialTransformSet())
    {
    this->m_Registration->SetInitialTransformParameters(
      this->m_ManualInitialTransform->GetParameters() );
    if(this->m_StdOutputFlag)
      {
      ParametersType pars = this->m_ManualInitialTransform->GetParameters();
      std::cout << "Start Rigid Registration " << std::endl;
      }
    }
  else
    {
    ParametersType initParameters;
    initParameters.SetSize(VImageDimension);
    initParameters.Fill(0);
    this->m_Registration->SetInitialTransformParameters(initParameters);
    }

  this->m_Registration->SetFixedImageRegion(this->m_FixedImage->GetBufferedRegion());

  this->m_Registration->SetTransform( this->m_RigidTransform );

  //  Define optimizer normaliztion to compensate for different dynamic range
  //  of motions/translations.
  typedef typename OptimizerType::ScalesType       OptimizerScalesType;
  OptimizerScalesType optimizerScales(this->m_RigidTransform->GetNumberOfParameters());
  const double translationScale = 1.0 / 1000.0;
  for (unsigned int i = 0; i < this->m_RigidTransform->GetNumberOfParameters(); i++)
    {
    optimizerScales[i] = translationScale;
    }
  this->m_Optimizer->SetScales( optimizerScales );

  this->m_Optimizer->SetMaximumStepLength( 100.0 * translationScale );
  this->m_Optimizer->SetMinimumStepLength( 0.1 * translationScale );

  // Define maximum number of iterations.
  this->m_Optimizer->SetNumberOfIterations( m_NumberOfIterations );

  // Regulating the number of samples in the Metric is equivalent to performing
  // multi-resolution registration because it sub-samples the image.
  this->m_Metric->SetUseSequentialSampling( false );
  this->m_Metric->SetNumberOfSpatialSamples( 10000L );

  // Define mask
  if( this->m_SpatialObjectMask && this->m_UseMask)
    {
    this->m_Metric->SetFixedImageMask( this->m_SpatialObjectMask );
    }

  if(this->m_StdOutputFlag)
    {
    std::cout << "Start Rigid Registration " << std::endl;
    }
  try
    {
    this->m_Registration->StartRegistration();
    }
  catch( itk::ExceptionObject & err )
    {
    itkExceptionMacro("ExceptionObject caught !" << err);
    }
  if(this->m_StdOutputFlag)
    {
    ParametersType parameters = this->m_RigidTransform->GetParameters();
    std::cout << "Rigid Registration completed at[";
    for (unsigned int i = 0;
         i < this->m_ManualInitialTransform->GetNumberOfParameters(); i++)
      {
      std::cout << parameters[i] << ", ";
      }
    std::cout << "]" << std::endl;
    }

  this->m_RigidTransform->SetParameters(
              this->m_Registration->GetLastTransformParameters() );
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
void
RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::SetManualInitialTransform(const ParametersType & parameters)
{
  this->m_ManualInitialTransform->SetParameters(parameters);
  this->m_ManualInitialTransformSet = true;

  if(this->m_StdOutputFlag)
    {
    std::cout << "Transformation manually initialized at [";
    for (unsigned int i = 0;
         i < this->m_ManualInitialTransform->GetNumberOfParameters(); i++)
      {
      std::cout << parameters[i] << ", ";
      }
    std::cout << "]" << std::endl;
    }
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
const typename RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::ParametersType &
RegisterImageTranslationFilter< ImagePixelType, VImageDimension >
::GetOutputParameters()
{
  return this->m_RigidTransform->GetParameters();
}

} //end namespace itk

#endif
