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

#ifndef __itkRegisterImageTranslationFilter_h
#define __itkRegisterImageTranslationFilter_h

#include "itkImage.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImageRegistrationMethod.h"
#include "itkImageMaskSpatialObject.h"
#include "itkCommand.h"
#include "itkTranslationTransform.h"
#include "itkProcessObject.h"

namespace itk
{
/**
 * This class takes input of a fixed image and a moving image and outputs
 * a translation that aligns the two.
 */

template < typename ImagePixelType, unsigned int VImageDimension >
class ITK_EXPORT RegisterImageTranslationFilter  : public ProcessObject
{
public:
  typedef  RegisterImageTranslationFilter  Self;
  typedef  ProcessObject                           Superclass;
  typedef  SmartPointer<Self>                      Pointer;

  itkNewMacro( Self );

  typedef double TransformCoordType;

  typedef typename itk::Image< ImagePixelType, VImageDimension > ImageType;
  typedef typename ImageType::Pointer               ImagePointerType;
  typedef typename ImageType::RegionType            ImageRegionType;

  typedef typename itk::TranslationTransform<TransformCoordType, VImageDimension>
                                                         RigidTransformType;
  typedef typename RigidTransformType::Pointer           RigidTransformPointerType;

  typedef typename itk::RegularStepGradientDescentOptimizer  OptimizerType;
  typedef typename OptimizerType::Pointer         OptimizerPointerType;
  typedef typename OptimizerType::ParametersType  ParametersType;

  typedef typename itk::MeanSquaresImageToImageMetric< ImageType, ImageType >
                                                   MeanSquaresMetricType;
  typedef typename MeanSquaresMetricType::Pointer  MeanSquaresMetricPointerType;

  typedef typename itk::LinearInterpolateImageFunction<
                                    ImageType,
                                    TransformCoordType>    InterpolatorType;
  typedef typename InterpolatorType::Pointer        InterpolatorPointerType;

  typedef typename itk::ImageRegistrationMethod< ImageType, ImageType >
                                                  RegistrationType;
  typedef typename RegistrationType::Pointer      RegistrationPointerType;

  typedef typename itk::ImageMaskSpatialObject< VImageDimension >
                                                         SpatialObjectMaskType;
  typedef typename SpatialObjectMaskType::Pointer SpatialObjectMaskPointerType;

  typedef ImagePixelType                                    MaskImagePixelType;
  typedef typename itk::Image< MaskImagePixelType, VImageDimension >
                                                            MaskImageType;

  typedef typename itk::SimpleMemberCommand<Self>        SimpleCommandType;
  typedef typename SimpleCommandType::Pointer       SimpleCommandPointerType;

  void SetMask( const MaskImageType * mask );

  void SetManualInitialTransform(const ParametersType & parameters);

  virtual void Update();

  const ParametersType & GetOutputParameters();

  itkSetMacro(FixedImage, ImagePointerType);
  itkGetMacro(FixedImage, ImagePointerType);

  itkSetMacro(MovingImage, ImagePointerType);
  itkGetMacro(MovingImage, ImagePointerType);

  itkGetMacro( StdOutputFlag, bool );
  itkSetMacro( StdOutputFlag, bool );

  itkGetMacro( UseMask, bool );
  itkSetMacro( UseMask, bool );

  itkGetMacro( NumberOfIterations, unsigned int );
  itkSetMacro( NumberOfIterations, unsigned int );

  itkGetMacro( ConvergenceThreshold, double );
  itkSetMacro( ConvergenceThreshold, double );

  itkGetMacro( ManualInitialTransformSet, bool );

  RegisterImageTranslationFilter();
  virtual ~RegisterImageTranslationFilter();

protected:
  void ObserveIteration();
  void InitializeTransform();
  void PerformRigidTransform();
  void ResetRegistrationConvergenceInformation();

  ImagePointerType                  m_FixedImage;
  ImagePointerType                  m_MovingImage;
  RigidTransformPointerType         m_RigidTransform;
  MeanSquaresMetricPointerType      m_Metric;
  OptimizerPointerType              m_Optimizer;
  InterpolatorPointerType           m_Interpolator;
  RegistrationPointerType           m_Registration;
  SpatialObjectMaskPointerType      m_SpatialObjectMask;

  bool                          m_StdOutputFlag;
  bool                          m_UseMask;
  unsigned int                  m_NumberOfIterations;
  double                        m_ConvergenceThreshold;

  RigidTransformPointerType     m_ManualInitialTransform;
  bool                          m_ManualInitialTransformSet;

  SimpleCommandPointerType      m_ObserveCommand;

  unsigned int                  m_NumberOfIterationsOfCurrentRegistration;
  double                        m_PreviousMetricValue;

private:
  RegisterImageTranslationFilter(
    const RegisterImageTranslationFilter&);               //Not implemented.
  void operator=(const RegisterImageTranslationFilter&);  //Not implemented.
};

} // end namespace itk

#include "itkRegisterImageTranslationFilter.txx"

#endif
