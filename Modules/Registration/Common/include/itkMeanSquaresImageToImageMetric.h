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
#ifndef itkMeanSquaresImageToImageMetric_h
#define itkMeanSquaresImageToImageMetric_h

#include "itkImageToImageMetric.h"
#include "itkPoint.h"
#include "itkIndex.h"


namespace itk
{

/**
 * \class MeanSquaresImageToImageMetric
 * \brief TODO
 * \ingroup ITKRegistrationCommon
 *
 * \wiki
 * \wikiexample{Metrics/MeanSquaresImageToImageMetric,Compute the mean squares metric between two images}
 * \endwiki
 */
template< typename TFixedImage, typename TMovingImage >
class ITK_TEMPLATE_EXPORT MeanSquaresImageToImageMetric:
  public ImageToImageMetric< TFixedImage, TMovingImage >
{
public:

  /** Standard class typedefs. */
  typedef MeanSquaresImageToImageMetric                   Self;
  typedef ImageToImageMetric< TFixedImage, TMovingImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanSquaresImageToImageMetric, ImageToImageMetric);

  /** Types inherited from Superclass. */
  typedef typename Superclass::TransformType                TransformType;
  typedef typename Superclass::TransformPointer             TransformPointer;
  typedef typename Superclass::TransformJacobianType        TransformJacobianType;
  typedef typename Superclass::InterpolatorType             InterpolatorType;
  typedef typename Superclass::MeasureType                  MeasureType;
  typedef typename Superclass::DerivativeType               DerivativeType;
  typedef typename Superclass::ParametersType               ParametersType;
  typedef typename Superclass::FixedImageType               FixedImageType;
  typedef typename Superclass::MovingImageType              MovingImageType;
  typedef typename Superclass::MovingImagePointType         MovingImagePointType;
  typedef typename Superclass::FixedImageConstPointer       FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer      MovingImageConstPointer;
  typedef typename Superclass::CoordinateRepresentationType CoordinateRepresentationType;
  typedef typename Superclass::FixedImageSampleContainer    FixedImageSampleContainer;
  typedef typename Superclass::ImageDerivativesType         ImageDerivativesType;
  typedef typename Superclass::WeightsValueType             WeightsValueType;
  typedef typename Superclass::IndexValueType               IndexValueType;

  // Needed for evaluation of Jacobian.
  typedef typename Superclass::FixedImagePointType FixedImagePointType;

  /** The moving image dimension. */
  itkStaticConstMacro(MovingImageDimension, unsigned int,
                      MovingImageType::ImageDimension);

  /**
   *  Initialize the Metric by
   *  (1) making sure that all the components are present and plugged
   *      together correctly,
   *  (2) uniformly select NumberOfSpatialSamples within
   *      the FixedImageRegion, and
   *  (3) allocate memory for pdf data structures. */
  virtual void Initialize(void) ITK_OVERRIDE;

  /**  Get the value. */
  MeasureType GetValue(const ParametersType & parameters) const ITK_OVERRIDE;

  /** Get the derivatives of the match measure. */
  void GetDerivative(const ParametersType & parameters,
                     DerivativeType & Derivative) const ITK_OVERRIDE;

  /**  Get the value and derivatives for single valued optimizers. */
  void GetValueAndDerivative(const ParametersType & parameters,
                             MeasureType & Value,
                             DerivativeType & Derivative) const ITK_OVERRIDE;

protected:

  MeanSquaresImageToImageMetric();
  virtual ~MeanSquaresImageToImageMetric() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(MeanSquaresImageToImageMetric);

  bool GetValueThreadProcessSample(ThreadIdType threadId,
                                   SizeValueType fixedImageSample,
                                   const MovingImagePointType & mappedPoint,
                                   double movingImageValue) const ITK_OVERRIDE;

  bool GetValueAndDerivativeThreadProcessSample(ThreadIdType threadId,
                                                SizeValueType fixedImageSample,
                                                const MovingImagePointType & mappedPoint,
                                                double movingImageValue,
                                                const ImageDerivativesType &
                                                movingImageGradientValue) const ITK_OVERRIDE;

  struct PerThreadS
  {
    TransformJacobianType m_Jacobian;
    MeasureType           m_MSE;
    DerivativeType        m_MSEDerivative;
  };

  itkAlignedTypedef( 64, PerThreadS, AlignedPerThreadType );
  AlignedPerThreadType *m_PerThread;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanSquaresImageToImageMetric.hxx"
#endif

#endif
