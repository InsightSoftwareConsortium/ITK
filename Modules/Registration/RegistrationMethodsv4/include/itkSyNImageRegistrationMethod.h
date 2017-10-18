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
#ifndef itkSyNImageRegistrationMethod_h
#define itkSyNImageRegistrationMethod_h

#include "itkImageRegistrationMethodv4.h"

#include "itkImageMaskSpatialObject.h"
#include "itkDisplacementFieldTransform.h"

namespace itk
{

/** \class SyNImageRegistrationMethod
 * \brief Interface method for the performing greedy SyN image registration.
 *
 * For greedy SyN we use \c m_Transform to map the time-parameterized middle
 * image to the fixed image (and vice versa using
 * \c m_Transform->GetInverseDisplacementField() ).  We employ another ivar,
 * \c m_InverseTransform, to map the time-parameterized middle image to the
 * moving image.
 *
 * Output: The output is the updated transform which has been added to the
 * composite transform.
 *
 * This implementation is based on the source code in Advanced Normalization Tools (ANTs)
 *
 *   Avants, B. B.; Tustison, N. J.; Song, G.; Cook, P. A.; Klein, A. & Gee, J. C.
 *   A reproducible evaluation of ANTs similarity metric performance in brain image registration.
 *   Neuroimage, Penn Image Computing and Science Laboratory, University of Pennsylvania,
 *   2011, 54, 2033-2044
 *
 * The original paper discussing the method is here:
 *
 *  Avants, B. B.; Epstein, C. L.; Grossman, M. & Gee, J. C.
 *  Symmetric diffeomorphic image registration with cross-correlation:
 *  evaluating automated labeling of elderly and neurodegenerative brain.
 *  Med Image Anal, Department of Radiology, University of Pennsylvania,
 *  2008, 12, 26-41
 *
 * The method evolved since that time with crucial contributions from Gang Song and
 * Nick Tustison. Though similar in spirit, this implementation is not identical.
 *
 * \todo Need to allow the fixed image to have a composite transform.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKRegistrationMethodsv4
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform =
  DisplacementFieldTransform<double, TFixedImage::ImageDimension>,
  typename TVirtualImage = TFixedImage,
  typename TPointSet = PointSet<unsigned int, TFixedImage::ImageDimension> >
class ITK_TEMPLATE_EXPORT SyNImageRegistrationMethod
: public ImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
{
public:
  /** Standard class typedefs. */
  typedef SyNImageRegistrationMethod                                                  Self;
  typedef ImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform,
                                                       TVirtualImage, TPointSet>      Superclass;
  typedef SmartPointer<Self>                                                          Pointer;
  typedef SmartPointer<const Self>                                                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** ImageDimension constants */
  itkStaticConstMacro( ImageDimension, unsigned int, TFixedImage::ImageDimension );

  /** Run-time type information (and related methods). */
  itkTypeMacro( SyNImageRegistrationMethod, SimpleImageRegistrationMethod );

  /** Input typedefs for the images. */
  typedef TFixedImage                                                 FixedImageType;
  typedef typename FixedImageType::Pointer                            FixedImagePointer;
  typedef typename Superclass::FixedImagesContainerType               FixedImagesContainerType;
  typedef TMovingImage                                                MovingImageType;
  typedef typename MovingImageType::Pointer                           MovingImagePointer;
  typedef typename Superclass::MovingImagesContainerType              MovingImagesContainerType;

  typedef typename Superclass::PointSetType                           PointSetType;
  typedef typename PointSetType::Pointer                              PointSetPointer;
  typedef typename Superclass::PointSetsContainerType                 PointSetsContainerType;

  /** Metric and transform typedefs */
  typedef typename Superclass::ImageMetricType                        ImageMetricType;
  typedef typename ImageMetricType::Pointer                           ImageMetricPointer;
  typedef typename ImageMetricType::MeasureType                       MeasureType;

  typedef ImageMaskSpatialObject<ImageDimension>                      ImageMaskSpatialObjectType;
  typedef typename Superclass::FixedImageMaskType                     FixedImageMaskType;
  typedef typename ImageMaskSpatialObjectType::ImageType              FixedMaskImageType;
  typedef typename Superclass::FixedImageMasksContainerType           FixedImageMasksContainerType;
  typedef typename Superclass::MovingImageMaskType                    MovingImageMaskType;
  typedef typename ImageMaskSpatialObjectType::ImageType              MovingMaskImageType;
  typedef typename Superclass::MovingImageMasksContainerType          MovingImageMasksContainerType;

  typedef typename Superclass::VirtualImageType                       VirtualImageType;
  typedef typename Superclass::VirtualImageBaseType                   VirtualImageBaseType;
  typedef typename Superclass::VirtualImageBaseConstPointer           VirtualImageBaseConstPointer;

  typedef typename Superclass::MultiMetricType                        MultiMetricType;
  typedef typename Superclass::MetricType                             MetricType;
  typedef typename MetricType::Pointer                                MetricPointer;
  typedef typename Superclass::PointSetMetricType                     PointSetMetricType;

  typedef typename Superclass::InitialTransformType                   InitialTransformType;
  typedef TOutputTransform                                            OutputTransformType;
  typedef typename OutputTransformType::Pointer                       OutputTransformPointer;
  typedef typename OutputTransformType::ScalarType                    RealType;
  typedef typename OutputTransformType::DerivativeType                DerivativeType;
  typedef typename DerivativeType::ValueType                          DerivativeValueType;
  typedef typename OutputTransformType::DisplacementFieldType         DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer                     DisplacementFieldPointer;
  typedef typename DisplacementFieldType::PixelType                   DisplacementVectorType;

  typedef typename Superclass::CompositeTransformType                 CompositeTransformType;
  typedef typename CompositeTransformType::TransformType              TransformBaseType;

  typedef typename Superclass::DecoratedOutputTransformType           DecoratedOutputTransformType;
  typedef typename DecoratedOutputTransformType::Pointer              DecoratedOutputTransformPointer;

  typedef DisplacementFieldTransform<RealType, ImageDimension>        DisplacementFieldTransformType;
  typedef typename DisplacementFieldTransformType::Pointer            DisplacementFieldTransformPointer;

  typedef Array<SizeValueType>                                        NumberOfIterationsArrayType;

  /** Set/Get the learning rate. */
  itkSetMacro( LearningRate, RealType );
  itkGetConstMacro( LearningRate, RealType );

  /** Set/Get the number of iterations per level. */
  itkSetMacro( NumberOfIterationsPerLevel, NumberOfIterationsArrayType );
  itkGetConstMacro( NumberOfIterationsPerLevel, NumberOfIterationsArrayType );

  /** Set/Get the convergence threshold */
  itkSetMacro( ConvergenceThreshold, RealType );
  itkGetConstMacro( ConvergenceThreshold, RealType );

  /** Set/Get the convergence window size */
  itkSetMacro( ConvergenceWindowSize, unsigned int );
  itkGetConstMacro( ConvergenceWindowSize, unsigned int );

  /** Let the user control whether we compute metric derivatives in the downsampled or full-res space.
   *  The default is 'true' --- classic SyN --- but there may be advantages to the other approach.
   *  Classic SyN did not have this possibility. This implementation will let us explore the question.
   */
  itkSetMacro( DownsampleImagesForMetricDerivatives, bool );
  itkGetConstMacro( DownsampleImagesForMetricDerivatives, bool );

  /** Allow the user to average the gradients in the mid-point domain. Default false.
   *  One might choose to do this to further reduce bias.
   */
  itkSetMacro( AverageMidPointGradients, bool );
  itkGetConstMacro( AverageMidPointGradients, bool );

  /**
   * Get/Set the Gaussian smoothing variance for the update field.
   * Default = 1.75.
   */
  itkSetMacro( GaussianSmoothingVarianceForTheUpdateField, RealType );
  itkGetConstReferenceMacro( GaussianSmoothingVarianceForTheUpdateField, RealType );

  /**
   * Get/Set the Gaussian smoothing variance for the total field.
   * Default = 0.5.
   */
  itkSetMacro( GaussianSmoothingVarianceForTheTotalField, RealType );
  itkGetConstReferenceMacro( GaussianSmoothingVarianceForTheTotalField, RealType );

  /** Get modifiable FixedToMiddle and MovingToMidle transforms to save the current state of the registration. */
  itkGetModifiableObjectMacro( FixedToMiddleTransform, OutputTransformType );
  itkGetModifiableObjectMacro( MovingToMiddleTransform, OutputTransformType );

  /** Set FixedToMiddle and MovingToMidle transforms to restore the registration from a saved state. */
  itkSetObjectMacro( FixedToMiddleTransform, OutputTransformType);
  itkSetObjectMacro( MovingToMiddleTransform, OutputTransformType);

protected:
  SyNImageRegistrationMethod();
  virtual ~SyNImageRegistrationMethod() ITK_OVERRIDE;
  virtual void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  /** Perform the registration. */
  virtual void  GenerateData() ITK_OVERRIDE;

  /** Handle optimization internally */
  virtual void StartOptimization();

  /**
   * Initialize by setting the interconnects between the components. Need to override
   * in the SyN class since we need to "adapt" the \c m_InverseTransform
   */
  virtual void InitializeRegistrationAtEachLevel( const SizeValueType ) ITK_OVERRIDE;

  virtual DisplacementFieldPointer ComputeUpdateField( const FixedImagesContainerType, const PointSetsContainerType,
    const TransformBaseType *, const MovingImagesContainerType, const PointSetsContainerType,
    const TransformBaseType *, const FixedImageMasksContainerType, const MovingImageMasksContainerType, MeasureType & );
  virtual DisplacementFieldPointer ComputeMetricGradientField( const FixedImagesContainerType,
    const PointSetsContainerType, const TransformBaseType *, const MovingImagesContainerType,
    const PointSetsContainerType, const TransformBaseType *, const FixedImageMasksContainerType,
    const MovingImageMasksContainerType, MeasureType & );

  virtual DisplacementFieldPointer ScaleUpdateField( const DisplacementFieldType * );
  virtual DisplacementFieldPointer GaussianSmoothDisplacementField( const DisplacementFieldType *, const RealType );
  virtual DisplacementFieldPointer InvertDisplacementField( const DisplacementFieldType *, const DisplacementFieldType * = ITK_NULLPTR );

  RealType                                                        m_LearningRate;

  OutputTransformPointer                                          m_MovingToMiddleTransform;
  OutputTransformPointer                                          m_FixedToMiddleTransform;

  RealType                                                        m_ConvergenceThreshold;
  unsigned int                                                    m_ConvergenceWindowSize;

  NumberOfIterationsArrayType                                     m_NumberOfIterationsPerLevel;
  bool                                                            m_DownsampleImagesForMetricDerivatives;
  bool                                                            m_AverageMidPointGradients;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SyNImageRegistrationMethod);

  RealType                                                        m_GaussianSmoothingVarianceForTheUpdateField;
  RealType                                                        m_GaussianSmoothingVarianceForTheTotalField;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSyNImageRegistrationMethod.hxx"
#endif

#endif
