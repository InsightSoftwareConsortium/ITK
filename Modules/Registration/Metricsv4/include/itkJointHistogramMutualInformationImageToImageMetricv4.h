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

#ifndef itkJointHistogramMutualInformationImageToImageMetricv4_h
#define itkJointHistogramMutualInformationImageToImageMetricv4_h

#include "itkImageToImageMetricv4.h"
#include "itkImage.h"
#include "itkBSplineDerivativeKernelFunction.h"

#include "itkJointHistogramMutualInformationComputeJointPDFThreader.h"
#include "itkJointHistogramMutualInformationGetValueAndDerivativeThreader.h"

namespace itk
{
/** \class JointHistogramMutualInformationImageToImageMetricv4
 * \brief Computes the mutual information between two images to be
 * registered using the method referenced below.
 *
 * References:
 * [1] "Optimization of Mutual Information for MultiResolution Image
 *      Registration"
 *      P. Thevenaz and M. Unser
 *      IEEE Transactions in Image Processing, 9(12) December 2000.
 *
 * \ingroup ITKMetricsv4
 */
template<typename TFixedImage,typename TMovingImage,typename TVirtualImage = TFixedImage,
          typename TInternalComputationValueType = double,
          typename TMetricTraits = DefaultImageToImageMetricTraitsv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType>
          >
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationImageToImageMetricv4 :
  public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
{
public:

  /** Standard class typedefs. */
  typedef JointHistogramMutualInformationImageToImageMetricv4              Self;
  typedef ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage,
                             TInternalComputationValueType,TMetricTraits>  Superclass;
  typedef SmartPointer<Self>                                               Pointer;
  typedef SmartPointer<const Self>                                         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JointHistogramMutualInformationImageToImageMetricv4, ImageToImageMetricv4);

  /** Type used for representing parameter values  */
  typedef typename Superclass::CoordinateRepresentationType
                                                  CoordinateRepresentationType;
  /** Type used internally for computations */
  /** It should be possible to derive the internal computation type from the class object. */
  typedef TInternalComputationValueType               InternalComputationValueType;
  /**  Type of the parameters. */
  typedef typename Superclass::ParametersType         ParametersType;
  typedef typename Superclass::ParametersValueType    ParametersValueType;
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /** Superclass typedefs */
  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::FixedImagePointType      FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType      FixedImagePixelType;
  typedef typename Superclass::FixedGradientPixelType   FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType     MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType     MovingImagePixelType;
  typedef typename Superclass::MovingGradientPixelType  MovingImageGradientType;

  typedef typename Superclass::FixedTransformType::JacobianType  FixedTransformJacobianType;
  typedef typename Superclass::MovingTransformType::JacobianType MovingTransformJacobianType;

  typedef typename Superclass::VirtualImageType         VirtualImageType;
  typedef typename Superclass::VirtualIndexType         VirtualIndexType;
  typedef typename Superclass::VirtualPointType         VirtualPointType;
  typedef typename Superclass::VirtualPointSetType      VirtualPointSetType;

  /* Image dimension accessors */
  itkStaticConstMacro(VirtualImageDimension, typename TVirtualImage::ImageDimensionType, TVirtualImage::ImageDimension);
  itkStaticConstMacro(MovingImageDimension,  typename TMovingImage::ImageDimensionType,  TMovingImage::ImageDimension);

  /** Value type of the PDF */
  typedef TInternalComputationValueType                  PDFValueType;

  /** Typedef for the joint PDF and marginal PDF are stored as ITK Images. */
  typedef Image<PDFValueType,1>                 MarginalPDFType;
  typedef typename MarginalPDFType::IndexType   MarginalPDFIndexType;
  typedef typename MarginalPDFType::PointType   MarginalPDFPointType;
  typedef Image< PDFValueType, 2>               JointPDFType;
  typedef typename JointPDFType::IndexType      JointPDFIndexType;
  typedef typename JointPDFType::PointType      JointPDFPointType;
  typedef typename JointPDFType::IndexValueType JointPDFIndexValueType;

  /** Get the JointPDF.  Valid after GetValueAndDerivative has been called. */
  itkGetModifiableObjectMacro(JointPDF, JointPDFType );

  // Declare the type for the derivative calculation
  typedef itk::GradientRecursiveGaussianImageFilter< JointPDFType > JPDFGradientFilterType;
  typedef typename JPDFGradientFilterType::OutputImageType          JPDFGradientImageType;
  typedef typename JPDFGradientImageType::Pointer                   JPDFGradientImagePointer;

  typedef itk::GradientRecursiveGaussianImageFilter< MarginalPDFType >  MarginalGradientFilterType;
  typedef typename MarginalGradientFilterType::OutputImageType          MarginalGradientImageType;
  typedef typename MarginalGradientImageType::Pointer                   MarginalGradientImagePointer;

  /** pdf interpolator */
  typedef LinearInterpolateImageFunction<JointPDFType,double>     JointPDFInterpolatorType;
  typedef typename JointPDFInterpolatorType::Pointer              JointPDFInterpolatorPointer;
  typedef LinearInterpolateImageFunction<MarginalPDFType,double>  MarginalPDFInterpolatorType;
  typedef typename MarginalPDFInterpolatorType::Pointer           MarginalPDFInterpolatorPointer;

  /** Joint PDF types */
  typedef typename JointPDFType::PixelType             JointPDFValueType;
  typedef typename JointPDFType::RegionType            JointPDFRegionType;
  typedef typename JointPDFType::SizeType              JointPDFSizeType;
  typedef typename JointPDFType::SpacingType           JointPDFSpacingType;


  /** Get/Set the number of histogram bins */
  itkSetClampMacro( NumberOfHistogramBins, SizeValueType, 5, NumericTraits< SizeValueType >::max() );
  itkGetConstReferenceMacro(NumberOfHistogramBins, SizeValueType );

  /** Get/Set option to smooth the joint pdf after it's updated */
  itkSetMacro(VarianceForJointPDFSmoothing, TInternalComputationValueType);
  itkGetMacro(VarianceForJointPDFSmoothing, TInternalComputationValueType);

  /** Initialize the metric. Make sure all essential inputs are plugged in. */
  virtual void Initialize() ITK_OVERRIDE;

  virtual MeasureType GetValue() const ITK_OVERRIDE;

protected:
  JointHistogramMutualInformationImageToImageMetricv4();
  virtual ~JointHistogramMutualInformationImageToImageMetricv4() ITK_OVERRIDE;

  /** Update the histograms for use in GetValueAndDerivative
   *  Results are returned in \c value and \c derivative.
   */
  virtual void InitializeForIteration() const ITK_OVERRIDE;

  /** Compute the metric value. For internal use. */
  MeasureType ComputeValue() const;

  /** Compute the point location with the JointPDF image.  Returns false if the
   * point is not inside the image. */
  inline void ComputeJointPDFPoint( const FixedImagePixelType fixedImageValue, const MovingImagePixelType movingImageValue, JointPDFPointType & jointPDFpoint ) const;

  friend class JointHistogramMutualInformationComputeJointPDFThreaderBase< ThreadedImageRegionPartitioner< Self::VirtualImageDimension >, Self >;
  friend class JointHistogramMutualInformationComputeJointPDFThreaderBase< ThreadedIndexedContainerPartitioner, Self >;
  friend class JointHistogramMutualInformationComputeJointPDFThreader< ThreadedImageRegionPartitioner< Self::VirtualImageDimension >, Self >;
  friend class JointHistogramMutualInformationComputeJointPDFThreader< ThreadedIndexedContainerPartitioner, Self >;

  typedef JointHistogramMutualInformationComputeJointPDFThreader< ThreadedImageRegionPartitioner< Self::VirtualImageDimension >, Self >
    JointHistogramMutualInformationDenseComputeJointPDFThreaderType;
  typedef JointHistogramMutualInformationComputeJointPDFThreader< ThreadedIndexedContainerPartitioner, Self >
    JointHistogramMutualInformationSparseComputeJointPDFThreaderType;

  typename JointHistogramMutualInformationDenseComputeJointPDFThreaderType::Pointer  m_JointHistogramMutualInformationDenseComputeJointPDFThreader;
  typename JointHistogramMutualInformationSparseComputeJointPDFThreaderType::Pointer m_JointHistogramMutualInformationSparseComputeJointPDFThreader;

  friend class JointHistogramMutualInformationGetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >;
  friend class JointHistogramMutualInformationGetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >;

  typedef JointHistogramMutualInformationGetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >
    JointHistogramMutualInformationDenseGetValueAndDerivativeThreaderType;
  typedef JointHistogramMutualInformationGetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >
    JointHistogramMutualInformationSparseGetValueAndDerivativeThreaderType;

  /** Standard PrintSelf method. */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Count of the number of valid histogram points. */
  SizeValueType   m_JointHistogramTotalCount;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(JointHistogramMutualInformationImageToImageMetricv4);

  /** The fixed image marginal PDF */
  typename MarginalPDFType::Pointer m_FixedImageMarginalPDF;

  /** The moving image marginal PDF. */
  typename MarginalPDFType::Pointer m_MovingImageMarginalPDF;

  /** The joint PDF and PDF derivatives. */
  mutable typename JointPDFType::Pointer            m_JointPDF;

  /** Flag to control smoothing of joint pdf */
  TInternalComputationValueType        m_VarianceForJointPDFSmoothing;

  /** Variables to define the marginal and joint histograms. */
  SizeValueType                        m_NumberOfHistogramBins;
  TInternalComputationValueType        m_FixedImageTrueMin;
  TInternalComputationValueType        m_FixedImageTrueMax;
  TInternalComputationValueType        m_MovingImageTrueMin;
  TInternalComputationValueType        m_MovingImageTrueMax;
  TInternalComputationValueType        m_FixedImageBinSize;
  TInternalComputationValueType        m_MovingImageBinSize;

  TInternalComputationValueType        m_JointPDFSum;
  JointPDFSpacingType                  m_JointPDFSpacing;

  TInternalComputationValueType        m_Log2;
  JointPDFIndexValueType               m_Padding;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkJointHistogramMutualInformationImageToImageMetricv4.hxx"
#endif

#endif
