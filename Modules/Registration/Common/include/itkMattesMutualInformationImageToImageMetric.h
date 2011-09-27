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
#ifndef __itkMattesMutualInformationImageToImageMetric_h
#define __itkMattesMutualInformationImageToImageMetric_h

#include "itkImageToImageMetric.h"
#include "itkPoint.h"
#include "itkIndex.h"
#include "itkBSplineDerivativeKernelFunction.h"
#include "itkArray2D.h"


namespace itk
{
/** \class MattesMutualInformationImageToImageMetric
 * \brief Computes the mutual information between two images to be
 * registered using the method of Mattes et al.
 *
 * MattesMutualInformationImageToImageMetric computes the mutual
 * information between a fixed and moving image to be registered.
 *
 * This class is templated over the FixedImage type and the MovingImage
 * type.
 *
 * The fixed and moving images are set via methods SetFixedImage() and
 * SetMovingImage(). This metric makes use of user specified Transform and
 * Interpolator. The Transform is used to map points from the fixed image to
 * the moving image domain. The Interpolator is used to evaluate the image
 * intensity at user specified geometric points in the moving image.
 * The Transform and Interpolator are set via methods SetTransform() and
 * SetInterpolator().
 *
 * If a BSplineInterpolationFunction is used, this class obtain
 * image derivatives from the BSpline interpolator. Otherwise,
 * image derivatives are computed using central differencing.
 *
 * \warning This metric assumes that the moving image has already been
 * connected to the interpolator outside of this class.
 *
 * The method GetValue() computes of the mutual information
 * while method GetValueAndDerivative() computes
 * both the mutual information and its derivatives with respect to the
 * transform parameters.
 *
 * The calculations are based on the method of Mattes et al [1,2]
 * where the probability density distribution are estimated using
 * Parzen histograms. Since the fixed image PDF does not contribute
 * to the derivatives, it does not need to be smooth. Hence,
 * a zero order (box car) BSpline kernel is used
 * for the fixed image intensity PDF. On the other hand, to ensure
 * smoothness a third order BSpline kernel is used for the
 * moving image intensity PDF.
 *
 * On Initialize(), the FixedImage is uniformly sampled within
 * the FixedImageRegion. The number of samples used can be set
 * via SetNumberOfSpatialSamples(). Typically, the number of
 * spatial samples used should increase with the image size.
 *
 * The option UseAllPixelOn() disables the random sampling and uses
 * all the pixels of the FixedImageRegion in order to estimate the
 * joint intensity PDF.
 *
 * During each call of GetValue(), GetDerivatives(),
 * GetValueAndDerivatives(), marginal and joint intensity PDF's
 * values are estimated at discrete position or bins.
 * The number of bins used can be set via SetNumberOfHistogramBins().
 * To handle data with arbitray magnitude and dynamic range,
 * the image intensity is scale such that any contribution to the
 * histogram will fall into a valid bin.
 *
 * One the PDF's have been contructed, the mutual information
 * is obtained by doubling summing over the discrete PDF values.
 *
 *
 * Notes:
 * 1. This class returns the negative mutual information value.
 * 2. This class in not thread safe due the private data structures
 *     used to the store the sampled points and the marginal and joint pdfs.
 *
 * References:
 * [1] "Nonrigid multimodality image registration"
 *      D. Mattes, D. R. Haynor, H. Vesselle, T. Lewellen and W. Eubank
 *      Medical Imaging 2001: Image Processing, 2001, pp. 1609-1620.
 * [2] "PET-CT Image Registration in the Chest Using Free-form Deformations"
 *      D. Mattes, D. R. Haynor, H. Vesselle, T. Lewellen and W. Eubank
 *      IEEE Transactions in Medical Imaging. Vol.22, No.1,
        January 2003. pp.120-128.
 * [3] "Optimization of Mutual Information for MultiResolution Image
 *      Registration"
 *      P. Thevenaz and M. Unser
 *      IEEE Transactions in Image Processing, 9(12) December 2000.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template< class TFixedImage, class TMovingImage >
class ITK_EXPORT MattesMutualInformationImageToImageMetric:
  public ImageToImageMetric< TFixedImage, TMovingImage >
{
public:

  /** Standard class typedefs. */
  typedef MattesMutualInformationImageToImageMetric       Self;
  typedef ImageToImageMetric< TFixedImage, TMovingImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MattesMutualInformationImageToImageMetric,
               ImageToImageMetric);

  /** Types inherited from Superclass. */
  typedef typename Superclass::TransformType                  TransformType;
  typedef typename Superclass::TransformPointer               TransformPointer;
  typedef typename Superclass::TransformJacobianType          TransformJacobianType;
  typedef typename Superclass::InterpolatorType               InterpolatorType;
  typedef typename Superclass::MeasureType                    MeasureType;
  typedef typename Superclass::DerivativeType                 DerivativeType;
  typedef typename Superclass::ParametersType                 ParametersType;
  typedef typename Superclass::FixedImageType                 FixedImageType;
  typedef typename Superclass::MovingImageType                MovingImageType;
  typedef typename Superclass::MovingImagePointType           MovingImagePointType;
  typedef typename Superclass::FixedImageConstPointer         FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer        MovingImageConstPointer;
  typedef typename Superclass::BSplineTransformWeightsType    BSplineTransformWeightsType;
  typedef typename Superclass::BSplineTransformIndexArrayType BSplineTransformIndexArrayType;

  typedef typename Superclass::CoordinateRepresentationType CoordinateRepresentationType;
  typedef typename Superclass::FixedImageSampleContainer    FixedImageSampleContainer;
  typedef typename Superclass::ImageDerivativesType         ImageDerivativesType;
  typedef typename Superclass::WeightsValueType             WeightsValueType;
  typedef typename Superclass::IndexValueType               IndexValueType;

  typedef typename FixedImageType::OffsetValueType OffsetValueType;

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
  virtual void Initialize(void)
  throw ( ExceptionObject );

  /**  Get the value. */
  MeasureType GetValue(const ParametersType & parameters) const;

  /** Get the derivatives of the match measure. */
  void GetDerivative(const ParametersType & parameters,
                     DerivativeType & Derivative) const;

  /**  Get the value and derivatives for single valued optimizers. */
  void GetValueAndDerivative(const ParametersType & parameters,
                             MeasureType & Value,
                             DerivativeType & Derivative) const;

  /** Number of bins to used in the histogram. Typical value is
   * 50. The minimum value is 5 due to the padding required by the Parzen
   * windowing with a cubic-BSpline kernel. Note that even if the metric
   * is used on binary images, the number of bins should at least be
   * equal to five. */
  itkSetClampMacro( NumberOfHistogramBins, SizeValueType,
                    5, NumericTraits< SizeValueType >::max() );
  itkGetConstReferenceMacro(NumberOfHistogramBins, SizeValueType);

  /** This variable selects the method to be used for computing the Metric
   * derivatives with respect to the Transform parameters. Two modes of
   * computation are available. The choice between one and the other is a
   * trade-off between computation speed and memory allocations. The two modes
   * are described in detail below:
   *
   * UseExplicitPDFDerivatives = True
   * will compute the Metric derivative by first calculating the derivatives of
   * each one of the Joint PDF bins with respect to each one of the Transform
   * parameters and then accumulating these contributions in the final metric
   * derivative array by using a bin-specific weight.  The memory required for
   * storing the intermediate derivatives is a 3D array of doubles with size
   * equals to the product of (number of histogram bins)^2 times number of
   * transform parameters. This method is well suited for Transform with a small
   * number of parameters.
   *
   * UseExplicitPDFDerivatives = False will compute the Metric derivative by
   * first computing the weights for each one of the Joint PDF bins and caching
   * them into an array. Then it will revisit each one of the PDF bins for
   * computing its weighted contribution to the full derivative array. In this
   * method an extra 2D array is used for storing the weights of each one of
   * the PDF bins. This is an array of doubles with size equals to (number of
   * histogram bins)^2. This method is well suited for Transforms with a large
   * number of parameters, such as, BSplineTransforms. */
  itkSetMacro(UseExplicitPDFDerivatives, bool);
  itkGetConstReferenceMacro(UseExplicitPDFDerivatives, bool);
  itkBooleanMacro(UseExplicitPDFDerivatives);

  /** The marginal PDFs are stored as std::vector. */
  typedef float PDFValueType;

  /** Typedef for the joint PDF and PDF derivatives are stored as ITK Images. */
  typedef Image< PDFValueType, 2 >            JointPDFType;
  typedef Image< PDFValueType, 3 >            JointPDFDerivativesType;
  itkGetConstReferenceMacro(JointPDF,typename JointPDFType::Pointer);
  itkGetConstReferenceMacro(JointPDFDerivatives,typename JointPDFDerivativesType::Pointer);
protected:

  MattesMutualInformationImageToImageMetric();
  virtual ~MattesMutualInformationImageToImageMetric();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:

  //purposely not implemented
  MattesMutualInformationImageToImageMetric(const Self &);
  //purposely not implemented
  void operator=(const Self &);

  typedef float *MarginalPDFType;

  mutable MarginalPDFType m_FixedImageMarginalPDF;

  /** The moving image marginal PDF. */
  mutable MarginalPDFType m_MovingImageMarginalPDF;

  /** Helper array for storing the values of the JointPDF ratios. */
  typedef double                PRatioType;
  typedef Array2D< PRatioType > PRatioArrayType;
  mutable PRatioArrayType m_PRatioArray;

  /** Helper variable for accumulating the derivative of the metric. */
  mutable DerivativeType  m_MetricDerivative;
  mutable DerivativeType *m_ThreaderMetricDerivative;

  typedef JointPDFType::IndexType             JointPDFIndexType;
  typedef JointPDFType::PixelType             JointPDFValueType;
  typedef JointPDFType::RegionType            JointPDFRegionType;
  typedef JointPDFType::SizeType              JointPDFSizeType;
  typedef JointPDFDerivativesType::IndexType  JointPDFDerivativesIndexType;
  typedef JointPDFDerivativesType::PixelType  JointPDFDerivativesValueType;
  typedef JointPDFDerivativesType::RegionType JointPDFDerivativesRegionType;
  typedef JointPDFDerivativesType::SizeType   JointPDFDerivativesSizeType;

  /** The joint PDF and PDF derivatives. */
  typename JointPDFType::Pointer m_JointPDF;

  SizeValueType m_JointPDFBufferSize;

  typename JointPDFDerivativesType::Pointer m_JointPDFDerivatives;

  SizeValueType m_JointPDFDerivativesBufferSize;

  /** Variables to define the marginal and joint histograms. */
  SizeValueType m_NumberOfHistogramBins;
  double        m_MovingImageNormalizedMin;
  double        m_FixedImageNormalizedMin;
  double        m_FixedImageTrueMin;
  double        m_FixedImageTrueMax;
  double        m_MovingImageTrueMin;
  double        m_MovingImageTrueMax;
  double        m_FixedImageBinSize;
  double        m_MovingImageBinSize;

  /** Typedefs for BSpline kernel and derivative functions. */
  typedef BSplineKernelFunction< 3 >           CubicBSplineFunctionType;
  typedef BSplineDerivativeKernelFunction< 3 > CubicBSplineDerivativeFunctionType;

  /** Cubic BSpline kernel for computing Parzen histograms. */
  typename CubicBSplineFunctionType::Pointer m_CubicBSplineKernel;
  typename CubicBSplineDerivativeFunctionType::Pointer
  m_CubicBSplineDerivativeKernel;

  /** Precompute fixed image parzen window indices. */
  virtual void ComputeFixedImageParzenWindowIndices(
    FixedImageSampleContainer & samples);

  /** Compute PDF derivative contribution for each parameter. */
  virtual void ComputePDFDerivatives(unsigned int threadID,
                                     unsigned int sampleNumber,
                                     int movingImageParzenWindowIndex,
                                     const ImageDerivativesType
                                     &  movingImageGradientValue,
                                     double cubicBSplineDerivativeValue
                                     ) const;

  PDFValueType *m_ThreaderFixedImageMarginalPDF;

  typename JointPDFType::Pointer              * m_ThreaderJointPDF;
  typename JointPDFDerivativesType::Pointer   * m_ThreaderJointPDFDerivatives;

  int *m_ThreaderJointPDFStartBin;
  int *m_ThreaderJointPDFEndBin;

  mutable double *m_ThreaderJointPDFSum;

  mutable double m_JointPDFSum;

  bool         m_UseExplicitPDFDerivatives;
  mutable bool m_ImplicitDerivativesSecondPass;

  virtual inline void GetValueThreadPreProcess(unsigned int threadID,
                                               bool withinSampleThread) const;

  virtual inline bool GetValueThreadProcessSample(unsigned int threadID,
                                                  SizeValueType fixedImageSample,
                                                  const MovingImagePointType & mappedPoint,
                                                  double movingImageValue) const;

  virtual inline void GetValueThreadPostProcess(unsigned int threadID,
                                                bool withinSampleThread) const;

  virtual inline void GetValueAndDerivativeThreadPreProcess(
    unsigned int threadID,
    bool withinSampleThread) const;

  virtual inline bool GetValueAndDerivativeThreadProcessSample(unsigned int threadID,
                                                               SizeValueType fixedImageSample,
                                                               const MovingImagePointType & mappedPoint,
                                                               double movingImageValue,
                                                               const ImageDerivativesType &
                                                               movingImageGradientValue) const;

  virtual inline void GetValueAndDerivativeThreadPostProcess(
    unsigned int threadID,
    bool withinSampleThread) const;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMattesMutualInformationImageToImageMetric.hxx"
#endif

#endif
