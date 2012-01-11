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
#ifndef __itkMattesMutualInformationImageToImageMetricv4_h
#define __itkMattesMutualInformationImageToImageMetricv4_h

#include "itkImageToImageMetricv4.h"
#include "itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader.h"
#include "itkPoint.h"
#include "itkIndex.h"
#include "itkBSplineDerivativeKernelFunction.h"
#include "itkArray2D.h"

namespace itk
{

/** \class MattesMutualInformationImageToImageMetricv4
 *
 * \brief Computes the mutual information between two images to be
 * registered using the method of Mattes et al.
 *
 * MattesMutualInformationImageToImageMetric computes the mutual
 * information between a fixed and moving image to be registered.
 *
 * This class is templated over the FixedImage type and the MovingImage
 * type.
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
 * \warning Local-support transforms are not yet supported. If used,
 * an exception is thrown during Initialize().
 *
 * \note The per-iteration post-processing code is not multi-threaded, but could be
 * readily be made so for a small performance gain.
 * See GetValueCommonAfterThreadedExecution(), GetValueAndDerivative()
 * and threader::AfterThreadedExecution().
 *
 * The algorithm and much of the code was copied from the previous
 * Mattes MI metric, i.e. itkMattesMutualInformationImageToImageMetric.
 *
 * See
 *  MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader::ProcessPoint
 *  for poritons of the algorithm implementation.
 *
 * See ImageToImageMetricv4 for details of common metric operation and options.
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
 * \sa itkImageToImageMetricv4
 * \ingroup ITKMetricsv4
 */
template <class TFixedImage, class TMovingImage, class TVirtualImage = TFixedImage >
class ITK_EXPORT MattesMutualInformationImageToImageMetricv4 :
public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
{
public:
  /** Standard class typedefs. */
  typedef MattesMutualInformationImageToImageMetricv4                                     Self;
  typedef ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage> Superclass;
  typedef SmartPointer<Self>                                             Pointer;
  typedef SmartPointer<const Self>                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MattesMutualInformationImageToImageMetricv4, ImageToImageMetricv4);

  /** Superclass types */
  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;

  typedef typename Superclass::FixedImageType          FixedImageType;
  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType  FixedImageGradientType;

  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType MovingImageGradientType;

  typedef typename Superclass::MovingTransformType        MovingTransformType;
  typedef typename Superclass::JacobianType               JacobianType;
  typedef typename Superclass::VirtualImageType           VirtualImageType;
  typedef typename Superclass::VirtualIndexType           VirtualIndexType;
  typedef typename Superclass::VirtualPointType           VirtualPointType;
  typedef typename Superclass::VirtualSampledPointSetType VirtualSampledPointSetType;

  /** Types inherited from Superclass. */
  typedef typename Superclass::FixedSampledPointSetPointer    FixedSampledPointSetPointer;


  /* Image dimension accessors */
  itkStaticConstMacro(VirtualImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<TVirtualImage>::ImageDimension);
  itkStaticConstMacro(FixedImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<TFixedImage>::ImageDimension);
  itkStaticConstMacro(MovingImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<TMovingImage>::ImageDimension);


  /** Number of bins to used in the histogram. Typical value is
   * 50. The minimum value is 5 due to the padding required by the Parzen
   * windowing with a cubic-BSpline kernel. Note that even if the metric
   * is used on binary images, the number of bins should at least be
   * equal to five. */
  itkSetClampMacro( NumberOfHistogramBins, SizeValueType,
                    5, NumericTraits<SizeValueType>::max() );
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
   * storing the intermediate derivatives is a 3D array of floating point values with size
   * equals to the product of (number of histogram bins)^2 times number of
   * transform parameters. This method is well suited for Transform with a small
   * number of parameters.
   *
   * UseExplicitPDFDerivatives = False will compute the Metric derivative by
   * first computing the weights for each one of the Joint PDF bins and caching
   * them into an array. Then it will revisit each one of the PDF bins for
   * computing its weighted contribution to the full derivative array. In this
   * method an extra 2D array is used for storing the weights of each one of
   * the PDF bins. This is an array of floating point values with size equals to (number of
   * histogram bins)^2. This method is well suited for Transforms with a large
   * number of parameters, such as, BSplineTransforms. */

  // FIXME: Implicit method should not be needed once local-support transforms
  // are added, using new v4 framework.
  /*
  itkSetMacro(UseExplicitPDFDerivatives, bool);
  itkGetConstReferenceMacro(UseExplicitPDFDerivatives, bool);
  itkBooleanMacro(UseExplicitPDFDerivatives);
  */

  virtual void Initialize(void) throw ( itk::ExceptionObject );

  /** Calculate and return both the value for the metric and its derivative.
   * Overloaded here to provide special handling.
   */
  virtual void GetValueAndDerivative( MeasureType & value,
                                      DerivativeType & derivative ) const;

  /** The marginal PDFs are stored as std::vector. */
  //NOTE:  floating point precision is not as stable.
  // Double precision proves faster and more robust in real-world testing.
  typedef double PDFValueType;

  /** Typedef for the joint PDF and PDF derivatives are stored as ITK Images. */
  typedef Image<PDFValueType, 2> JointPDFType;
  typedef Image<PDFValueType, 3> JointPDFDerivativesType;

  /**
   * Get the internal JointPDF image that was used in
   * creating the metric value.
   */
  const typename JointPDFType::Pointer GetJointPDF () const
    {
    if( this->m_ThreaderJointPDF.size() == 0 )
      {
      return JointPDFType::Pointer(NULL);
      }
    return this->m_ThreaderJointPDF[0];
    }

  /**
   * Get the internal JointPDFDeriviative image that was used in
   * creating the metric derivative value.
   * This is only created when UseExplicitPDFDerivatives is ON, and
   * derivatives are requested.
   */
  const typename JointPDFDerivativesType::Pointer GetJointPDFDerivatives () const
    {
    if( this->m_ThreaderJointPDFDerivatives.size() == 0 )
      {
      return JointPDFDerivativesType::Pointer(NULL);
      }
    return this->m_ThreaderJointPDFDerivatives[0];
    }


protected:
  MattesMutualInformationImageToImageMetricv4();
  virtual ~MattesMutualInformationImageToImageMetricv4();

  friend class MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >;
  friend class MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >;
  typedef MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >
    MattesMutualInformationDenseGetValueAndDerivativeThreaderType;
  typedef MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >
    MattesMutualInformationSparseGetValueAndDerivativeThreaderType;

  void PrintSelf(std::ostream& os, Indent indent) const;


  typedef JointPDFType::IndexType             JointPDFIndexType;
  typedef JointPDFType::PixelType             JointPDFValueType;
  typedef JointPDFType::RegionType            JointPDFRegionType;
  typedef JointPDFType::SizeType              JointPDFSizeType;
  typedef JointPDFDerivativesType::IndexType  JointPDFDerivativesIndexType;
  typedef JointPDFDerivativesType::PixelType  JointPDFDerivativesValueType;
  typedef JointPDFDerivativesType::RegionType JointPDFDerivativesRegionType;
  typedef JointPDFDerivativesType::SizeType   JointPDFDerivativesSizeType;

  /** Typedefs for BSpline kernel and derivative functions. */
  typedef BSplineKernelFunction<3,PDFValueType>           CubicBSplineFunctionType;
  typedef BSplineDerivativeKernelFunction<3,PDFValueType> CubicBSplineDerivativeFunctionType;

  /** Post-processing code common to both GetValue
   * and GetValueAndDerivative. */
  virtual void GetValueCommonAfterThreadedExecution();

  OffsetValueType ComputeSingleFixedImageParzenWindowIndex( const FixedImagePixelType & value ) const;

  /** Variables to define the marginal and joint histograms. */
  SizeValueType m_NumberOfHistogramBins;
  PDFValueType  m_MovingImageNormalizedMin;
  PDFValueType  m_FixedImageNormalizedMin;
  PDFValueType  m_FixedImageTrueMin;
  PDFValueType  m_FixedImageTrueMax;
  PDFValueType  m_MovingImageTrueMin;
  PDFValueType  m_MovingImageTrueMax;
  PDFValueType  m_FixedImageBinSize;
  PDFValueType  m_MovingImageBinSize;

  /** Cubic BSpline kernel for computing Parzen histograms. */
  typename CubicBSplineFunctionType::Pointer           m_CubicBSplineKernel;
  typename CubicBSplineDerivativeFunctionType::Pointer m_CubicBSplineDerivativeKernel;

  /** Helper array for storing the values of the JointPDF ratios. */
  typedef PDFValueType        PRatioType;
  typedef Array2D<PRatioType> PRatioArrayType;

  mutable PRatioArrayType m_PRatioArray;

  /** Helper variable for accumulating the derivative of the metric. */
  mutable std::vector<DerivativeType> m_ThreaderMetricDerivative;

  /** The moving image marginal PDF. */
  mutable std::vector<PDFValueType>               m_MovingImageMarginalPDF;
  mutable std::vector<std::vector<PDFValueType> > m_ThreaderFixedImageMarginalPDF;

  /** The joint PDF and PDF derivatives. */
  typename std::vector<JointPDFType::Pointer>            m_ThreaderJointPDF;
  typename std::vector<JointPDFDerivativesType::Pointer> m_ThreaderJointPDFDerivatives;

  std::vector<int> m_ThreaderJointPDFStartBin;
  std::vector<int> m_ThreaderJointPDFEndBin;

  mutable std::vector<PDFValueType> m_ThreaderJointPDFSum;

  // FIXME: Implicit method should not be needed once local-support transforms
  // are added, using new v4 framework.
  bool         m_UseExplicitPDFDerivatives;
  mutable bool m_ImplicitDerivativesSecondPass;

private:
  MattesMutualInformationImageToImageMetricv4(const Self &); //purposely not implemented
  void operator = (const Self &); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMattesMutualInformationImageToImageMetricv4.hxx"
#endif

#endif
