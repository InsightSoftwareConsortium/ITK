/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMattesMutualInformationImageToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMattesMutualInformationImageToImageMetric_h
#define __itkMattesMutualInformationImageToImageMetric_h

#include "itkImageToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"
#include "itkIndex.h"
#include "itkBSplineKernelFunction.h"
#include "itkBSplineDerivativeKernelFunction.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkBSplineDeformableTransform.h"


namespace itk
{

/** \class MattesMutualInformationImageToImageMetric
 * \brief Computes the mutual information between two images to be 
 * registered using the methof of Mattes et al.
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
 *      IEEE Transactions in Medical Imaging. To Appear.
 * [3] "Optimization of Mutual Information for MultiResolution Image
 *      Registration"
 *      P. Thevenaz and M. Unser
 *      IEEE Transactions in Image Processing, 9(12) December 2000.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ThreadUnSafe
 */
template <class TFixedImage,class TMovingImage >
class ITK_EXPORT MattesMutualInformationImageToImageMetric :
  public ImageToImageMetric< TFixedImage, TMovingImage >
{
public:

  /** Standard class typedefs. */
  typedef MattesMutualInformationImageToImageMetric  Self;
  typedef ImageToImageMetric< TFixedImage, TMovingImage > Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MattesMutualInformationImageToImageMetric, ImageToImageMetric);

  /** Types inherited from Superclass. */
  typedef typename Superclass::TransformType            TransformType;
  typedef typename Superclass::TransformPointer         TransformPointer;
  typedef typename Superclass::TransformJacobianType    TransformJacobianType;
  typedef typename Superclass::InterpolatorType         InterpolatorType;
  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::FixedImageType           FixedImageType;
  typedef typename Superclass::MovingImageType          MovingImageType;
  typedef typename Superclass::FixedImageConstPointer   FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer  MovingImageCosntPointer;

  /** Index and Point typedef support. */
  typedef typename FixedImageType::IndexType            FixedImageIndexType;
  typedef typename FixedImageIndexType::IndexValueType  FixedImageIndexValueType;
  typedef typename MovingImageType::IndexType           MovingImageIndexType;
  typedef typename TransformType::InputPointType        FixedImagePointType;
  typedef typename TransformType::OutputPointType       MovingImagePointType;

  /** The moving image dimension. */
  itkStaticConstMacro( MovingImageDimension, unsigned int,
      MovingImageType::ImageDimension );

  /** 
   *  Initialize the Metric by 
   *  (1) making sure that all the components are present and plugged 
   *      together correctly,
   *  (2) uniformly select NumberOfSpatialSamples within
   *      the FixedImageRegion, and 
   *  (3) allocate memory for pdf data structures. */
  void Initialize(void) throw ( ExceptionObject );

  /** Get the derivatives of the match measure. */
  void GetDerivative( 
    const ParametersType& parameters,
    DerivativeType & Derivative ) const;

  /**  Get the value. */
  MeasureType GetValue( const ParametersType& parameters ) const;

  /**  Get the value and derivatives for single valued optimizers. */
  void GetValueAndDerivative( const ParametersType& parameters, 
    MeasureType& Value, DerivativeType& Derivative ) const;

  /** Number of spatial samples to used to compute metric */
  itkSetClampMacro( NumberOfSpatialSamples, unsigned long,
    1, NumericTraits<unsigned long>::max() );
  itkGetMacro( NumberOfSpatialSamples, unsigned long); 

  /** Number of bins to used in the histogram. Typical value is 50. */
  itkSetClampMacro( NumberOfHistogramBins, unsigned long,
    1, NumericTraits<unsigned long>::max() );
  itkGetMacro( NumberOfHistogramBins, unsigned long);   
  
protected:

  MattesMutualInformationImageToImageMetric();
  virtual ~MattesMutualInformationImageToImageMetric() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:

  MattesMutualInformationImageToImageMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /**
   * A fixed image spatial sample consists of the fixed domain point 
   * and the fixed image value at that point. */
  class FixedImageSpatialSample
    {
    public:
      FixedImageSpatialSample():FixedImageValue(0.0)
      { FixedImagePointValue.Fill(0.0); }
      ~FixedImageSpatialSample() {};

      FixedImagePointType            FixedImagePointValue;
      double                        FixedImageValue;
    };

  /** FixedImageSpatialSample typedef support. */
  typedef std::vector<FixedImageSpatialSample>  
    FixedImageSpatialSampleContainer;

  /** Container to store a set of points and fixed image values. */
  mutable FixedImageSpatialSampleContainer    m_FixedImageSamples;

  /** Uniformly select a sample set from the fixed image domain. */
  void SampleFixedImageDomain( 
    FixedImageSpatialSampleContainer& samples) const;

  /** The marginal PDFs are stored as std::vector. */
  typedef float PDFValueType;
  typedef std::vector<PDFValueType> MarginalPDFType;

  /** The fixed image marginal PDF. */
  mutable MarginalPDFType m_FixedImageMarginalPDF;

  /** The moving image marginal PDF. */
  mutable MarginalPDFType m_MovingImageMarginalPDF;

  /** Typedef for the joint PDF and PDF derivatives are stored as ITK Images. */
  typedef Image<PDFValueType,2> JointPDFType;
  typedef Image<PDFValueType,3> JointPDFDerivativesType;
  typedef JointPDFType::IndexType                JointPDFIndexType;
  typedef JointPDFType::PixelType                JointPDFValueType;
  typedef JointPDFType::RegionType              JointPDFRegionType;
  typedef JointPDFType::SizeType                JointPDFSizeType;
  typedef JointPDFDerivativesType::IndexType    JointPDFDerivativesIndexType;
  typedef JointPDFDerivativesType::PixelType    JointPDFDerivativesValueType;
  typedef JointPDFDerivativesType::RegionType    JointPDFDerivativesRegionType;
  typedef JointPDFDerivativesType::SizeType      JointPDFDerivativesSizeType;

  /** The joint PDF and PDF derivatives. */
  typename JointPDFType::Pointer m_JointPDF;
  typename JointPDFDerivativesType::Pointer m_JointPDFDerivatives;

  unsigned long m_NumberOfSpatialSamples;
  unsigned long m_NumberOfParameters;

  /** Variables to define the marginal and joint histograms. */
  unsigned long m_NumberOfHistogramBins;
  double m_FixedImageMin;
  double m_FixedImageMax;
  double m_MovingImageMin;
  double m_MovingImageMax;
  double m_MovingImageTrueMin;
  double m_MovingImageTrueMax;
  double m_FixedImageBinSize;
  double m_MovingImageBinSize;

  /** Typedefs for BSpline kernel and derivative functions. */
  typedef BSplineKernelFunction<3> CubicBSplineFunctionType;
  typedef BSplineDerivativeKernelFunction<3> 
    CubicBSplineDerivativeFunctionType;

  /** Cubic BSpline kernel for computing Parzen histograms. */
  typename CubicBSplineFunctionType::Pointer m_CubicBSplineKernel;
  typename CubicBSplineDerivativeFunctionType::Pointer 
    m_CubicBSplineDerivativeKernel;

  /**
   * Types and variables related to image derivative calculations.
   * If a BSplineInterpolationFunction is used, this class obtain
   * image derivatives from the BSpline interpolator. Otherwise, 
   * image derivatives are computed using central differencing.
   */
  typedef CovariantVector< double,
    itkGetStaticConstMacro(MovingImageDimension) > ImageDerivativesType;

  /** Compute image derivatives at a point. */
  virtual void ComputeImageDerivatives( const MovingImagePointType& mappedPoint,
    ImageDerivativesType& gradient ) const;

  /** Boolean to indicate if the interpolator BSpline. */
  bool m_InterpolatorIsBSpline;

  /** Typedefs for using BSpline interpolator. */
  typedef typename Superclass::CoordinateRepresentationType
    CoordinateRepresentationType;
  typedef
    BSplineInterpolateImageFunction<MovingImageType,
      CoordinateRepresentationType> BSplineInterpolatorType;

  /** Pointer to BSplineInterpolator. */
  typename BSplineInterpolatorType::Pointer m_BSplineInterpolator;

  /** Typedefs for using central difference calculator. */
  typedef CentralDifferenceImageFunction<MovingImageType,
    CoordinateRepresentationType> DerivativeFunctionType;

  /** Pointer to central difference calculator. */
  typename DerivativeFunctionType::Pointer m_DerivativeCalculator;

  /** Transform a point from FixedImage domain to MovingImage domain.
   * This function also checks if mapped point is within support region. */
  virtual void TransformPoint( const FixedImagePointType& fixedImagePoint,
    MovingImagePointType& mappedPoint, bool& sampleWithinSupportRegion,
    double& movingImageValue ) const;

  /** Compute PDF derivative contribution for each parameter. */
  virtual void ComputePDFDerivatives( const FixedImagePointType& fixedImagePoint,
    int fixedImageParzenWindowIndex, int movingImageParzenWindowIndex,
    const ImageDerivativesType& movingImageGradientValue,
    double cubicBSplineDerivativeValue ) const;

  /**
   * Types and variables related to BSpline deformable transforms.
   * If the transform is of type third order BSplineDeformableTransform,
   * then we can speed up the metric derivative calculation by
   * only inspecting the parameters within the support region
   * of a mapped point.
   */

  /** Boolean to indicate if the transform is BSpline deformable. */
  bool m_TransformIsBSpline;

  /** The number of BSpline parameters per image dimension. */
  long m_NumParametersPerDim;

  /** 
   * The number of BSpline transform weights is the number of
   * of parameter in the support region (per dimension ). */   
  unsigned long m_NumBSplineWeights;

  /** The fixed image dimension. */
  itkStaticConstMacro( FixedImageDimension, unsigned int,
    FixedImageType::ImageDimension );

  /** 
   * Enum of the deformabtion field spline order. 
   */
  enum { DeformationSplineOrder = 3 };

  /**
   * Typedefs for the BSplineDeformableTransform.
   */
  typedef BSplineDeformableTransform<
    CoordinateRepresentationType,
    ::itk::GetImageDimension<FixedImageType>::ImageDimension,
    DeformationSplineOrder> BSplineTransformType;
  typedef typename BSplineTransformType::WeightsType 
    BSplineTransformWeightsType;
  typedef typename BSplineTransformType::ParameterIndexArrayType 
    BSplineTransformIndexArrayType;

  /**
   * Variables used when transform is of type BSpline deformable.
   */
  typename BSplineTransformType::Pointer m_BSplineTransform;
  mutable BSplineTransformWeightsType    m_BSplineTransformWeights;
  mutable BSplineTransformIndexArrayType m_BSplineTransformIndices;  

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMattesMutualInformationImageToImageMetric.txx"
#endif

#endif

