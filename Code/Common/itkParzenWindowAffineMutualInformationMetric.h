/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkParzenWindowAffineMutualInformationMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkAffineMutualInformationVW_h
#define _itkAffineMutualInformationVW_h

#include "itkAffineMutualInformation.h"
#include "itkIndex.h"
#include "itkLinearInterpolateFunction.h"
#include "itkKernelFunction.h"
#include "vnl/vnl_math.h"

namespace itk
{

/** 
 * \class AffineMutualInformationVW
 * \brief Calculates the mutual information between a reference
 * and an affine transformed test image using the Viola and Wells method.
 *
 * AffineMutualInformationVW calculats the mutual information
 * between a reference image and a affine transformed test image
 * using the method of Viola and Wells where the probability
 * density distributions are estimated using Parzen windows.
 *
 * By default a Gaussian kernel is used in the density estimation.
 * Other option include Cauchy and spline-based. A user can specify
 * the kernel passing in a pointer a KernelFunction using the
 * SetKernelFunction() method.
 *
 * Mutual information is estimated using two sample set: one to calculate
 * the singluar and joint pdf's and one to calculate the entropy 
 * integral. By default 50 samples points are used in each set.
 * Other values can be set via the SetNumberOfSamples() method.
 *
 * Quality of the density estimate depends on the choice of the
 * kernel's variance. Optimal choice will depend on the images. 
 * It is can be shown that around the optimal variance, the mutual
 * information estimate is relatively insensitive to small changes
 * of the variance. In our experiments, we have found that a 
 * variance of 0.1 works well for images normalized between 0 and 1.
 * The variance can be set via methods SetReferenceStdDev()
 * and SetTestStdDev().
 * 
 * Implementaton of this class is based on:
 * Viola, P. and Wells III, W. (1997). 
 * "Alignment by Maximization of Mutual Information"
 * International Journal of Computer Vision, 24(2):137-154
 *
 * This class is templated over the reference image type, the test
 * image type and the test derivative image type.
 *
 * Caveat:
 * This algorithm assumes that the all of the images are 
 * in the image buffered before CalculateMutualInformation()
 * or CalculateMutualInformationAndGradient() is called.
 * It does not activative any updating of the inputs. This
 * must be done before the functions are called.
 *
 * Possible Improvements:
 * Current algorithm is of order N^2 where N is the sample size.
 * The can be improved by using a finitely supported kernel and
 * sorting the samples.
 *
 * \sa AffineMutualInformation
 * \sa KernelFunction
 *
 */
template <
class TRefImage,
class TTestImage,
class TDerivImage
>
class ITK_EXPORT AffineMutualInformationVW :
  public AffineMutualInformation<TRefImage,TTestImage,TDerivImage>
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef AffineMutualInformationVW Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef AffineMutualInformation<TRefImage,TTestImage,TDerivImage> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(AffineMutualInformationVW, AffineMutualInformation);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * ImageDimension enumeration.
   * Although already defined in the superclass, needed here for gcc 2.95.2-5
   * to compile.
   */
  enum { ImageDimension = TRefImage::ImageDimension };

  /**
   * ImageIndex typedef support.
   */
  typedef itk::Index<ImageDimension> IndexType;

  /**
   * Set the input test image.
   * Caveat: requires the whole dataset to be in the image buffer.
   */
  void SetTestImage( TestImageType *ptr );

  /**
   * Set the number of spatial samples. This is the number of image
   * samples used to calculate the joint probability distribution.
   * Default value is 50.
   */
  void SetNumberOfSamples( unsigned int num )
  { this->Resize( num ); }

  /**
   * Get the number of spatial samples.
   */
  itkGetConstMacro( NumberOfSamples, unsigned int );

  /**
   * Set the reference image intensitiy standard deviation. This
   * defines the kernel bandwidth used in the joint probability
   * distribution calculation. Default value is 0.1 which works 
   * well for image intensities normalized to between 0 and 1.
   */
  itkSetClampMacro( ReferenceStdDev, double, 0.0, 
    NumericTraits<double>::max() );

  /**
   * Get the reference image intensity standard deviation.
   */
  itkGetMacro( ReferenceStdDev, double );

  /**
   * Set the test image intensitiy standard deviation. This defines
   * the kernel bandwidth used in the joint probability distribution
   * calculation. Default value is 0.1 which works well for image
   * intensities normalized to between 0 and 1.
   */
  itkSetClampMacro( TestStdDev, double, 0.0,
    NumericTraits<double>::max() );

  /**
   * Get the test image intensity standard deviation.
   */
  itkGetMacro( TestStdDev, double );

  /**
   * Set the kernel function. This is used to calculate the joint
   * probability distribution. Default is the GaussianKernelFunction.
   */
  void SetKernelFunction( KernelFunction * ptr )
    { m_KernelFunction = ptr; }

  /**
   * Get the kernel function.
   */
  KernelFunction * GetKernelFunction()
    { return m_KernelFunction; }

  /**
   * Calculate the mutual information between the reference image and the
   * test image using the current affine transformation. The input argument
   * resample determines if the calculation is based on the current spatial
   * sample or a new one.
   */
  virtual void CalculateMutualInformation()
  {
    this->CalculateMutualInformation( true );
  }
  void CalculateMutualInformation( bool resample = true );

  /**
   * Calculate the mutual information and the gradient with respect 
   * to the current affine transformtion. The input argument resample 
   * determines if the calculation is based on the current spatial 
   * sample or a new one.
   */
  virtual void CalculateMutualInformationAndGradient()
  {
    this->CalculateMutualInformationAndGradient( true );
  }
  void CalculateMutualInformationAndGradient( bool resample = true );

protected:
  AffineMutualInformationVW();
  ~AffineMutualInformationVW(){};
  AffineMutualInformationVW(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent);

private:

  typedef std::vector<IndexType>    IndexContainer;
  typedef std::vector<double>       IntensityContainer;
  typedef std::vector<MatrixType>   MatrixContainer;
  typedef std::vector<VectorType>   VectorContainer;

  unsigned int                            m_NumberOfSamples;
  bool                                    m_SamplesValid;
  IndexContainer                          m_SpatialSamplesA;
  IntensityContainer                      m_RefIntensitiesA;
  IntensityContainer                      m_TestIntensitiesA;
  VectorContainer                         m_TestDerivativesA;

  MatrixContainer                         m_TestMatrixDerivA;

  IndexContainer                          m_SpatialSamplesB;
  IntensityContainer                      m_RefIntensitiesB;
  IntensityContainer                      m_TestIntensitiesB;
  VectorContainer                         m_TestDerivativesB;

  double                                  m_ReferenceStdDev;
  double                                  m_TestStdDev;
  typename KernelFunction::Pointer        m_KernelFunction;

  typedef 
    LinearInterpolateFunction<TTestImage> InterpolatorType;

  typename InterpolatorType::Pointer      m_Interpolator;

  void SpatialSample();
  void SpatialSample(IndexContainer&, IntensityContainer& );

  void CalculateTestIntensities();
  void CalculateTestIntensities(IndexContainer&, IntensityContainer&);

  void CalculateTestDerivatives();
  void CalculateTestDerivatives( IndexContainer&, VectorContainer& );

  void Resize( unsigned int num );

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineMutualInformationVW.txx"
#endif

#endif