/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformationRigidRegistrationVW.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkMutualInformationRigidRegistrationVW_h
#define _itkMutualInformationRigidRegistrationVW_h

#include "itkMutualInformationRigidRegistration.h"
#include "itkAffineMutualInformation.h"
#include "itkAffineMutualInformationVW.h"

namespace itk
{

/** 
 * \class MutualInformationRigidRegistrationVW
 * \brief Rigid mutual information registration by the Viola and Wells method.
 *
 * MutualInformationRigidRegistrationVW performs a rigid registration
 * between a reference and test image using mutual information. Specially,
 * it uses the optimization method of Viola and Wells to find the
 * best affine transform to register the test image onto the reference
 * image.
 *
 * In this optimization scheme any subclass of AffineMutualInformation
 * can be used as the mutual information calculator. The calculator can
 * be set using method SetMutualInformationCalculator(). The default calculator
 * is of type AffineMutualInformationVW.
 *
 * This class uses a simple stochastic gradient descent scheme. Steps
 * are taken repeatedly taken that are proportional to the approximate
 * deriviative of the mutual information with respect to the affine
 * transform parameters. The stepsize is governed by the LearningRate
 * parameter. The default is 1.0. The LearningRate is set via the method
 * SetLearningRate(). Typically, the learning rate should be annealed
 * (decreased over time).
 *
 * Since values in the affine matrix parameter is different in magnitud
 * to the values in the affine vector parameter, scaling is required
 * to improve convergence. Scaling for the values in the affine matrix
 * parameter can be set via SetMatrixScaling(). Default is 0.001 for
 * all values in the matrix. Scaling for the values in the affine 
 * vector parameter is set via SetVectorScaling(). Default is 1.0 for
 * all values in the vector.
 *
 * Implementaton of this class is based on:
 * Viola, P. and Wells III, W. (1997). 
 * "Alignment by Maximization of Mutual Information"
 * International Journal of Computer Vision, 24(2):137-154
 *
 * This class is templated over the reference image type, the test
 * image type and the test derivative image type.
 *
 * Possible improvements:
 * - current optimization is based on a simple steepest scheme.
 * Future implementation should look at more complex schemes.
 *
 * \sa AffineMutualInformation
 * \sa AffineMutualInformationVW.
 */
template <
class TRefImage,
class TTestImage,
class TDerivImage
>
class ITK_EXPORT MutualInformationRigidRegistrationVW :
  public MutualInformationRigidRegistration<TRefImage,TTestImage,TDerivImage>
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef MutualInformationRigidRegistrationVW Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef 
    MutualInformationRigidRegistration<TRefImage,TTestImage,TDerivImage> 
      Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(MutualInformationRigidRegistrationVW, 
    MutualInformationRigidRegistration);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * CalculatorType typedef support. Type of the mutual information calculator.
   */
  typedef AffineMutualInformation<TRefImage,TTestImage,TDerivImage>
    CalculatorType;

  /**
   * DefaultCalculatorType typedef support. Type of the default mutual information
   * calculator.
   */
  typedef AffineMutualInformationVW<TRefImage,TTestImage,TDerivImage> 
    DefaultCalculatorType;

  /**
   * RefImageType typedef support.
   */
  typedef TRefImage RefImageType;
  typedef typename RefImageType::Pointer RefImagePointer;

  /**
   * TestImageType typedef support.
   */
  typedef TTestImage TestImageType;
  typedef typename TestImageType::Pointer TestImagePointer;

  /**
   * DerivImageType typedef support.
   */
  typedef TDerivImage DerivImageType;
  typedef typename DerivImageType::Pointer DerivImagePointer;

  /**
   * ImageDimension enumeration.
   */
  enum { ImageDimension = TRefImage::ImageDimension };

  /**
   * MatrixType typedef support.
   */
  typedef vnl_matrix_fixed<double,ImageDimension,ImageDimension> MatrixType;

  /**
   * VectorType typedef support.
   */
  typedef vnl_vector_fixed<double,ImageDimension> VectorType;

  /**
   * Set the input reference image.
   */
  void SetReferenceImage( RefImageType *ptr );

  /**
   * Set the input test image.
   */
  void SetTestImage( TestImageType *ptr );

  /**
   * Set one of the test derivative images.
   */
  void SetTestImageDerivative( DerivImageType *ptr, unsigned int idx );

  /**
   * Set the mutual information calculator. The default calculator is of type
   * AffineMutalInformationVW.
   */
  void SetMutualInformationCalculator( CalculatorType *ptr );

  /**
   * Get the mutual information calculator
   */
  typename CalculatorType::Pointer GetMutualInformationCalculator()
    { return m_MutualInformationCalculator; }

  /**
   * Get the last affine parameter matrix visited.
   */
  const MatrixType& GetLastAffineMatrix() const
    { return m_AffineMatrix; }

  /**
   * Get the last affine parameter vector visited.
   */
  const VectorType& GetLastAffineVector() const
    { return m_AffineVector; }

  /** 
   * Set the learning rate. This is used in the optimization scheme.
   * Typically, the learning rate needs to be annealed (decreased over
   * time). Default is 1.0.
   */
  itkSetClampMacro( LearningRate, double, 0.0,
    NumericTraits<double>::max() );

  /**
   * Get the learning rate.
   */
  itkGetMacro( LearningRate, double );

  /**
   * Set the affine matrix scaling factors. This is used to scale 
   * each of the affine matrix parameters during the steepest descent 
   * optimization. Default is a matrix with all values of 0.001.
   */
  void SetMatrixScaling( MatrixType& matrix )
    { m_MatrixScaling = matrix; }

  /** 
   * Get the affine matrix scaling factors.
   */
  const MatrixType& GetMatrixScaling() const
    { return m_MatrixScaling; }

  /**
   * Set the affine vector scaling factors. This is used to scale
   * each of the affine vector parameters during the steepest descent
   * optimization. Default is a vector with all values of 1.0.
   */
  void SetVectorScaling( VectorType& vector )
    { m_VectorScaling = vector; }

  /**
   * Get the affine vector scaling factors.
   */
  const VectorType& GetVectorScaling() const
    { return m_VectorScaling; }

  /** 
   * Set the number of iterations. This determines the number of
   * iterations performed in the steepest descent optimization.
   * Default is 10.
   */
  itkSetMacro( NumberOfIterations, unsigned int );

  /**
   * Get the number of iterations.
   */
  itkGetMacro( NumberOfIterations, unsigned int );

  /** 
   * Maximize the mutual information using a steepest descent 
   * optmization scheme.
   */
  virtual void Maximize();

protected:
  MutualInformationRigidRegistrationVW();
  ~MutualInformationRigidRegistrationVW(){};
  MutualInformationRigidRegistrationVW(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent);

private:

  typename CalculatorType::Pointer            m_MutualInformationCalculator;
  double                                      m_LearningRate;
  unsigned int                                m_NumberOfIterations;
  MatrixType                                  m_AffineMatrix;
  VectorType                                  m_AffineVector;
  MatrixType                                  m_MatrixScaling;
  VectorType                                  m_VectorScaling;

  bool                                        m_DebugOn;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMutualInformationRigidRegistrationVW.txx"
#endif

#endif
