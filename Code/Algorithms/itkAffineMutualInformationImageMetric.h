/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineMutualInformationImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkAffineMutualInformationImageMetric_h
#define _itkAffineMutualInformationImageMetric_h

#include "itkImage.h"
#include "itkMutualInformationImageMetric.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{

/** \class AffineMutualInformationImageMetric
 * \brief Calculates the mutual information between a reference 
 * and an affine transformed test image.
 *
 * AffineMutualInformationImageMetric is an abstract which calculates
 * the mutual information between a reference and a affine
 * transformed test image.
 *
 * An affine transform is a linear coordinate transformation
 * that includes translation, rotation, scaling, stretching
 * and shearing. The transform can be specified in terms
 * of a vector addition and matrix multplication.
 *
 * transformed_index = matrix * index + vector
 *
 * The affine transformed is specified in terms of one matrix of
 * type AffineMutualInformationImageMetric::MatrixType and one vector of
 * type AffineMutualInformationImageMetric::VectorType. This class provides methods 
 * SetAffineMatrix() and SetAffineVector() for defining the affine 
 * transform. The default matrix is the identity matrix and the 
 * default vector is an all zero vector.
 *
 * The method CalculateMutualInformationImageMetricAndGradient() estimates a 
 * stochastic derivative of the mutual information with respect to 
 * the affine transform parameters. The stochastic derivative can
 * be used for gradient-based optimization schemes. Gradient calculation 
 * requires derivatives of the test image. These test derivatives images can 
 * be defined using the method SetDerivativeImage(). There should be 
 * one derivative image for each dimension of the test image.
 *
 * This class is templated over the reference image type,
 * the test image type and the test derivative image type.
 *
 * Possible improvements:
 * - make calculating the image derivatives part of the class.
 *
 * \sa MutualInformationImageMetric
 * 
 */
template <
class TRefImage,
class TTestImage,
class TDerivImage = Image<float,TTestImage::ImageDimension>
>
class ITK_EXPORT AffineMutualInformationImageMetric :
  public MutualInformationImageMetric<TRefImage,TTestImage>
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef AffineMutualInformationImageMetric Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef MutualInformationImageMetric<TRefImage,TTestImage> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(AffineMutualInformationImageMetric, MutualInformationImageMetric);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

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
   * DerivImage typedef support.
   */
  typedef TDerivImage DerivImageType;

  /** 
   * DerivImagePointer typedef support
   */
  typedef typename DerivImageType::Pointer DerivImagePointer;

  /**
   * Set the affine parameter matrix.
   */
  void SetAffineMatrix( MatrixType& matrix )
    { 
      m_AffineMatrix = matrix; 
      this->Modified();
    }

  /**
   * Get the affine parameter matrix.
   */
  const MatrixType& GetAffineMatrix() const
    { return m_AffineMatrix; }

  /**
   * Set the affine parameter vector.
   */
  void SetAffineVector( VectorType& vector )
    { 
      m_AffineVector = vector; 
      this->Modified();
    }

  /**
   * Get the affine parameter vector.
   */
  const VectorType& GetAffineVector() const
    { return m_AffineVector; }

  /**
   * Set one of the test derivative images.
   */
  void SetTestImageDerivative( DerivImageType *ptr, unsigned int idx );

  /**
   * Get one of the test derivative images.
   */
  DerivImagePointer GetTestImageDerivative( unsigned int idx ) const
    {  
      if( idx >= ImageDimension ) return NULL;
      return m_DerivImages[idx];
    }

  /**
   * Calculate the mutual information gradient with respect to
   * the affine transform parameters.
   */
  virtual void CalculateMutualInformationImageMetricAndGradient(){};

  /**
   * Get the derivative of the mutual information with respect to
   * the affine matrix paramters.
   */
  const MatrixType& GetAffineMatrixDerivative() const
    { return m_AffineMatrixDerivative; }

  /**
   * Get the derivative of the mutual information with respect to
   * the affine vector parameters.
   */
  const VectorType& GetAffineVectorDerivative() const
    { return m_AffineVectorDerivative; }


protected:
  AffineMutualInformationImageMetric();
  ~AffineMutualInformationImageMetric(){};
  AffineMutualInformationImageMetric(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent);

  // made protected so subclass can access
  MatrixType           m_AffineMatrix;
  VectorType           m_AffineVector;
  
  DerivImagePointer    m_DerivImages[ImageDimension];
  MatrixType           m_AffineMatrixDerivative;
  VectorType           m_AffineVectorDerivative;

};


} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineMutualInformationImageMetric.txx"
#endif

#endif
