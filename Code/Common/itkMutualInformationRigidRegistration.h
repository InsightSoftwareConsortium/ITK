/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformationRigidRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkMutualInformationRigidRegistration_h
#define _itkMutualInformationRigidRegistration_h

#include "itkObject.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{

/** 
 * \class MutualInformationRigidRegistration
 * \brief Rigid mutual information registration.
 *
 * MutualInformationRigidRegistration is a base class for all objects
 * which performs a rigid registration between a reference and
 * test image using mutual information. Specifically, the method
 * Maximize() attempts to find the best affine transform to
 * register the test image onto the reference image.
 *
 * The input reference and test image are defined via methods
 * SetReferenceImage() and SetTestImage(). Derivates of the test image
 * are also required and are set via SetDerivativeImage().
 * Initial starting point of the affine transform parameters can be defined 
 * using methods SetInitialAffineMatrix() and SetInitialAffineVector().
 *
 * After the method Maximize() has been evoked, the best affine
 * transform with respect to the mutual information value can be
 * obtained via GetBestAffineMatrix() and GetBestAffineVector();
 * 
 * This class is templated over the reference image type, the test
 * image type and the test derivative image type.
 */
template <
class TRefImage,
class TTestImage,
class TDerivImage
>
class ITK_EXPORT MutualInformationRigidRegistration :
  public Object
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef MutualInformationRigidRegistration Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef Object Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(MutualInformationRigidRegistration, Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

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
  virtual void SetReferenceImage( RefImageType *ptr )
    { m_RefImage = ptr; }

  /**
   * Get the input reference image.
   */
  RefImagePointer GetReferenceImage()
    { return m_RefImage; }

  /**
   * Set the input test image.
   */
  virtual void SetTestImage( TestImageType *ptr )
    { m_TestImage = ptr; }

  /**
   * Get the input test image
   */
  TestImagePointer GetTestImage()
    { return m_TestImage; }

  /**
   * Set one of the test derivative images.
   */
  void SetTestImageDerivative( DerivImageType *ptr, unsigned int idx )
    {
      if ( idx >= ImageDimension ) return;
      m_DerivImage[idx] = ptr;
    }

  /**
   * Get one of the test derivative images.
   */
  DerivImagePointer GetTestImageDerivative( unsigned int idx ) const
    {  
      if( idx >= ImageDimension ) return NULL;
      return m_DerivImages[idx];
    }

  /**
   * Set the initial affine parameter matrix.
   */
  void SetInitialAffineMatrix( MatrixType& matrix )
    { m_InitialMatrix = matrix; }

  /**
   * Set the initial affine parameter vector.
   */
  void SetInitialAffineVector( VectorType& vector )
    { m_InitialVector = vector; }

  /** 
   * Get the best affine matrix parameter visited. This returns the
   * best affine parameter with respect to mutual information.
   */
  const MatrixType& GetBestAffineMatrix() const
    { return m_BestAffineMatrix; }

  /** 
   * Get the best affine vector parameter visited. This returns the
   * best affine parameter with respect to mutual information.
   */
  const VectorType& GetBestAffineVector() const
    { return m_BestAffineVector; }

  /**
   * Get the best mutual information value visited.
   */
  itkGetMacro( BestMutualInformation, double );

  /** 
   * Maximize the mutual information.
   */
  virtual void Maximize() = 0;

protected:
  MutualInformationRigidRegistration();
  ~MutualInformationRigidRegistration(){};
  MutualInformationRigidRegistration(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent);

  // made protected so subclass can access
  RefImagePointer       m_RefImage;
  TestImagePointer      m_TestImage;
  DerivImagePointer     m_DerivImages[ImageDimension];

  MatrixType            m_InitialMatrix;
  VectorType            m_InitialVector;

  double                m_BestMutualInformation;
  MatrixType            m_BestAffineMatrix;
  VectorType            m_BestAffineVector;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMutualInformationRigidRegistration.txx"
#endif

#endif
