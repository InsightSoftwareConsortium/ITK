/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkMutualInformation_h
#define _itkMutualInformation_h

#include "itkObject.h"

namespace itk
{
/** \class MutualInformation
 * \brief Calculate the mutual information between a reference
 * and a test image.
 *
 * MutualInformation is an abstract class for all objects which
 * calculates the mutual information between two images.
 * Specifically, this class provides methods SetReferenceImage()
 * and SetTestImage() for defining the reference and test images.
 *
 * The method CalculateMutualInformation() evokes the calculation.
 * The mutual information value is obtained via method
 * GetMutualInformation().
 *
 * This class is templated over the image type of the reference
 * image and the image type of the test image.
 *
 *
 */
template <
class TRefImage, 
class TTestImage 
>
class ITK_EXPORT MutualInformation : public Object
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef MutualInformation Self;

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
  itkTypeMacro(MutualInformation, Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * RefImage typedef support
   */
  typedef TRefImage RefImageType;

  /**
   * RefImagePointer typedef support
   */
  typedef typename RefImageType::Pointer RefImagePointer;

  /**
   * TestImage typedef support.
   */
  typedef TTestImage TestImageType;

  /**
   * TestImagePointer typedef support
   */
  typedef typename TestImageType::Pointer TestImagePointer;

  /**
   * Set the input reference image.
   */
  virtual void SetReferenceImage( RefImageType * ptr );

  /**
   * Get the input reference image.
   */
  RefImagePointer GetReferenceImage() const
    { return m_RefImage; }

  /**
   * Set the input test image.
   */
  virtual void SetTestImage( TestImageType * ptr );

  /**
   * Get the input test image.
   */
  TestImagePointer GetTestImage() const
    { return m_TestImage; }

  /** 
   * Get the mutual information value from the last calculation.
   */
  itkGetMacro( MutualInformation, double );

  /**
   * Calculate the mutual information between the reference image 
   * and the test image.
   */
  virtual void CalculateMutualInformation(){};

protected:
  MutualInformation();
  ~MutualInformation(){};
  MutualInformation(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent);

  // made protected so subclass can access
  RefImagePointer   m_RefImage;
  TestImagePointer  m_TestImage;
  double            m_MutualInformation;

};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMutualInformation.txx"
#endif

#endif
