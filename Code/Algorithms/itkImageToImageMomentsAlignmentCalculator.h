/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSFile: $
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkImageToImageMomentsAlignmentCalculator_h
#define __itkImageToImageMomentsAlignmentCalculator_h

#include "itkAffineTransform.h"
#include "itkObject.h"
#include "itkImageMomentsCalculator.h"

namespace itk
{

/** 
 * \Compute initial parameters for an affine transformation.
 *  The two inputs are images that we want to adjust.
 *  The output is the transformation that aligns the two images
 */

template <class TReference, class TTarget>          
class ITK_EXPORT ImageToImageMomentsAlignmentCalculator: public Object 
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef ImageToImageMomentsAlignmentCalculator  Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   *  Type of the Reference
   */
  typedef TReference  ReferenceType;

  /**
   *  Type of the Target
   */
  typedef TTarget TargetType;

  /**
   *  Pointer type for the Reference 
   */
  typedef typename ReferenceType::Pointer ReferencePointer;
  
  /**
   *  Pointer type for the Target 
   */
  typedef typename TargetType::Pointer TargetPointer;

  /**
   *  Typedef for the image moments calculator
   */
  typedef ImageMomentsCalculator<ReferenceType>  ImageMomentsCalculatorType;

  /**
   * Standard affine transform type for this class
   */  
  enum {ImageDimension = ReferenceType::ImageDimension};

  typedef AffineTransform<double, ImageDimension> AffineTransformType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Self, Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Set the Target
   */
  void SetTarget( TargetType * );
   
  /**
   * Set the Reference
   */
  void SetReference( ReferenceType * );

  /**
   * Execute 
   */
  void Execute(void);

  /**
   * Get the Tranformation
   */
  itkGetMacro(OutputTransform ,AffineTransformType);


protected:

  ImageToImageMomentsAlignmentCalculator();
  virtual ~ImageToImageMomentsAlignmentCalculator();
  ImageToImageMomentsAlignmentCalculator(const Self&);
  const Self & operator=(const Self&);
  
private:
   
  ReferencePointer     m_Reference;
  TargetPointer        m_Target;
  AffineTransformType  m_OutputTransform;

};  


}; // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageMomentsAlignmentCalculator.txx"
#endif

#endif 
