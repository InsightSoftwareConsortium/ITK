/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageMomentsAlignmentCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToImageMomentsAlignmentCalculator_h
#define __itkImageToImageMomentsAlignmentCalculator_h

#include "itkAffineTransform.h"
#include "itkObject.h"
#include "itkImageMomentsCalculator.h"

namespace itk
{

/** Compute initial parameters for an affine transformation.
 *  The two inputs are images that we want to adjust.
 *  The output is the transformation that aligns the two images
 * 
 * \ingroup Operators
 */
template <class TReference, class TTarget>          
class ITK_EXPORT ImageToImageMomentsAlignmentCalculator: public Object 
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageMomentsAlignmentCalculator  Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Self, Object);

  /**  Type of the reference. */
  typedef TReference  ReferenceType;

  /**  Type of the target. */
  typedef TTarget TargetType;

  /**  Pointer type for the reference.  */
  typedef typename ReferenceType::ConstPointer ReferenceConstPointer;
  
  /**  Pointer type for the target.  */
  typedef typename TargetType::ConstPointer TargetConstPointer;

  /**  Typedef for the image moments calculator. */
  typedef ImageMomentsCalculator<ReferenceType>  ImageMomentsCalculatorType;

  /** Standard affine transform type for this class. */  
  enum {ImageDimension = ReferenceType::ImageDimension};

  /** The type of affine transformation. */  
  typedef AffineTransform<double, ImageDimension> AffineTransformType;

  /** Set the target. */
  void SetTarget( TargetType * );
   
  /** Set the reference. */
  void SetReference( ReferenceType * );

  /** Execute.  */
  void Execute(void);

  /** Get the tranformation. */
  itkGetMacro(OutputTransform ,AffineTransformType);

protected:
  ImageToImageMomentsAlignmentCalculator();
  virtual ~ImageToImageMomentsAlignmentCalculator();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  ImageToImageMomentsAlignmentCalculator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  ReferenceConstPointer     m_Reference;
  TargetConstPointer        m_Target;
  AffineTransformType       m_OutputTransform;

};  


} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageMomentsAlignmentCalculator.txx"
#endif

#endif 
