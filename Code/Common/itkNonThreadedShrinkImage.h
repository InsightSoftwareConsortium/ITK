/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNonThreadedShrinkImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkNonThreadedShrinkImage_h
#define __itkNonThreadedShrinkImage_h

#include "itkFilterImageToImage.h"

namespace itk
{

/** \class NonThreadedShrinkImage
 * \brief Reduce the size of an image by an integer factor.
 *
 * NonThreadedShrinkImage reduces the size of an image by an integer factor. The
 * algorithm implemented is a simple subsample. Since this filter produces
 * an image which is a different resolution and with different pixel spacing
 * than its input image, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::UpdateOutputInformation().
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT NonThreadedShrinkImage:
    public FilterImageToImage<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef NonThreadedShrinkImage         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FilterImageToImage<TInputImage,TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(NonThreadedShrinkImage, FilterImageToImage);

  /** 
   * Set the shrink factor. The default value is 1.
   */
  itkSetClampMacro(ShrinkFactor,unsigned int, 1,
                   NumericTraits<unsigned int>::max());
  
  /** 
   * Get the shrink factor.
   */
  itkGetMacro(ShrinkFactor,unsigned int);
                 
  /**
   * NonThreadedShrinkImage produces an image which is a different resolution
   * and with a different pixel spacing than its input image.  As such,
   * NonThreadedShrinkImage needs to provide an implementation for
   * UpdateOutputInformation() in order to inform the pipeline execution
   * model.  The original documentation of this method is below.
   *
   * \sa ProcessObject::UpdateOutputInformaton() */
  virtual void UpdateOutputInformation();

  /**
   * NonThreadedShrinkImage needs a larger input requested region than the
   * output requested region.  As such, NonThreadedShrinkImage needs to
   * provide an implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

 protected:
  NonThreadedShrinkImage();
  ~NonThreadedShrinkImage() {};
  NonThreadedShrinkImage(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);
  
  void GenerateData();

private:
  unsigned int m_ShrinkFactor;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNonThreadedShrinkImage.txx"
#endif
  
#endif
