/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNonThreadedShrinkImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNonThreadedShrinkImageFilter_h
#define __itkNonThreadedShrinkImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class NonThreadedShrinkImageFilter
 * \brief Reduce the size of an image by an integer factor.
 *
 * NonThreadedShrinkImageFilter reduces the size of an image by an
 * integer factor. The algorithm implemented is a simple
 * subsample. Since this filter produces an image which is a different
 * resolution and with different pixel spacing than its input image,
 * it needs to override several of the methods defined in
 * ProcessObject in order to properly manage the pipeline execution
 * model.  In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::GenerateOutputInformation().
 * 
 * \ingroup GeometricTransforms
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT NonThreadedShrinkImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef NonThreadedShrinkImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(NonThreadedShrinkImageFilter, ImageToImageFilter);

  /** Set the shrink factor. The default value is 1. */
  itkSetClampMacro(ShrinkFactor,unsigned int, 1,
                   NumericTraits<unsigned int>::max());
  
  /** Get the shrink factor. */
  itkGetMacro(ShrinkFactor,unsigned int);
                 
  /** NonThreadedShrinkImageFilter produces an image which is a
   * different resolution and with a different pixel spacing than its
   * input image.  As such, NonThreadedShrinkImageFilter needs to
   * provide an implementation for GenerateOutputInformation() in
   * order to inform the pipeline execution model.  The original
   * documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation();

  /** NonThreadedShrinkImageFilter needs a larger input requested
   * region than the output requested region.  As such,
   * NonThreadedShrinkImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

protected:
  NonThreadedShrinkImageFilter();
  ~NonThreadedShrinkImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  void GenerateData();

private:
  NonThreadedShrinkImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int m_ShrinkFactor;
};
  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNonThreadedShrinkImageFilter.txx"
#endif
  
#endif
