/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNonThreadedShrinkImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNonThreadedShrinkImageFilter_h
#define __itkNonThreadedShrinkImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkShrinkImageFilter.h"

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
 * NOTE: This filter runs the ShrinkImageFilter with the number of
 * threads set to 1. To avoid confusion, developers shopuld replace
 * this class with ShrinkImageFilter. If you need to limit the number
 * of threads, instantiate the afilter and apply the
 * SetNumberOfThreads(1) methods.
 * \ingroup GeometricTransforms
 */
template< class TInputImage, class TOutputImage >
class ITK_EXPORT NonThreadedShrinkImageFilter:
  public ShrinkImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef NonThreadedShrinkImageFilter                    Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NonThreadedShrinkImageFilter, ShrinkImageFilter);

  virtual void SetNumberOfThreads(int)
  {
    Superclass::SetNumberOfThreads(1);
  }

protected:
  NonThreadedShrinkImageFilter()
  {
    Superclass::SetNumberOfThreads(1);
  }

private:
  NonThreadedShrinkImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);               //purposely not implemented
};
} // end namespace itk

#endif
