/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToImageFilter_h
#define __itkImageToImageFilter_h

#include "itkImageSource.h"

namespace itk
{

/** \class ImageToImageFilter
 * \brief 
 *
 * ImageToImageFilter is the base class for all process objects that output
 * image data and require image data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * This class provides the infrastructure for supporting multithreaded
 * processing of images.  If a filter provides an implementation of
 * GenerateData(), the image processing will run in a single thread and the
 * implementation is responsible for allocating its output data.  If a filter
 * provides an implementation of ThreadedGenerateData() instead, the image
 * will be divided into a number of pieces, a number of threads will be
 * spawned, and ThreadedGenerateData() will be called in each thread.  Here,
 * the output memory will be allocated by this superclass prior to calling
 * ThreadedGenerateData().
 *
 * ImageToImageFilter provides an implementation of
 * GenerateInputRequestedRegion().  The base assumption to this point in the
 * heirarchy is that a process object would ask for the largest possible
 * region on input in order to produce any output.  Imaging filters,
 * however, can usually answer this question more precisely.  The default
 * implementation of GenerateInputRequestedRegion() in this class is to
 * request an input that matches the size of the requested output.  If a
 * filter requires more input (say a filter that uses neighborhood
 * information) or less input (for instance a magnify filter), then these
 * filters will have to provide another implmentation of this method. By
 * convention, such implementations should call the Superclass' method
 * first.
 *
 * \ingroup ImageFilters
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ImageToImageFilter : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageFilter  Self;
  typedef ImageSource<TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageFilter,ImageSource);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 
  
  /** Set/Get the image input of this process object.  */
  virtual void SetInput(InputImageType *input);
  InputImagePointer GetInput();
  InputImagePointer GetInput(unsigned int idx);
  
protected:
  ImageToImageFilter();
  ~ImageToImageFilter();

  /** What is the input requested region that is required to produce the
   * output requested region? The base assumption for image processing
   * filters is that the input requested region can be set to match the
   * output requested region.  If a filter requires more input (for instance
   * a filter that uses neighborhoods needs more input than output to avoid
   * introducing artificial boundary conditions) or less input (for instance 
   * a magnify filter) will have to override this method.  In doing so, it
   * should call its superclass' implementation as its first step. Note that
   * this imaging filters operate differently than the classes to this
   * point in the class hierachy.  Up till now, the base assumption has been
   * that the largest possible region will be requested of the input.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion(),
   *     ImageSource::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();
  
private:
  ImageToImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageFilter.txx"
#endif

#endif
