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
#include "itkConceptChecking.h"
#include "itkImageToImageFilterDetail.h"

namespace itk
{
  
/** \class ImageToImageFilter
 * \brief Base class for filters that take an image as input and produce an image as output.
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
  
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageFilter,ImageSource);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType; 
  typedef typename InputImageType::PixelType      InputImagePixelType; 
  
  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Set/Get the image input of this process object.  */
  virtual void SetInput( const InputImageType *image);
  virtual void SetInput( unsigned int, const TInputImage * image);
  const InputImageType * GetInput(void);
  const InputImageType * GetInput(unsigned int idx);


 protected:
  ImageToImageFilter();
  ~ImageToImageFilter();

  virtual void PrintSelf(std::ostream& os, Indent indent) const;

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


  /** Typedef for the region copier function object. */
  typedef ImageToImageFilterDetail::ImageRegionCopier<itkGetStaticConstMacro(InputImageDimension),
                                                      itkGetStaticConstMacro(OutputImageDimension)> RegionCopierType;

  /** This function calls the actual region copier to do the mapping from
   * output image space to input image space.  It uses a 
   * Function object used for dispatching to various routines to
   * copy an output region (start index and size) to an input region.
   * For most filters, this is a trivial copy because most filters
   * require the input dimension to match the output dimension.
   * However, some filters like itk::ExtractImageFilter can
   * support output images of a lower dimension that the input.
   *
   * This function object is used by the default implementation of
   * GenerateInputRequestedRegion(). It can also be used in routines
   * like ThreadedGenerateData() where a filter may need to map the
   * the output region for a particular thread to an input region.
   *
   * The default copier uses a "dispatch pattern" to call one of three
   * overloaded functions depending on whether the input and output
   * images are the same dimension, the input is a higher dimension
   * that the output, or the input is of a lower dimension than the
   * output. The use of an overloaded function is required for proper
   * compilation of the various cases.
   * 
   * For the latter two cases, trivial implementations are used.  If
   * the input image is a higher dimension than the output, the output
   * region information is copied into the first portion of the input
   * region and the rest of the input region is set to zero.  If the
   * input region is a lower dimension than the output, the first
   * portion of the output region is copied to the input region.
   *
   * If a filter needs a different default behavior, it can call the
   * SetRegionCopier() method passing in a subclass of the
   * ImageToImageDetail::RegionCopier function object. This would
   * typically be done in the constructor of the filter. The
   * ExtractImageFilter overrides this function object so that if the
   * input image is a higher dimension than the output image, the
   * filter can control "where" in the input image the output subimage
   * is extracted (as opposed to mapping to first few dimensions of
   * the input). */
  virtual void CallCopyRegion(InputImageRegionType &destRegion,
                              const OutputImageRegionType &srcRegion);

private:
  ImageToImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageFilter.txx"
#endif

#endif
