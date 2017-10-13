/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkImageToImageFilter_h
#define itkImageToImageFilter_h

#include "itkImageSource.h"
#include "itkConceptChecking.h"
#include "itkImageToImageFilterDetail.h"
#include "itkImageToImageFilterCommon.h"

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
 * hierarchy is that a process object would ask for the largest possible
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
 * All inputs to ImageToImageFilter (if there is more than one) are
 * checked in the VerifyInputInformation method to verify that they
 * occupy the same physical space.  If the input images are in the
 * same physical space, then the location of each voxel is identical,
 * and the filter can operate voxel-by-voxel in index space.  Some
 * filters -- registration filters, for example -- will violate this
 * precondition, in which case they should redefine
 * VerifyInputInformation to relax or eliminate this requirement.
 *
 * Access methods -- Set/GetCoordinateTolerance and
 * Set/GetDirectionTolerance -- are provided for cases where the
 * default spatial-congruency tolerances are too fine, and images that
 * are almost in the same space should be regard as being in the same
 * space. This has come up, for example when comparing different
 * on-disk image formats with differing digits of precision in the
 * position, spacing, and orientation.
 *
 * The default tolerance is govern by the
 * GlobalDefaultCoordinateTolerance and the
 * GlobalDefaultDirectionTolerance properties, defaulting to 1.0e-6.
 * The default tolerance for spatial comparison is then scaled by the
 * voxelSpacing for coordinates (i.e. the coordinates must be the same
 * to within one part per million). For the direction cosines the
 * values must be within the current absolute tolerance.
 *
 * \ingroup ImageFilters
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Developer/ImageFilter,Filter an image}
 * \wikiexample{Developer/ImageFilterMultipleInputs,Write a filter with multiple inputs of the same type.}
 * \wikiexample{Developer/ImageFilterMultipleInputsDifferentType,Write a filter with multiple inputs of different types.}
 * \wikiexample{Developer/ImageFilterMultipleOutputs,Write a filter with multiple outputs of the same type.}
 * \wikiexample{Developer/OilPaintingImageFilter,Multi-threaded oil painting image filter}
 * \wikiexample{Developer/ImageFilterMultipleOutputsDifferentType,Write a filter with multiple outputs of different types.}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ImageToImageFilter:public ImageSource< TOutputImage >,
  private ImageToImageFilterCommon
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageFilter          Self;
  typedef ImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageFilter, ImageSource);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::OutputImagePixelType  OutputImagePixelType;

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Set/Get the image input of this process object.  */
  using Superclass::SetInput;
  virtual void SetInput(const InputImageType *image);

  virtual void SetInput(unsigned int, const TInputImage *image);

  const InputImageType * GetInput() const;

  const InputImageType * GetInput(unsigned int idx) const;

  /** Push/Pop the input of this process object. These methods allow a
   * filter to model its input vector as a queue or stack.  These
   * routines may not be appropriate for all filters, especially
   * filters with different types of inputs.  These routines follow
   * the semantics of STL.
   *
   * The routines are useful for applications that need to process
   * "rolling" sets of images.  For instance, if an application has 10
   * images and they need to run a filter on images 1, 2, 3, 4, then
   * run the filter on images 2, 3, 4, 5, then run the filter on
   * images 3, 4, 5, 6, the application can accomplish this by popping
   * an input off the front of the input list and push a new image
   * onto the back of input list.  Again, this only makes sense for
   * filters that single type of input.
   *
   * Other uses are also possible. For a single input filter, pushing
   * and popping inputs allow the application to temporarily replace
   * an input to a filter.
   */
  virtual void PushBackInput(const InputImageType *image);

  virtual void PopBackInput() ITK_OVERRIDE;

  virtual void PushFrontInput(const InputImageType *image);

  virtual void PopFrontInput() ITK_OVERRIDE;

  /** get/set the Coordinate tolerance
   *  This tolerance is used when comparing the space defined
   *  by the input images.  With ITK 4.x a requirement has
   *  been added that both input images be congruent in space.
   */
  itkSetMacro(CoordinateTolerance,double);
  itkGetConstMacro(CoordinateTolerance,double);

  /** get/set the direction tolerance
   *  This tolerance is used to make sure that all input
   *  images are oriented the same before performing the filter's
   *  transformations.
   */
  itkSetMacro(DirectionTolerance,double);
  itkGetConstMacro(DirectionTolerance,double);

  /** get/set the global default direction tolerance
   *
   * This value is used to initialize the DirectionTolerance upon
   * class construction of \b any ImageToImage filter. This has no
   * effect on currently constructed classes.
   */
  using ImageToImageFilterCommon::SetGlobalDefaultDirectionTolerance;
  using ImageToImageFilterCommon::GetGlobalDefaultDirectionTolerance;


  /** get/set the global default coordinate tolerance
   *
   * This value is used to initialize the CoordinateTolerance upon
   * class construction of \b any ImageToImage filter. This has no
   * effect on currently constructed  classes.
   */
  using ImageToImageFilterCommon::SetGlobalDefaultCoordinateTolerance;
  using ImageToImageFilterCommon::GetGlobalDefaultCoordinateTolerance;


protected:
  ImageToImageFilter();
  ~ImageToImageFilter() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** \brief Verifies that the input images occupy the same physical
   * space and the each index is at the same physical location.
   *
   * The default implementation of the PropagateRequestedRegion
   * methods copies the index and size from the output to the
   * input. This makes an implicit assumption that the images occupy
   * the same physical location at each voxel. This method enforces
   * that they are the same.
   *
   * This implementation verifies that all input images of
   * InputImageDimensions have the same origin, spacing and direction.
   *
   * Filters which do not expect all input images to be at the same
   * physical location should over-ride this method. Also filters
   * whose inputs are different dimensions may need to overide this
   * method.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  virtual void VerifyInputInformation() ITK_OVERRIDE;

  /** What is the input requested region that is required to produce
   * the output requested region? The base assumption for image
   * processing filters is that the input requested region can be set
   * to match the output requested region.  If a filter requires more
   * input (for instance a filter that uses neighborhoods needs more
   * input than output to avoid introducing artificial boundary
   * conditions) or less input (for instance a magnify filter) will
   * have to override this method.  In doing so, it should call its
   * superclass' implementation as its first step. Note that imaging
   * filters operate differently than the classes to this point in the
   * class hierarchy.  Up till now, the base assumption has been that
   * the largest possible region will be requested of the input.
   *
   * This implementation of GenerateInputRequestedRegion() only
   * processes the inputs that are a subclass of the
   * ImageBase<InputImageDimension>.  If an input is another type of
   * DataObject (including an Image of a different dimension), they
   * are skipped by this method. The subclasses of ImageToImageFilter
   * are responsible for providing an implementation of
   * GenerateInputRequestedRegion() when there are multiple inputs of
   * different types.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion(),
   *     ImageSource::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Typedef for the region copier function object that converts an
   * input region to an output region. */
  typedef ImageToImageFilterDetail::ImageRegionCopier< itkGetStaticConstMacro(OutputImageDimension),
                                                       itkGetStaticConstMacro(InputImageDimension) >
  InputToOutputRegionCopierType;

  /** Typedef for the region copier function object that converts an
   * output region to an input region. */
  typedef ImageToImageFilterDetail::ImageRegionCopier< itkGetStaticConstMacro(InputImageDimension),
                                                       itkGetStaticConstMacro(OutputImageDimension) >
  OutputToInputRegionCopierType;

  /** This function calls the actual region copier to do the mapping
   * from output image space to input image space.  It uses a Function
   * object used for dispatching to various routines to copy an output
   * region (start index and size) to an input region.  For most
   * filters, this is a trivial copy because most filters require the
   * input dimension to match the output dimension.  However, some
   * filters like itk::ExtractImageFilter can support output images of
   * a lower dimension that the input.
   *
   * This function object can be used by GenerateOutputInformation()
   * to copy the input LargestPossibleRegion to the output
   * LargestPossibleRegion and can also be used in GenerateData or
   * ThreadedGenerateData() where a filter may need to map an
   * output region to an input region.
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
   * If a filter needs a different default behavior, it can override
   * this method. The ExtractImageFilter overrides this function
   * object so that if the input image is a higher dimension than the
   * output image, the filter can control "where" in the input image
   * the output subimage is extracted (as opposed to mapping to first
   * few dimensions of the input). */
  virtual void CallCopyOutputRegionToInputRegion(InputImageRegionType & destRegion,
                                                 const OutputImageRegionType & srcRegion);

  /** This function calls the actual region copier to do the mapping
   * from input image space to output image space.  It uses a Function
   * object used for dispatching to various routines to copy an input
   * region (start index and size) to an output region.  For most
   * filters, this is a trivial copy because most filters require the
   * input dimension to match the output dimension.  However, some
   * filters like itk::UnaryFunctorImageFilter can support output
   * images of a higher dimension that the input.
   *
   * This function object is used by the default implementation of
   * GenerateOutputInformation(). It can also be used in routines
   * like ThreadedGenerateData() where a filter may need to map an
   * input region to an output region.
   *
   * The default copier uses a "dispatch pattern" to call one of three
   * overloaded functions depending on whether the input and output
   * images are the same dimension, the input is a higher dimension
   * that the output, or the input is of a lower dimension than the
   * output. The use of an overloaded function is required for proper
   * compilation of the various cases.
   *
   * For the latter two cases, trivial implementations are used.  If
   * the input image is a higher dimension than the output, the first
   * portion of the input region is copied to the output region.  If
   * the input region is a lower dimension than the output, the input
   * region information is copied into the first portion of the output
   * region and the rest of the output region is set to zero.
   *
   * If a filter needs a different default behavior, it can override
   * this method. */
  virtual void CallCopyInputRegionToOutputRegion(OutputImageRegionType & destRegion,
                                                 const InputImageRegionType & srcRegion);

  /**
   * PushBackInput(), PushFronInput() in the public section force the
   * input to be the type expected by an ImageToImageFilter. However,
   * these methods end of "hiding" the versions from the superclass
   * (ProcessObject) whose arguments are DataObjects. Here, we re-expose
   * the versions from ProcessObject to avoid warnings about hiding
   * methods from the superclass.
   */
  void PushBackInput(const DataObject *input) ITK_OVERRIDE
  { Superclass::PushBackInput(input); }
  void PushFrontInput(const DataObject *input) ITK_OVERRIDE
  { Superclass::PushFrontInput(input); }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToImageFilter);
  /**
   *  Tolerances for checking whether input images are defined to
   *  occupy the same physical space.
   */
  double m_CoordinateTolerance;
  double m_DirectionTolerance;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageFilter.hxx"
#endif

#endif
