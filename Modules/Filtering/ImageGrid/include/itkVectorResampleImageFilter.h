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
#ifndef __itkVectorResampleImageFilter_h
#define __itkVectorResampleImageFilter_h

#include "itkTransform.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkVectorInterpolateImageFunction.h"
#include "itkSize.h"

namespace itk
{
/** \class VectorResampleImageFilter
 * \brief Resample an image via a coordinate transform
 *
 * VectorResampleImageFilter resamples an existing image through some coordinate
 * transform, interpolating via some image function.  The class is templated
 * over the types of the input and output images.
 *
 * Note that the choice of interpolator function can be important.
 * This function is set via SetInterpolator().  The default is
 * itk::VectorLinearInterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>, which
 * is reasonable for ordinary medical images.
 *
 * Since this filter produces an image which is a different size than
 * its input, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::GenerateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * ThreadedGenerateData() method for its implementation.
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 *
 * \wiki
 * \wikiexample{VectorImages/VectorResampleImageFilter,Translate a vector image}
 * \endwiki
 */
template< class TInputImage, class TOutputImage, class TInterpolatorPrecisionType = double >
class ITK_EXPORT VectorResampleImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef VectorResampleImageFilter                       Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  typedef TInputImage                           InputImageType;
  typedef TOutputImage                          OutputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorResampleImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Transform typedef.
   *
   * \todo Check that input and output images have the same number of
   * dimensions; this is required by the current implementation of
   * AffineTransform. */
  typedef Transform< TInterpolatorPrecisionType, itkGetStaticConstMacro(ImageDimension),
                     itkGetStaticConstMacro(ImageDimension) > TransformType;
  typedef typename TransformType::ConstPointer
  TransformPointerType;

  /** Interpolator typedef. */
  typedef VectorInterpolateImageFunction< InputImageType, TInterpolatorPrecisionType > InterpolatorType;
  typedef typename InterpolatorType::Pointer                                           InterpolatorPointerType;

  /** Image size typedef. */
  typedef Size< itkGetStaticConstMacro(ImageDimension) > SizeType;

  /** Image index typedef. */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image point typedef. */
  typedef typename InterpolatorType::PointType PointType;

  /** Image pixel value typedef. */
  typedef typename TOutputImage::PixelType PixelType;
  typedef typename PixelType::ValueType    PixelComponentType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Image spacing,origin and direction typedef */
  typedef typename TOutputImage::SpacingType   SpacingType;
  typedef typename TOutputImage::PointType     OriginPointType;
  typedef typename TOutputImage::DirectionType DirectionType;

  /** Get/Set the coordinate transformation.
   * Set the coordinate transform to use for resampling.  Note that this
   * must be in index coordinates and is the output-to-input transform,
   * NOT the input-to-output transform that you might naively expect.
   * The default is itk::AffineTransform<TInterpolatorPrecisionType, ImageDimension>. */
  itkSetConstObjectMacro(Transform, TransformType);

  /** Set the interpolator function.  The default is
   * itk::VectorLinearInterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>.  */
  itkSetObjectMacro(Interpolator, InterpolatorType);

  /** Get a pointer to the interpolator function. */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set the size of the output image. */
  itkSetMacro(Size, SizeType);

  /** Get the size of the output image. */
  itkGetConstReferenceMacro(Size, SizeType);

  /** Set the pixel value when a transformed pixel is outside of the
   * image.  The default default pixel value is 0. */
  itkSetMacro(DefaultPixelValue, PixelType);

  /** Get the pixel value when a transformed pixel is outside of the image */
  itkGetConstMacro(DefaultPixelValue, PixelType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void SetOutputSpacing(const double *values);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, PointType);
  virtual void SetOutputOrigin(const double *values);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, PointType);

  /** Set the output direciton cosine matrix. */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Set the start index of the output largest possible region.
   * The default is an index of all zeros. */
  itkSetMacro(OutputStartIndex, IndexType);

  /** Get the start index of the output largest possible region. */
  itkGetConstReferenceMacro(OutputStartIndex, IndexType);

  /** VectorResampleImageFilter produces an image which is a different size
   * than its input.  As such, it needs to provide an implementation
   * for GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below. \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation();

  /** VectorResampleImageFilter needs a different input requested region than
   * the output requested region.  As such, VectorResampleImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

  /** This method is used to set the state of the filter before
   * multi-threading. */
  virtual void BeforeThreadedGenerateData();

  /** This method is used to set the state of the filter after
   * multi-threading. */
  virtual void AfterThreadedGenerateData();

  /** Method Compute the Modified Time based on changed to the components. */
  ModifiedTimeType GetMTime(void) const;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType::ValueType > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< PixelComponentType > ) );
  /** End concept checking */
#endif

protected:
  VectorResampleImageFilter();
  ~VectorResampleImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** VectorResampleImageFilter can be implemented as a multithreaded filter.  Therefore,
   * this implementation provides a ThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId);

private:
  VectorResampleImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);            //purposely not implemented

  SizeType                m_Size;       // Size of the output image
  TransformPointerType    m_Transform;  // Coordinate transform to use
  InterpolatorPointerType m_Interpolator;
  // Image function for interpolation
  PixelType m_DefaultPixelValue;
  // default pixel value if the point
  // is outside the image
  SpacingType   m_OutputSpacing;           // output image spacing
  PointType     m_OutputOrigin;            // output image origin
  DirectionType m_OutputDirection;         // output image direction cosines
  IndexType     m_OutputStartIndex;        // output start index
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorResampleImageFilter.hxx"
#endif

#endif
