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
#ifndef itkResampleImageFilter_h
#define itkResampleImageFilter_h

#include "itkFixedArray.h"
#include "itkTransform.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkExtrapolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkSize.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkDataObjectDecorator.h"


namespace itk
{
/** \class ResampleImageFilter
 * \brief Resample an image via a coordinate transform
 *
 * ResampleImageFilter resamples an existing image through some coordinate
 * transform, interpolating via some image function.  The class is templated
 * over the types of the input and output images.
 *
 * Note that the choice of interpolator function can be important.
 * This function is set via SetInterpolator().  The default is
 * LinearInterpolateImageFunction<InputImageType,
 * TInterpolatorPrecisionType>, which
 * is reasonable for ordinary medical images.  However, some synthetic
 * images have pixels drawn from a finite prescribed set.  An example
 * would be a mask indicating the segmentation of a brain into a small
 * number of tissue types.  For such an image, one does not want to
 * interpolate between different pixel values, and so
 * NearestNeighborInterpolateImageFunction< InputImageType,
 * TCoordRep > would be a better choice.
 *
 * If an sample is taken from outside the image domain, the default behavior is
 * to use a default pixel value.  If different behavior is desired, an
 * extrapolator function can be set with SetExtrapolator().
 *
 * Output information (spacing, size and direction) for the output
 * image should be set. This information has the normal defaults of
 * unit spacing, zero origin and identity direction. Optionally, the
 * output information can be obtained from a reference image. If the
 * reference image is provided and UseReferenceImage is On, then the
 * spacing, origin and direction of the reference image will be used.
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
 * \warning For multithreading, the TransformPoint method of the
 * user-designated coordinate transform must be threadsafe.
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 *
 * \wiki
 * \wikiexample{SimpleOperations/TranslationTransform,Translate an image}
 * \wikiexample{ImageProcessing/Upsampling,Upsampling an image}
 * \wikiexample{ImageProcessing/ResampleImageFilter,Resample (stretch or compress) an image}
 * \endwiki
 */
template< typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType = double,
          typename TTransformPrecisionType = TInterpolatorPrecisionType>
class ITK_TEMPLATE_EXPORT ResampleImageFilter :
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ResampleImageFilter                             Self;
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
  itkTypeMacro(ResampleImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** base type for images of the current ImageDimension */
  typedef ImageBase< itkGetStaticConstMacro(ImageDimension) > ImageBaseType;

  /**
   *  Transform typedef.
   */
  typedef Transform< TTransformPrecisionType,
                     itkGetStaticConstMacro(ImageDimension),
                     itkGetStaticConstMacro(ImageDimension) >   TransformType;
  typedef typename TransformType::ConstPointer                  TransformPointerType;
  typedef DataObjectDecorator<TransformType>                    DecoratedTransformType;
  typedef typename DecoratedTransformType::Pointer              DecoratedTransformPointer;


  /** Interpolator typedef. */
  typedef InterpolateImageFunction< InputImageType,
                                    TInterpolatorPrecisionType >     InterpolatorType;
  typedef typename InterpolatorType::Pointer InterpolatorPointerType;

  typedef typename InterpolatorType::OutputType InterpolatorOutputType;

  typedef DefaultConvertPixelTraits< InterpolatorOutputType > InterpolatorConvertType;

  typedef typename InterpolatorConvertType::ComponentType ComponentType;

  typedef LinearInterpolateImageFunction< InputImageType,
                                          TInterpolatorPrecisionType >   LinearInterpolatorType;
  typedef typename LinearInterpolatorType::Pointer
  LinearInterpolatorPointerType;

  /** Extrapolator typedef. */
  typedef ExtrapolateImageFunction< InputImageType,
                                    TInterpolatorPrecisionType >     ExtrapolatorType;
  typedef typename ExtrapolatorType::Pointer ExtrapolatorPointerType;

  /** Image size typedef. */
  typedef Size< itkGetStaticConstMacro(ImageDimension) > SizeType;

  /** Image index typedef. */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image point typedef. */
  typedef typename InterpolatorType::PointType PointType;
  //typedef typename TOutputImage::PointType            PointType;

  /** Image pixel value typedef. */
  typedef typename TOutputImage::PixelType PixelType;
  typedef typename TInputImage::PixelType  InputPixelType;

  typedef DefaultConvertPixelTraits<PixelType> PixelConvertType;

  typedef typename PixelConvertType::ComponentType PixelComponentType;

  /** Input pixel continuous index typdef */
  typedef ContinuousIndex< TTransformPrecisionType, ImageDimension >
  ContinuousInputIndexType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Image spacing,origin and direction typedef */
  typedef typename TOutputImage::SpacingType   SpacingType;
  typedef typename TOutputImage::PointType     OriginPointType;
  typedef typename TOutputImage::DirectionType DirectionType;

  /** Typedef the reference image type to be the ImageBase of the OutputImageType */
  typedef ImageBase<ImageDimension> ReferenceImageBaseType;

  /** Get/Set the coordinate transformation.
   * Set the coordinate transform to use for resampling.  Note that this must
   * be in physical coordinates and it is the output-to-input transform, NOT
   * the input-to-output transform that you might naively expect.  By default
   * the filter uses an Identity transform. You must provide a different
   * transform here, before attempting to run the filter, if you do not want to
   * use the default Identity transform. */
   itkSetGetDecoratedObjectInputMacro(Transform, TransformType);

  /** Get/Set the interpolator function.  The default is
   * LinearInterpolateImageFunction<InputImageType,
   * TInterpolatorPrecisionType>. Some
   * other options are NearestNeighborInterpolateImageFunction
   * (useful for binary masks and other images with a small number of
   * possible pixel values), and BSplineInterpolateImageFunction
   * (which provides a higher order of interpolation).  */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Get/Set the extrapolator function.  The default behavior when sampling outside
   * of the input image is to use the DefaultPixelValue.  Some other options
   * include NearestNeighborExtrapolateImageFunction. */
  itkSetObjectMacro(Extrapolator, ExtrapolatorType);
  itkGetModifiableObjectMacro(Extrapolator, ExtrapolatorType);

  /** Get/Set the size of the output image. */
  itkSetMacro(Size, SizeType);
  itkGetConstReferenceMacro(Size, SizeType);

  /** Get/Set the pixel value when a transformed pixel is outside of the
   * image.  The default default pixel value is 0. */
  itkSetMacro(DefaultPixelValue, PixelType);
  itkGetConstReferenceMacro(DefaultPixelValue, PixelType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void SetOutputSpacing(const double *values);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, OriginPointType);
  virtual void SetOutputOrigin(const double *values);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, OriginPointType);

  /** Set the output direciton cosine matrix. */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Helper method to set the output parameters based on this image. */
  void SetOutputParametersFromImage(const ImageBaseType *image);

  /** Set the start index of the output largest possible region.
   * The default is an index of all zeros. */
  itkSetMacro(OutputStartIndex, IndexType);

  /** Get the start index of the output largest possible region. */
  itkGetConstReferenceMacro(OutputStartIndex, IndexType);

   /** Set a reference image to use to define the output information.
    *  By default, output information is specificed through the
    *  SetOutputSpacing, Origin, and Direction methods.  Alternatively,
    *  this method can be used to specify an image from which to
    *  copy the information. UseReferenceImageOn must be set to utilize the
    *  reference image. */
  itkSetInputMacro(ReferenceImage, ReferenceImageBaseType);

  /** Get the reference image that is defining the output information. */
  itkGetInputMacro(ReferenceImage, ReferenceImageBaseType);

  /** Turn on/off whether a specified reference image should be used to define
   *  the output information. */
  itkSetMacro(UseReferenceImage, bool);
  itkBooleanMacro(UseReferenceImage);
  itkGetConstMacro(UseReferenceImage, bool);

  /** ResampleImageFilter produces an image which is a different size
   * than its input.  As such, it needs to provide an implementation
   * for GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below. \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** ResampleImageFilter needs a different input requested region than
   * the output requested region.  As such, ResampleImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Set up state of filter before multi-threading.
   * InterpolatorType::SetInputImage is not thread-safe and hence
   * has to be set up before ThreadedGenerateData */
  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Set the state of the filter after multi-threading. */
  virtual void AfterThreadedGenerateData() ITK_OVERRIDE;

  /** Compute the Modified Time based on the changed components. */
  ModifiedTimeType GetMTime(void) const ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< PixelComponentType > ) );
  // End concept checking
#endif

protected:
  ResampleImageFilter();
  ~ResampleImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Override VeriyInputInformation() since this filter's inputs do
   * not need to occoupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  virtual void VerifyInputInformation() ITK_OVERRIDE { }

  /** ResampleImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior
   * to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  virtual void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                                    ThreadIdType threadId) ITK_OVERRIDE;

  /** Default implementation for resampling that works for any
   * transformation type. */
  virtual void NonlinearThreadedGenerateData(const OutputImageRegionType &
                                             outputRegionForThread,
                                             ThreadIdType threadId);

  /** Implementation for resampling that works for with linear
   *  transformation types.
   */
  virtual void LinearThreadedGenerateData(const OutputImageRegionType &
                                          outputRegionForThread,
                                          ThreadIdType threadId);

  /** Cast pixel from interpolator output to PixelType. */
  virtual PixelType CastPixelWithBoundsChecking( const InterpolatorOutputType value,
                                                 const ComponentType minComponent,
                                                 const ComponentType maxComponent) const;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ResampleImageFilter);

  SizeType                m_Size;         // Size of the output image
  InterpolatorPointerType m_Interpolator; // Image function for
                                          // interpolation
  ExtrapolatorPointerType m_Extrapolator; // Image function for
                                          // extrapolation
  PixelType m_DefaultPixelValue;          // default pixel value
                                          // if the point is
                                          // outside the image
  SpacingType     m_OutputSpacing;        // output image spacing
  OriginPointType m_OutputOrigin;         // output image origin
  DirectionType   m_OutputDirection;      // output image direction cosines
  IndexType       m_OutputStartIndex;     // output image start index
  bool            m_UseReferenceImage;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkResampleImageFilter.hxx"
#endif

#endif
