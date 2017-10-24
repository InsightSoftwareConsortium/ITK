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
#ifndef itkVectorExpandImageFilter_h
#define itkVectorExpandImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{
/** \class VectorExpandImageFilter
 * \brief Expand the size of a vector image by an integer factor in each
 * dimension.
 *
 * VectorExpandImageFilter increases the size of an image by an integer
 * factor in each dimension using a interpolation method.
 * The output image size in each dimension is given by:
 *
 * OutputSize[j] = InputSize[j] * ExpandFactors[j]
 *
 * The output values are obtained by interpolating the input image.
 * The default interpolation type used is the
 * VectorLinearInterpolateImageFunction.
 * The user can specified a particular interpolation function via
 * SetInterpolator(). Note that the input interpolator must derive
 * from base class VectorInterpolateImageFunction.
 *
 * This filter will produce an output with different pixel spacing
 * that its input image such that:
 *
 * OutputSpacing[j] = InputSpacing[j] / ExpandFactors[j]
 *
 * The filter is templated over the input image type and the output
 * image type.
 *
 * This filter is implemented as a multithreaded filter and supports
 * streaming.
 *
 * \warning This filter only works for image with pixel types
 * base on itk::Vectors. For scalar pixel images use
 * ExpandImageFilter.
 *
 * This filter assumes that the input and output image has the same
 * number of dimensions, and that the input and output pixel types
 * have the same vector dimension.
 *
 * \sa Vector
 * \sa VectorInterpolateImageFunction
 * \sa VectorLinearInterpolationImageFunction
 *
 * \sa ExpandImageFilter
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT VectorExpandImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef VectorExpandImageFilter                         Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef to describe the output image region type. */
  typedef typename TInputImage::Pointer     InputImagePointer;
  typedef typename TOutputImage::Pointer    OutputImagePointer;
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorExpandImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Inherit some types from superclass */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  /** Input/output vector types. */
  typedef typename OutputImageType::PixelType OutputPixelType;
  typedef typename OutputPixelType::ValueType OutputValueType;
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef typename InputPixelType::ValueType  InputValueType;

  /** Determine the vector dimension. */
  enum { VectorDimension = InputPixelType::Dimension };

  /** The type of the expand factors representation */
  typedef float                                           ExpandFactorsType;
  typedef FixedArray< ExpandFactorsType, ImageDimension > ExpandFactorsArrayType;

  /** Typedef support for the interpolation function */
  typedef double                                                               CoordRepType;
  typedef VectorInterpolateImageFunction< InputImageType, CoordRepType >       InterpolatorType;
  typedef typename InterpolatorType::Pointer                                   InterpolatorPointer;
  typedef VectorLinearInterpolateImageFunction< InputImageType, CoordRepType > DefaultInterpolatorType;

  /** Get/Set the interpolator function. */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set the expand factors. Values are clamped to
   * a minimum value of 1. Default is 1 for all dimensions. */
  itkSetMacro(ExpandFactors, ExpandFactorsArrayType);
  virtual void SetExpandFactors(const float factor);
  itkSetVectorMacro(ExpandFactors, const unsigned int, ImageDimension);

  /** Get the expand factors. */
  itkGetConstReferenceMacro(ExpandFactors, ExpandFactorsArrayType);

//TEST_RMV20100728  /** Set the edge padding value. The default is a vector of
// zero. */
//TEST_RMV20100728  virtual void SetEdgePaddingValue( const OutputPixelType&
// value );
//TEST_RMV20100728
//TEST_RMV20100728  /** Get the edge padding value. */
//TEST_RMV20100728  virtual const OutputPixelType& GetEdgePaddingValue()
//TEST_RMV20100728    { return m_EdgePaddingValue; }

  /** VectorExpandImageFilter produces an image which is a different
   * resolution and with a different pixel spacing than its input image.  As
   * such, VectorExpandImageFilter needs to provide an implementation for
   * UpdateOutputInformation() in order to inform the pipeline execution
   * model.  The original documentation of this method is below.  \sa
   * ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** VectorExpandImageFilter needs a smaller input requested region than the
   * output requested region.  As such, ShrinkImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.  \sa
   * ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputValueType > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< OutputValueType > ) );
  // End concept checking
#endif

protected:

  VectorExpandImageFilter();
  ~VectorExpandImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** VectorExpandImageFilter is implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread" \sa ImageToImageFilter::ThreadedGenerateData(),
   * ImageToImageFilter::GenerateData() */
  virtual
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  /** This method is used to set the state of the filter before
   * multi-threading. */
  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorExpandImageFilter);

  ExpandFactorsArrayType   m_ExpandFactors;
  InterpolatorPointer      m_Interpolator;
//TEST_RMV20100728 * \warning: The following is valid only when the flag
//TEST_RMV20100728 * ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY is ON
//TEST_RMV20100728 * The output image will not contain any padding, and
// therefore the
//TEST_RMV20100728 * EdgePaddingValue will not be used.
//TEST_RMV20100728 *
//TEST_RMV20100728  OutputPixelType        m_EdgePaddingValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorExpandImageFilter.hxx"
#endif

#endif
