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
#ifndef itkMaskedFFTNormalizedCorrelationImageFilter_h
#define itkMaskedFFTNormalizedCorrelationImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class MaskedFFTNormalizedCorrelationImageFilter
 * \brief Calculate masked normalized cross correlation using FFTs.
 *
 * This filter calculates the masked normalized cross correlation (NCC) of two
 * images under masks using FFTs instead of spatial correlation.
 * It is much faster than spatial correlation
 * for reasonably large structuring elements.
 * This filter is not equivalent to simply masking the images first and then
 * correlating them; the latter approach yields incorrect results because the
 * zeros in the images still affect the metric in the correlation process.
 * This filter implements the masked NCC correctly so that the masked-out regions are completely ignored.
 * The fundamental difference is described in detail in the references below.
 * If the masks are set to images of all ones, the result of this filter is
 * the same as standard NCC.
 *
 * Inputs:
 * Two images are required as inputs, fixedImage and movingImage,
 * and two are optional, fixedMask and movingMask.
 * In the context of correlation, inputs are often defined as: "image"
 * and "template".  In this filter, the fixedImage plays the role of the
 * image, and the movingImage plays the role of the template.
 * However, this filter is capable of correlating any two images and
 * is not restricted to small movingImages (templates).
 * In the fixedMask and movingMask, non-zero positive values
 * indicate locations of useful information in the corresponding image,
 * whereas zero and negative values indicate locations that should be
 * masked out (ignored).
 * Internally, the masks are converted to have values of only 0 and 1.
 * For each optional mask that is not set, the filter
 * internally creates an image of ones, which is equivalent to not
 * masking the image.  Thus, if both masks are not set, the result
 * will be equivalent to unmasked NCC.
 * For example, if only a mask for the fixed image is needed, the
 * movingMask can either not be set or can be set to an image of ones.
 *
 * Optional parameters:
 * The RequiredNumberOfOverlappingPixels enables the user to specify the minimum number of voxels
 * of the two masks that must overlap; any location in the correlation map that results
 * from fewer than this number of voxels will be set to zero.
 * Larger values zero-out pixels on a larger border around the correlation image.
 * Thus, larger values remove less stable computations but also limit the capture range.
 * If RequiredNumberOfOverlappingPixels is set to 0, the default, no zeroing will take place.
 *
 * The RequiredFractionOfOverlappingPixels enables the user to specify a fraction of the maximum
 * number of overlapping pixels that need to overlap; any location in the correlation map that
 * results from fewer than the product of this fraction and the internally computed maximum
 * number of overlapping pixels will be set to zero.
 * The value ranges between 0.0 and 1.0.
 * This is very useful when the user does does not know beforehand the maximum number of pixels
 * of the masks that will overlap.
 * For example, when the masks have strange shapes, it is difficult to predict
 * how the correlation of the masks will interact and what the maximum overlap will be.
 * It is also useful when the mask shapes or sizes change because it is relative to the internally
 * computed maximum of the overlap.
 * Larger values zero-out pixels on a larger border around the correlation image.
 * Thus, larger values remove less stable computations but also limit the capture range.
 * Experiments have shown that a value between 0.1 and 0.6 works well for images with significant
 * overlap and between 0.05 and 0.1 for images with little overlap (such as in stitching applications).
 * If RequiredFractionOfOverlappingPixels is set to 0, the default, no zeroing will take place.
 *
 * The user can either specify RequiredNumberOfOverlappingPixels or RequiredFractionOfOverlappingPixels
 * (or both or none).
 * Internally, the number of required pixels resulting from both of these methods is calculated
 * and the one that gives the largest number of pixels is chosen.
 * Since these both default to 0, if a user only sets one, the other is ignored.
 *
 * Image size:
 * fixedImage and movingImage need not be the same size, but fixedMask
 * must be the same size as fixedImage, and movingMask must be the same
 * size as movingImage.
 * Furthermore, whereas some algorithms require that the "template"
 * be smaller than the "image" because of errors in the regions where
 * the two are not fully overlapping, this filter has no such restriction.
 *
 * Image spacing:
 * Since the computations are done in the pixel domain, all input
 * images must have the same spacing.
 *
 * Outputs;
 * The output is an image of RealPixelType that is the masked NCC of
 * the two images and its values range from -1.0 to 1.0.
 * The size of this NCC image is, by definition,
 * size(fixedImage) + size(movingImage) - 1.
 *
 * Example filter usage:
 * \code
 * typedef itk::MaskedFFTNormalizedCorrelationImageFilter< ShortImageType, DoubleImageType > FilterType;
 * FilterType::Pointer filter = FilterType::New();
 * filter->SetFixedImage( fixedImage );
 * filter->SetMovingImage( movingImage );
 * filter->SetFixedImageMask( fixedMask );
 * filter->SetMovingImageMask( movingMask );
 * filter->SetRequiredNumberOfOverlappingPixels(20);
 * filter->Update();
 * \endcode
 *
 * \warning The pixel type of the output image must be of real type
 * (float or double). ConceptChecking is used to enforce the output pixel
 * type. You will get a compilation error if the pixel type of the
 * output image is not float or double.
 *
 * References:
 * 1) D. Padfield. "Masked object registration in the Fourier domain."
 * Transactions on Image Processing.
 * 2) D. Padfield. "Masked FFT registration". In Proc. Computer
 * Vision and Pattern Recognition, 2010.
 *
 * \author: Dirk Padfield, GE Global Research, padfield\@research.ge.com
 * \ingroup ITKConvolution
 */

template <typename TInputImage, typename TOutputImage, typename TMaskImage=TInputImage >
class ITK_TEMPLATE_EXPORT MaskedFFTNormalizedCorrelationImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef MaskedFFTNormalizedCorrelationImageFilter                     Self;
  typedef ImageToImageFilter < TInputImage, TOutputImage >              Superclass;
  typedef SmartPointer<Self>                                            Pointer;
  typedef SmartPointer<const Self>                                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MaskedFFTNormalizedCorrelationImageFilter, ImageToImageFilter);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Extract some information from the image types. */
  typedef TInputImage                               InputImageType;
  typedef typename InputImageType::RegionType       InputRegionType;
  typedef typename InputImageType::Pointer          InputImagePointer;
  typedef typename InputImageType::ConstPointer     InputImageConstPointer;
  typedef typename InputImageType::SizeType         InputSizeType;
  typedef typename itk::SizeValueType               SizeValueType;

  typedef TOutputImage                              OutputImageType;
  typedef typename OutputImageType::Pointer         OutputImagePointer;
  typedef typename OutputImageType::PixelType       OutputPixelType;

  typedef OutputPixelType                           RealPixelType;
  typedef Image< RealPixelType, ImageDimension>     RealImageType;
  typedef typename RealImageType::Pointer           RealImagePointer;
  typedef typename RealImageType::IndexType         RealIndexType;
  typedef typename RealImageType::SizeType          RealSizeType;
  typedef typename RealImageType::RegionType        RealRegionType;
  typedef typename RealImageType::PointType         RealPointType;

  typedef TMaskImage                                MaskImageType;
  typedef typename MaskImageType::Pointer           MaskImagePointer;

  typedef Image< std::complex<RealPixelType>, ImageDimension >  FFTImageType;
  typedef typename FFTImageType::Pointer                        FFTImagePointer;

  /** Set and get the fixed image */
  itkSetInputMacro(FixedImage, InputImageType);
  itkGetInputMacro(FixedImage, InputImageType);

  /** Set and get the moving image */
  itkSetInputMacro(MovingImage, InputImageType);
  itkGetInputMacro(MovingImage, InputImageType);

  /** Set and get the fixed mask */
  itkSetInputMacro(FixedImageMask, MaskImageType);
  itkGetInputMacro(FixedImageMask, MaskImageType);

  /** Set and get the moving mask */
  itkSetInputMacro(MovingImageMask, MaskImageType);
  itkGetInputMacro(MovingImageMask, MaskImageType);

  /** Set and get the required number of overlapping pixels */
  itkSetMacro(RequiredNumberOfOverlappingPixels,SizeValueType);
  itkGetMacro(RequiredNumberOfOverlappingPixels,SizeValueType);

  /** Set and get the required fraction of overlapping pixels */
  itkGetMacro(RequiredFractionOfOverlappingPixels,RealPixelType);
  itkSetClampMacro(RequiredFractionOfOverlappingPixels, RealPixelType, 0.0f, 1.0f);

  /** Get the maximum number of overlapping pixels. */
  itkGetMacro(MaximumNumberOfOverlappingPixels,SizeValueType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputPixelTypeIsFloatingPointCheck,
                   ( Concept::IsFloatingPoint< OutputPixelType > ) );
  // End concept checking
#endif

protected:
  MaskedFFTNormalizedCorrelationImageFilter():m_TotalForwardAndInverseFFTs(12)
  {
    // #0 "FixedImage" required
    Self::SetPrimaryInputName("FixedImage");

    // #1 "MaskImage" required
    Self::AddRequiredInputName("MovingImage", 1);

    // #2 "FixedImageMask" optional
    Self::AddOptionalInputName("FixedImageMask", 2);

    // #3 "MaskImageMask" optional
    Self::AddOptionalInputName("MovingImageMask", 3);

    m_RequiredNumberOfOverlappingPixels = 0;
    m_RequiredFractionOfOverlappingPixels = 0;
    m_MaximumNumberOfOverlappingPixels = 0;
    m_AccumulatedProgress = 0.0;
  }
  virtual ~MaskedFFTNormalizedCorrelationImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /** Overlap the VerifyInputInformation method */
  void VerifyInputInformation() ITK_OVERRIDE;

  /** Standard pipeline method.*/
  void GenerateData() ITK_OVERRIDE;

  /** This filter needs a different input requested region than the output
   * requested region.  As such, it needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Since the output of this filter is a different
   * size than the input, it must provide an implementation of
   * GenerateOutputInformation.
   * \sa ProcessObject::GenerateOutputRequestedRegion() */
  void GenerateOutputInformation() ITK_OVERRIDE;

  void EnlargeOutputRequestedRegion( DataObject *output ) ITK_OVERRIDE;

  typename TMaskImage::Pointer PreProcessMask( const InputImageType * inputImage, const MaskImageType * inputMask );

  typename TInputImage::Pointer PreProcessImage( const InputImageType * inputImage, const MaskImageType * inputMask );

  template< typename LocalInputImageType >
  typename LocalInputImageType::Pointer RotateImage( LocalInputImageType * inputImage );

  template< typename LocalInputImageType, typename LocalOutputImageType >
  typename LocalOutputImageType::Pointer CalculateForwardFFT( LocalInputImageType * inputImage, InputSizeType & FFTImageSize );

  template< typename LocalInputImageType, typename LocalOutputImageType >
  typename LocalOutputImageType::Pointer CalculateInverseFFT( LocalInputImageType * inputImage, RealSizeType & combinedImageSize );

  // Helper math methods.
  template< typename LocalInputImageType, typename LocalOutputImageType >
  typename LocalOutputImageType::Pointer ElementProduct( LocalInputImageType * inputImage1, LocalInputImageType * inputImage2 );

  template< typename LocalInputImageType >
  typename LocalInputImageType::Pointer ElementQuotient( LocalInputImageType * inputImage1, LocalInputImageType * inputImage2 );

  template< typename LocalInputImageType >
  typename LocalInputImageType::Pointer ElementSubtraction( LocalInputImageType * inputImage1, LocalInputImageType * inputImage2 );

  template< typename LocalInputImageType >
  typename LocalInputImageType::Pointer ElementPositive( LocalInputImageType * inputImage );

  template< typename LocalInputImageType, typename LocalOutputImageType >
  typename LocalOutputImageType::Pointer ElementRound( LocalInputImageType * inputImage );

  // This function factorizes the image size uses factors of 2, 3, and
  // 5.  After this factorization, if there are any remaining values,
  // the function returns this value.
  int FactorizeNumber( int n );

  // Find the closest valid dimension above the desired dimension.  This
  // will be a combination of 2s, 3s, and 5s.
  int FindClosestValidDimension( int n );

  template< typename LocalInputImageType >
  double CalculatePrecisionTolerance( LocalInputImageType * inputImage );

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaskedFFTNormalizedCorrelationImageFilter);

  /** Larger values zero-out pixels on a larger border around the correlation image.
   * Thus, larger values remove less stable computations but also limit the capture range.
   * The default is set to 0. */
  SizeValueType m_RequiredNumberOfOverlappingPixels;
  /** Similar to m_RequiredNumberOfOverlappingPixels except that the m_RequiredFractionOfOverlappingPixels is multiplied by the
   * m_MaximumNumberOfOverlappingPixels to determine the requiredNumberOfOverlappingPixels.
   * The default is 0. */
  RealPixelType m_RequiredFractionOfOverlappingPixels;
  /** This is computed internally */
  SizeValueType m_MaximumNumberOfOverlappingPixels;

  /** This is used for the progress reporter */
  const unsigned int m_TotalForwardAndInverseFFTs;
  /** The total accumulated progress */
  float m_AccumulatedProgress;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMaskedFFTNormalizedCorrelationImageFilter.hxx"
#endif

#endif
