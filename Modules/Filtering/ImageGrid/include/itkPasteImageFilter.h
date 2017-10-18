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
#ifndef itkPasteImageFilter_h
#define itkPasteImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkSmartPointer.h"

namespace itk
{
/** \class PasteImageFilter
 * \brief Paste an image into another image
 *
 * PasteImageFilter allows you to take a section of one image and
 * paste into another image.  The SetDestinationIndex() method
 * prescribes where in the first input to start pasting data from the
 * second input.  The SetSourceRegion method prescribes the section of
 * the second image to paste into the first. If the output requested
 * region does not include the SourceRegion after it has been
 * repositioned to DestinationIndex, then the output will just be
 * a copy of the input.
 *
 * The two inputs and output image will have the same pixel type.
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 *
 * \wiki
 * \wikiexample{ImageProcessing/PasteImageFilter,Paste a part of one image into another image}
 * \endwiki
 */
template< typename TInputImage, typename TSourceImage = TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT PasteImageFilter:
  public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef PasteImageFilter                                Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PasteImageFilter, InPlaceImageFilter);

  /** Typedefs from Superclass */
  typedef typename Superclass::InputImagePointer  InputImagePointer;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;

  /** Typedef to describe the output and input image region types. */
  typedef TInputImage                          InputImageType;
  typedef TOutputImage                         OutputImageType;
  typedef TSourceImage                         SourceImageType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename InputImageType::RegionType  InputImageRegionType;
  typedef typename SourceImageType::RegionType SourceImageRegionType;

  typedef typename SourceImageType::Pointer      SourceImagePointer;
  typedef typename SourceImageType::ConstPointer SourceImageConstPointer;

  /** Typedef to describe the type of pixel. */
  typedef typename OutputImageType::PixelType OutputImagePixelType;
  typedef typename InputImageType::PixelType  InputImagePixelType;
  typedef typename SourceImageType::PixelType SourceImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename OutputImageType::IndexType OutputImageIndexType;
  typedef typename OutputImageType::SizeType  OutputImageSizeType;
  typedef typename InputImageType::IndexType  InputImageIndexType;
  typedef typename InputImageType::SizeType   InputImageSizeType;
  typedef typename SourceImageType::IndexType SourceImageIndexType;
  typedef typename SourceImageType::SizeType  SourceImageSizeType;

  /** ImageDimension enumeration */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      OutputImageType::ImageDimension);
  itkStaticConstMacro(SourceImageDimension, unsigned int,
                      SourceImageType::ImageDimension);

  /** Set/Get the destination index (where in the first input the second
   * input will be pasted. */
  itkSetMacro(DestinationIndex, InputImageIndexType);
  itkGetConstMacro(DestinationIndex, InputImageIndexType);

  /** Set/Get the source region (what part of the second input will be
   * pasted. */
  itkSetMacro(SourceRegion, SourceImageRegionType);
  itkGetConstMacro(SourceRegion, SourceImageRegionType);

  /** Set/Get the "destination" image.  This is the image that will be
   * obscured by the paste operation. */
  void SetDestinationImage(const InputImageType *dest);

  const InputImageType * GetDestinationImage() const;

  /** Set/Get the "source" image.  This is the image that will be
   * pasted over the destination image. */
  void SetSourceImage(const SourceImageType *src);

  const SourceImageType * GetSourceImage() const;

  /** PasteImageFilter needs to set the input requested regions for its
   * inputs.  The first input's requested region will be set to match
   * the output requested region.  The second input's requested region
   * will be set to the value of the m_SourceRegion ivar.  Note that
   * if the output requested region is a portion of the image that
   * is outside the DestinationIndex + size of the source region,
   * then the first input is copied to the output.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;


  /** Override VeriyInputInformation() since this filter's inputs do
   * not need to occoupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  virtual void VerifyInputInformation() ITK_OVERRIDE {}

protected:
  PasteImageFilter();
  ~PasteImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** PasteImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData(). ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  SourceImageRegionType m_SourceRegion;

  InputImageIndexType m_DestinationIndex;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PasteImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPasteImageFilter.hxx"
#endif

#endif
