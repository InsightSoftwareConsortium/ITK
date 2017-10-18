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
#ifndef itkShiftScaleImageFilter_h
#define itkShiftScaleImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkArray.h"

namespace itk
{
/** \class ShiftScaleImageFilter
 * \brief Shift and scale the pixels in an image.
 *
 * ShiftScaleImageFilter shifts the input pixel by Shift (default 0.0)
 * and then scales the pixel by Scale (default 1.0). All computattions
 * are performed in the precision of the input pixel's RealType. Before
 * assigning the computed value to the output pixel, the value is clamped
 * at the NonpositiveMin and max of the pixel type.
 * \ingroup IntensityImageFilters
 *
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ShiftScaleImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ShiftScaleImageFilter                           Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef to describe the output and input image region types. */
  typedef typename TInputImage::RegionType  InputImageRegionType;
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Typedef to describe the pointer to the input/output. */
  typedef typename TInputImage::Pointer  InputImagePointer;
  typedef typename TOutputImage::Pointer OutputImagePointer;

  /** Typedef to describe the type of pixel. */
  typedef typename TInputImage::PixelType  InputImagePixelType;
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename TInputImage::IndexType   InputImageIndexType;
  typedef typename TInputImage::SizeType    InputImageSizeType;
  typedef typename TInputImage::OffsetType  InputImageOffsetType;
  typedef typename TOutputImage::IndexType  OutputImageIndexType;
  typedef typename TOutputImage::SizeType   OutputImageSizeType;
  typedef typename TOutputImage::OffsetType OutputImageOffsetType;

  /** Type to use form computations. */
  typedef typename NumericTraits< OutputImagePixelType >::RealType RealType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShiftScaleImageFilter, ImageToImageFilter);

  /** Set/Get the amount to Shift each Pixel. The shift is followed by a Scale.
    */
  itkSetMacro(Shift, RealType);
  itkGetConstMacro(Shift, RealType);

  /** Set/Get the amount to Scale each Pixel. The Scale is applied after the
    Shift. */
  itkSetMacro(Scale, RealType);
  itkGetConstMacro(Scale, RealType);

  /** Get the number of pixels that underflowed and overflowed. */
  itkGetConstMacro(UnderflowCount, long);
  itkGetConstMacro(OverflowCount, long);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< OutputImagePixelType > ) );
  itkConceptMacro( InputPlusRealTypeCheck,
                   ( Concept::AdditiveOperators< InputImagePixelType, RealType, RealType > ) );
  itkConceptMacro( RealTypeMultiplyOperatorCheck,
                   ( Concept::MultiplyOperator< RealType > ) );
  // End concept checking
#endif

protected:
  ShiftScaleImageFilter();
  ~ShiftScaleImageFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Tally accumulated in threads. */
  void AfterThreadedGenerateData() ITK_OVERRIDE;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const OutputImageRegionType &
                             outputRegionForThread,
                             ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShiftScaleImageFilter);

  RealType m_Shift;
  RealType m_Scale;

  long m_UnderflowCount;
  long m_OverflowCount;

  Array< long > m_ThreadUnderflow;
  Array< long > m_ThreadOverflow;

  const TInputImage *m_InputImage;
  TOutputImage      *m_OutputImage;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShiftScaleImageFilter.hxx"
#endif

#endif
