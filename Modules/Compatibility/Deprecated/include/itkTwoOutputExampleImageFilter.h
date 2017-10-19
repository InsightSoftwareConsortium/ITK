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
#ifndef itkTwoOutputExampleImageFilter_h
#define itkTwoOutputExampleImageFilter_h
#if !defined( ITK_LEGACY_REMOVE )

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class TwoOutputExampleImageFilter
 * \brief Example of a filter that produce two outputs.
 *
 * TwoOutputExampleImageFilter sets image values to a user-specified "outside"
 * value (by default, "black") if the image values are below, above, or
 * between simple threshold values. The filter can produce two outputs,
 * one the inverse of the other. (GetOutput() returns an image whose
 * pixels satisfy the threshold values and are passed to the output
 * unchanged (and those that don't are marked with the outside user-value);
 * GetInverseOutput() returns an image in which pixels satisfying the
 * threshold are marked "outside", and the other pixel values are passed
 * through.)
 *
 * The pixels must support the operators >= and <=.
 *
 * \deprecated
 * \ingroup ITKDeprecated
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT TwoOutputExampleImageFilter:
  public ImageToImageFilter< TImage, TImage >
{
public:
  /** Standard class typedefs. */
  typedef TwoOutputExampleImageFilter          Self;
  typedef ImageToImageFilter< TImage, TImage > Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef to describe the type of pixel. */
  typedef typename TImage::PixelType PixelType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TwoOutputExampleImageFilter, ImageToImageFilter);

  /** Set the "outside" pixel value. The default value
   * NumericTraits<PixelType>::ZeroValue(). */
  itkSetMacro(OutsideValue, PixelType);

  /** Get the "outside" pixel value. */
  itkGetConstMacro(OutsideValue, PixelType);

  /** The values greater than or equal to the value are set to OutsideValue. */
  void ThresholdAbove(PixelType & thresh);

  /** The values less than or equal to the value are set to OutsideValue. */
  void ThresholdBelow(PixelType & thresh);

  /** The values outside the range are set to OutsideValue. */
  void ThresholdOutside(PixelType & lower, PixelType & upper);

  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;

  /** Some typedefs to handle the second output. */
  typedef TImage                               OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType  OutputImagePixelType;

  /** Get the image output of this process object.  */
  OutputImagePointer GetInverseOutput()
  { return static_cast< TImage * >( this->ProcessObject::GetOutput(1) ); }

  /** Set the image output of this process object.  */
  void SetInverseOutput(OutputImageType *output)
  { this->SetNthOutput(1, output); }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( ComparableCheck,
                   ( Concept::Comparable< PixelType > ) );
  itkConceptMacro( OStreamWritableCheck,
                   ( Concept::OStreamWritable< PixelType > ) );
  // Begin concept checking
#endif

protected:
  TwoOutputExampleImageFilter();
  ~TwoOutputExampleImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** TwoOutputExampleImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TwoOutputExampleImageFilter);

  PixelType m_OutsideValue;
  PixelType m_Lower;
  PixelType m_Upper;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTwoOutputExampleImageFilter.hxx"
#endif

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
