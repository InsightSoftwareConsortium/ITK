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
#ifndef itkAccumulateImageFilter_h
#define itkAccumulateImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class AccumulateImageFilter
 * \brief Implements an accumulation of an image along a selected direction.
 *
 *    This class accumulates an image along a dimension and reduce the size
 * of this dimension to 1. The dimension being accumulated is set by
 * AccumulateDimension.
 *
 *   Each pixel is the cumulative sum of the pixels along the collapsed
 * dimension and reduce the size of the accumulated dimension to 1 (only
 * on the accumulated).
 *
 *   The dimensions of the InputImage and the OutputImage must be the same.
 *
 *
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 *
 * \author Emiliano Beronich
 *
 * This filter was contributed by Emiliano Beronich
 *
 * \sa GetAverageSliceImageFilter
 *
 * \ingroup   IntensityImageFilters     SingleThreaded
 * \ingroup ITKImageStatistics
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT AccumulateImageFilter:public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef AccumulateImageFilter                           Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AccumulateImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef typename    InputImageType::Pointer    InputImagePointer;
  typedef typename    InputImageType::RegionType InputImageRegionType;
  typedef typename    InputImageType::PixelType  InputImagePixelType;

  typedef TOutputImage                             OutputImageType;
  typedef typename     OutputImageType::Pointer    OutputImagePointer;
  typedef typename     OutputImageType::RegionType OutputImageRegionType;
  typedef typename     OutputImageType::PixelType  OutputImagePixelType;

  /** ImageDimension enumeration */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Input and output images must be the same dimension, or the output's
      dimension must be one less than that of the input. */
#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( ImageDimensionCheck,
                   ( Concept::SameDimension< itkGetStaticConstMacro(InputImageDimension),
                                             itkGetStaticConstMacro(OutputImageDimension) > ) );
  // End concept checking
#endif

  /** Set the direction in which to accumulate the data.  It must be
   * set before the update of the filter. Defaults to the last
   * dimension. */
  itkGetConstMacro(AccumulateDimension, unsigned int);
  itkSetMacro(AccumulateDimension, unsigned int);

  /** Perform a division by the size of the accumulated dimension
   * after the accumulation is done. If true, the output image is the
   * average of the accumulated dimension, if false the output is the
   * sum of the pixels along the selected direction.  The default
   * value is false. */
  itkSetMacro(Average, bool);
  itkGetConstMacro(Average, bool);
  itkBooleanMacro(Average);

protected:
  AccumulateImageFilter();
  virtual ~AccumulateImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Apply changes to the output image information. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** Apply changes to the input image requested region. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** This method implements the actual accumulation of the image.
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void GenerateData(void) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AccumulateImageFilter);

  unsigned int m_AccumulateDimension;
  bool         m_Average;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAccumulateImageFilter.hxx"
#endif

#endif
