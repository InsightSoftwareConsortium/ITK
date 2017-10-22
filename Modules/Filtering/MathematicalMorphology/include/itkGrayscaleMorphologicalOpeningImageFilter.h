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
#ifndef itkGrayscaleMorphologicalOpeningImageFilter_h
#define itkGrayscaleMorphologicalOpeningImageFilter_h

#include "itkKernelImageFilter.h"
#include "itkMovingHistogramDilateImageFilter.h"
#include "itkMovingHistogramErodeImageFilter.h"
#include "itkBasicDilateImageFilter.h"
#include "itkBasicErodeImageFilter.h"
#include "itkAnchorOpenImageFilter.h"
#include "itkVanHerkGilWermanErodeImageFilter.h"
#include "itkVanHerkGilWermanDilateImageFilter.h"
#include "itkConstantBoundaryCondition.h"
#include "itkNeighborhood.h"

namespace itk
{
/**
 * \class GrayscaleMorphologicalOpeningImageFilter
 * \brief gray scale dilation of an image
 *
 * Dilate an image using grayscale morphology. Dilation takes the
 * maximum of all the pixels identified by the structuring element.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \sa MorphologyImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template< typename TInputImage, typename TOutputImage, typename TKernel >
class ITK_TEMPLATE_EXPORT GrayscaleMorphologicalOpeningImageFilter:
  public KernelImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  /** Standard class typedefs. */
  typedef GrayscaleMorphologicalOpeningImageFilter                Self;
  typedef KernelImageFilter< TInputImage, TOutputImage, TKernel > Superclass;
  typedef SmartPointer< Self >                                    Pointer;
  typedef SmartPointer< const Self >                              ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GrayscaleMorphologicalOpeningImageFilter,
               KernelImageFilter);

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Image related typedefs. */
  typedef TInputImage                                InputImageType;
  typedef TOutputImage                               OutputImageType;
  typedef typename TInputImage::RegionType           RegionType;
  typedef typename TInputImage::SizeType             SizeType;
  typedef typename TInputImage::IndexType            IndexType;
  typedef typename TInputImage::PixelType            PixelType;
  typedef typename TInputImage::OffsetType           OffsetType;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  typedef FlatStructuringElement< itkGetStaticConstMacro(ImageDimension) >
  FlatKernelType;
  typedef MovingHistogramDilateImageFilter< TInputImage, TOutputImage, TKernel >
  HistogramDilateFilterType;
  typedef MovingHistogramErodeImageFilter< TInputImage, TOutputImage, TKernel >
  HistogramErodeFilterType;
  typedef BasicErodeImageFilter< TInputImage, TInputImage, TKernel >
  BasicErodeFilterType;
  typedef BasicDilateImageFilter< TInputImage, TOutputImage, TKernel >
  BasicDilateFilterType;
  typedef AnchorOpenImageFilter< TInputImage, FlatKernelType > AnchorFilterType;
  typedef VanHerkGilWermanErodeImageFilter< TInputImage, FlatKernelType >
  VanHerkGilWermanErodeFilterType;
  typedef VanHerkGilWermanDilateImageFilter< TInputImage, FlatKernelType >
  VanHerkGilWermanDilateFilterType;
  typedef CastImageFilter< TInputImage, TOutputImage > SubtractFilterType;

  /** Kernel typedef. */
  typedef TKernel KernelType;
//   typedef typename KernelType::Superclass KernelSuperclass;
//   typedef Neighborhood< typename KernelType::PixelType, ImageDimension >
// KernelSuperclass;

  /** Set kernel (structuring element). */
  void SetKernel(const KernelType & kernel) ITK_OVERRIDE;

  /** Set/Get the backend filter class. */
  void SetAlgorithm(int algo);

  itkGetConstMacro(Algorithm, int);

  /** GrayscaleMorphologicalOpeningImageFilter need to set its internal filters
    as modified */
  virtual void Modified() const ITK_OVERRIDE;

  /** define values used to determine which algorithm to use */
  enum AlgorithmType {
    BASIC = 0,
    HISTO = 1,
    ANCHOR = 2,
    VHGW = 3
  };

  /** A safe border is added to input image to avoid borders effects
   * and remove it once the closing is done */
  itkSetMacro(SafeBorder, bool);
  itkGetConstReferenceMacro(SafeBorder, bool);
  itkBooleanMacro(SafeBorder);

protected:
  GrayscaleMorphologicalOpeningImageFilter();
  ~GrayscaleMorphologicalOpeningImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GrayscaleMorphologicalOpeningImageFilter);

  // the filters used internally
  typename HistogramDilateFilterType::Pointer m_HistogramDilateFilter;

  typename HistogramErodeFilterType::Pointer m_HistogramErodeFilter;

  typename BasicDilateFilterType::Pointer m_BasicDilateFilter;

  typename BasicErodeFilterType::Pointer m_BasicErodeFilter;

  typename VanHerkGilWermanDilateFilterType::Pointer m_VanHerkGilWermanDilateFilter;

  typename VanHerkGilWermanErodeFilterType::Pointer m_VanHerkGilWermanErodeFilter;

  typename AnchorFilterType::Pointer m_AnchorFilter;

  // and the name of the filter
  int m_Algorithm;

  bool m_SafeBorder;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleMorphologicalOpeningImageFilter.hxx"
#endif

#endif
