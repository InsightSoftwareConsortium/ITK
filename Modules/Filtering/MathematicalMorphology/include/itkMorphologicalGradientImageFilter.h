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
#ifndef itkMorphologicalGradientImageFilter_h
#define itkMorphologicalGradientImageFilter_h

#include "itkKernelImageFilter.h"
#include "itkMovingHistogramMorphologicalGradientImageFilter.h"
#include "itkBasicDilateImageFilter.h"
#include "itkBasicErodeImageFilter.h"
#include "itkAnchorErodeImageFilter.h"
#include "itkAnchorDilateImageFilter.h"
#include "itkVanHerkGilWermanDilateImageFilter.h"
#include "itkVanHerkGilWermanErodeImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkConstantBoundaryCondition.h"
#include "itkNeighborhood.h"

namespace itk
{
/**
 * \class MorphologicalGradientImageFilter
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
class ITK_TEMPLATE_EXPORT MorphologicalGradientImageFilter:
  public KernelImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  /** Standard class typedefs. */
  typedef MorphologicalGradientImageFilter                        Self;
  typedef KernelImageFilter< TInputImage, TOutputImage, TKernel > Superclass;
  typedef SmartPointer< Self >                                    Pointer;
  typedef SmartPointer< const Self >                              ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MorphologicalGradientImageFilter,
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
  typedef MovingHistogramMorphologicalGradientImageFilter< TInputImage, TOutputImage, TKernel >
  HistogramFilterType;
  typedef BasicDilateImageFilter< TInputImage, TInputImage, TKernel >
  BasicDilateFilterType;
  typedef BasicErodeImageFilter< TInputImage, TInputImage, TKernel >
  BasicErodeFilterType;
  typedef AnchorDilateImageFilter< TInputImage, FlatKernelType >
  AnchorDilateFilterType;
  typedef AnchorErodeImageFilter< TInputImage, FlatKernelType > AnchorErodeFilterType;
  typedef VanHerkGilWermanDilateImageFilter< TInputImage, FlatKernelType >
  VHGWDilateFilterType;
  typedef VanHerkGilWermanErodeImageFilter< TInputImage, FlatKernelType >
  VHGWErodeFilterType;
  typedef SubtractImageFilter< TInputImage, TInputImage, TOutputImage >
  SubtractFilterType;

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

  /** MorphologicalGradientImageFilter need to set its internal filters as
    modified */
  virtual void Modified() const ITK_OVERRIDE;

  /** define values used to determine which algorithm to use */
  enum AlgorithmType {
    BASIC = 0,
    HISTO = 1,
    ANCHOR = 2,
    VHGW = 3
    };

protected:
  MorphologicalGradientImageFilter();
  ~MorphologicalGradientImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MorphologicalGradientImageFilter);

  // the filters used internally
  typename HistogramFilterType::Pointer m_HistogramFilter;

  typename BasicDilateFilterType::Pointer m_BasicDilateFilter;

  typename BasicErodeFilterType::Pointer m_BasicErodeFilter;

  typename AnchorDilateFilterType::Pointer m_AnchorDilateFilter;

  typename AnchorErodeFilterType::Pointer m_AnchorErodeFilter;

  typename VHGWDilateFilterType::Pointer m_VanHerkGilWermanDilateFilter;

  typename VHGWErodeFilterType::Pointer m_VanHerkGilWermanErodeFilter;

  // and the name of the filter
  int m_Algorithm;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMorphologicalGradientImageFilter.hxx"
#endif

#endif
