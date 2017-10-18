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
#ifndef itkGrayscaleErodeImageFilter_h
#define itkGrayscaleErodeImageFilter_h

#include "itkKernelImageFilter.h"
#include "itkMovingHistogramErodeImageFilter.h"
#include "itkBasicErodeImageFilter.h"
#include "itkAnchorErodeImageFilter.h"
#include "itkVanHerkGilWermanErodeImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkConstantBoundaryCondition.h"
#include "itkNeighborhood.h"

namespace itk
{
/**
 * \class GrayscaleErodeImageFilter
 * \brief Grayscale erosion of an image.
 *
 * Erode an image using grayscale morphology. Erosion takes the
 * maximum of all the pixels identified by the structuring element.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \sa MorphologyImageFilter, GrayscaleFunctionErodeImageFilter, BinaryErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 *
 * \wiki
 * \wikiexample{Morphology/GrayscaleErodeImageFilter,Erode a grayscale image}
 * \endwiki
 */

template< typename TInputImage, typename TOutputImage, typename TKernel >
class ITK_TEMPLATE_EXPORT GrayscaleErodeImageFilter:
  public KernelImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  /** Standard class typedefs. */
  typedef GrayscaleErodeImageFilter                               Self;
  typedef KernelImageFilter< TInputImage, TOutputImage, TKernel > Superclass;
  typedef SmartPointer< Self >                                    Pointer;
  typedef SmartPointer< const Self >                              ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GrayscaleErodeImageFilter,
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

  /** define values used to determine which algorithm to use */
  enum AlgorithmType {
    BASIC = 0,
    HISTO = 1,
    ANCHOR = 2,
    VHGW = 3
    };

  typedef MovingHistogramErodeImageFilter< TInputImage, TOutputImage, TKernel >
  HistogramFilterType;
  typedef BasicErodeImageFilter< TInputImage, TOutputImage, TKernel >
  BasicFilterType;

  typedef FlatStructuringElement< itkGetStaticConstMacro(ImageDimension) > FlatKernelType;

  typedef AnchorErodeImageFilter< TInputImage, FlatKernelType >           AnchorFilterType;
  typedef VanHerkGilWermanErodeImageFilter< TInputImage, FlatKernelType > VHGWFilterType;
  typedef CastImageFilter< TInputImage, TOutputImage >                    CastFilterType;

  /** Typedef for boundary conditions. */
  typedef ImageBoundaryCondition< InputImageType > *      ImageBoundaryConditionPointerType;
  typedef ImageBoundaryCondition< InputImageType > const *ImageBoundaryConditionConstPointerType;
  typedef ConstantBoundaryCondition< InputImageType >     DefaultBoundaryConditionType;

  /** Kernel typedef. */
  typedef TKernel KernelType;
//   typedef typename KernelType::Superclass KernelSuperclass;
//   typedef Neighborhood< typename KernelType::PixelType, ImageDimension >
// KernelSuperclass;

  /** Set kernel (structuring element). */
  void SetKernel(const KernelType & kernel) ITK_OVERRIDE;

  /** Set/Get the boundary value. */
  void SetBoundary(const PixelType value);

  itkGetConstMacro(Boundary, PixelType);

  /** Set/Get the backend filter class. */
  void SetAlgorithm(int algo);

  itkGetConstMacro(Algorithm, int);

  /** GrayscaleErodeImageFilter need to set its internal filters as modified */
  virtual void Modified() const ITK_OVERRIDE;

  void SetNumberOfThreads(ThreadIdType nb) ITK_OVERRIDE;

protected:
  GrayscaleErodeImageFilter();
  ~GrayscaleErodeImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GrayscaleErodeImageFilter);

  PixelType m_Boundary;

  // the filters used internally
  typename HistogramFilterType::Pointer m_HistogramFilter;

  typename BasicFilterType::Pointer m_BasicFilter;

  typename AnchorFilterType::Pointer m_AnchorFilter;

  typename VHGWFilterType::Pointer m_VHGWFilter;

  // and the name of the filter
  int m_Algorithm;

  // the boundary condition need to be stored here
  DefaultBoundaryConditionType m_BoundaryCondition;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleErodeImageFilter.hxx"
#endif

#endif
