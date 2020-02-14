/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkGradientVectorFlowImageFilter_h
#define itkGradientVectorFlowImageFilter_h

#include "vnl/vnl_matrix_fixed.h"
#include "itkMath.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkLaplacianImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/**
 *\class GradientVectorFlowImageFilter
 * \brief
 * This class computes a diffusion of the gradient vectors for graylevel or binary
 * edge map derive from the image. It enlarges the capture range of the gradient
 * force and make external force derived from the gradient work effectively in the
 * framework of deformable model.
 *
 * This implementation of GVF closely follows this paper:
 * http://ww.vavlab.ee.boun.edu.tr/courses/574/materialx/Active%20Contours/xu_GVF.pdf
 *
 * dx and dy are assumed to be 1 and the CFL restriction for convergence
 * has been modified for multi-dimensional images
 *
 * \ingroup ImageFilters
 * \ingroup ImageSegmentation
 * \ingroup ITKImageFeature
 */
template <typename TInputImage, typename TOutputImage, typename TInternalPixel = double>
class ITK_TEMPLATE_EXPORT GradientVectorFlowImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GradientVectorFlowImageFilter);

  /** Standard "Self" type alias. */
  using Self = GradientVectorFlowImageFilter;

  /** Standard "Superclass" type alias. */
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientVectorFlowImageFilter, ImageToImageFilter);

  /** Some type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  using IndexType = typename TInputImage::IndexType;
  using SizeType = typename TInputImage::SizeType;
  using PixelType = typename TInputImage::PixelType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using RegionType = typename OutputImageType::RegionType;

  /** Image and Image iterator definition. */
  using InputImageIterator = ImageRegionIterator<InputImageType>;
  using InputImageConstIterator = ImageRegionConstIterator<InputImageType>;
  using OutputImageIterator = ImageRegionIterator<OutputImageType>;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  using InternalPixelType = TInternalPixel;
  using InternalImageType = itk::Image<InternalPixelType, Self::ImageDimension>;
  using InternalImagePointer = typename InternalImageType::Pointer;
  using InternalImageIterator = ImageRegionIterator<InternalImageType>;
  using InternalImageConstIterator = ImageRegionConstIterator<InternalImageType>;

  using LaplacianFilterType = LaplacianImageFilter<InternalImageType, InternalImageType>;
  using LaplacianFilterPointer = typename LaplacianFilterType::Pointer;

  /** Routines. */

  itkSetObjectMacro(LaplacianFilter, LaplacianFilterType);
  itkGetModifiableObjectMacro(LaplacianFilter, LaplacianFilterType);

  itkSetMacro(TimeStep, double);
  itkGetConstMacro(TimeStep, double);

  itkSetMacro(NoiseLevel, double);
  itkGetConstMacro(NoiseLevel, double);

  itkSetMacro(IterationNum, int);
  itkGetConstMacro(IterationNum, int);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename PixelType::ValueType>));
  itkConceptMacro(OutputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<typename TOutputImage::PixelType::ValueType>));
  // End concept checking
#endif

protected:
  GradientVectorFlowImageFilter();
  ~GradientVectorFlowImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  /** Precompute m_BImage and m_CImage[i] and allocate memory for all the various internal images */
  void
  InitInterImage();

  /**
   *  Convenience function to split the m_IntermediateImage into its component
   *  images (m_InternalImages[i]
   */
  void
  UpdateInterImage();

  /** Calculate the next timestep and update the appropriate images */
  void
  UpdatePixels();

private:
  // parameters;
  double m_TimeStep;                               // the timestep of each
                                                   // iteration
  double m_Steps[Superclass::InputImageDimension]; // set to be 1 in all
                                                   // directions in most cases
  double m_NoiseLevel;                             // the noise level of the
                                                   // image
  int m_IterationNum;                              // the iteration number

  LaplacianFilterPointer                 m_LaplacianFilter;
  typename Superclass::InputImagePointer m_IntermediateImage;

  InternalImagePointer m_InternalImages[Superclass::InputImageDimension];
  InternalImagePointer m_BImage; // store the "b" value for every pixel

  typename Superclass::InputImagePointer m_CImage; // store the $c_i$ value for
                                                   // every pixel
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGradientVectorFlowImageFilter.hxx"
#endif

#endif
