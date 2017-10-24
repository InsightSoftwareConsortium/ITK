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
/** \class GradientVectorFlowImageFilter
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
template< typename TInputImage, typename TOutputImage, typename TInternalPixel = double >
class ITK_TEMPLATE_EXPORT GradientVectorFlowImageFilter:public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard "Self" typedef. */
  typedef GradientVectorFlowImageFilter Self;

  /** Standard "Superclass" typedef. */
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientVectorFlowImageFilter, ImageToImageFilter);

  /** Some typedefs. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  typedef typename TInputImage::IndexType      IndexType;
  typedef typename TInputImage::SizeType       SizeType;
  typedef typename TInputImage::PixelType      PixelType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::RegionType RegionType;

  /** Image and Image iterator definition. */
  typedef ImageRegionIterator< InputImageType >               InputImageIterator;
  typedef ImageRegionConstIterator< InputImageType >          InputImageConstIterator;
  typedef ImageRegionIterator< OutputImageType >              OutputImageIterator;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  typedef TInternalPixel                                                          InternalPixelType;
  typedef itk::Image< InternalPixelType, itkGetStaticConstMacro(ImageDimension) > InternalImageType;
  typedef typename InternalImageType::Pointer                                     InternalImagePointer;
  typedef ImageRegionIterator< InternalImageType >                                InternalImageIterator;
  typedef ImageRegionConstIterator< InternalImageType >                           InternalImageConstIterator;

  typedef LaplacianImageFilter< InternalImageType, InternalImageType > LaplacianFilterType;
  typedef typename LaplacianFilterType::Pointer                        LaplacianFilterPointer;

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
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< ImageDimension, OutputImageDimension > ) );
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename PixelType::ValueType > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TOutputImage::PixelType::ValueType > ) );
  // End concept checking
#endif

protected:
  GradientVectorFlowImageFilter();
  ~GradientVectorFlowImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  /** Precompute m_BImage and m_CImage[i] and allocate memory for all the various internal images */
  void InitInterImage();

  /**
   *  Convenience function to split the m_IntermediateImage into its component
   *  images (m_InternalImages[i]
   */
  void UpdateInterImage();

  /** Calculate the next timestep and update the appropriate images */
  void UpdatePixels();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GradientVectorFlowImageFilter);

  // parameters;
  double m_TimeStep;                               // the timestep of each
                                                   // iteration
  double m_Steps[Superclass::InputImageDimension]; // set to be 1 in all
                                                   // directions in most cases
  double m_NoiseLevel;                             // the noise level of the
                                                   // image
  int m_IterationNum;                              // the iteration number

  LaplacianFilterPointer m_LaplacianFilter;
  typename Superclass::InputImagePointer m_IntermediateImage;

  InternalImagePointer m_InternalImages[Superclass::InputImageDimension];
  InternalImagePointer m_BImage;  // store the "b" value for every pixel

  typename Superclass::InputImagePointer m_CImage; // store the $c_i$ value for
                                                   // every pixel
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientVectorFlowImageFilter.hxx"
#endif

#endif
