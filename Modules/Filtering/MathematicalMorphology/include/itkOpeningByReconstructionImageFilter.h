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
#ifndef itkOpeningByReconstructionImageFilter_h
#define itkOpeningByReconstructionImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class OpeningByReconstructionImageFilter
 * \brief Opening by reconstruction of an image
 *
 * This filter preserves regions, in the foreground, that can
 * completely contain the structuring element. At the same time, this
 * filter eliminates all other regions of foreground pixels. Contrary
 * to the mophological opening, the opening by reconstruction
 * preserves the shape of the components that are not removed by
 * erosion.  The opening by reconstruction of an image "f" is defined
 * as:
 *
 *   OpeningByReconstruction(f) = DilationByRecontruction(f, Erosion(f)).
 *
 * Opening by reconstruction not only removes structures destroyed by
 * the erosion, but also levels down the contrast of the brightest
 * regions. If PreserveIntensities is on, a subsequent reconstruction
 * by dilation using a marker image that is the original image for all
 * unaffected pixels.
 *
 * Opening by reconstruction is described in Chapter 6.3.9 of Pierre
 * Soille's book "Morphological Image Analysis: Principles and
 * Applications", Second Edition, Springer, 2003.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa GrayscaleMorphologicalOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template< typename TInputImage, typename TOutputImage, typename TKernel >
class ITK_TEMPLATE_EXPORT OpeningByReconstructionImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef OpeningByReconstructionImageFilter              Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef typename InputImageType::Pointer       InputImagePointer;
  typedef typename InputImageType::ConstPointer  InputImageConstPointer;
  typedef typename InputImageType::RegionType    InputImageRegionType;
  typedef typename InputImageType::PixelType     InputImagePixelType;
  typedef TOutputImage                           OutputImageType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;

  /** Kernel typedef. */
  typedef TKernel KernelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(OpeningByReconstructionImageFilter,
               ImageToImageFilter);

  /** Set kernel (structuring element). */
  itkSetMacro(Kernel, KernelType);

  /** Get the kernel (structuring element). */
  itkGetConstReferenceMacro(Kernel, KernelType);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  /**
   * Set/Get whether the original intensities of the image retained for
   * those pixels unaffected by the opening by reconstrcution. If Off,
   * the output pixel contrast will be reduced. */
  itkSetMacro(PreserveIntensities, bool);
  itkGetConstReferenceMacro(PreserveIntensities, bool);
  itkBooleanMacro(PreserveIntensities);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputImagePixelType > ) );
  // End concept checking
#endif

protected:
  OpeningByReconstructionImageFilter();
  ~OpeningByReconstructionImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** OpeningByReconstructionImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** OpeningByReconstructionImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(OpeningByReconstructionImageFilter);

  /** kernel or structuring element to use. */
  KernelType m_Kernel;
  bool       m_FullyConnected;
  bool       m_PreserveIntensities;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOpeningByReconstructionImageFilter.hxx"
#endif

#endif
