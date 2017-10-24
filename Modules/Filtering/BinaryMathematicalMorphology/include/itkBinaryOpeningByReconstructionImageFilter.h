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
#ifndef itkBinaryOpeningByReconstructionImageFilter_h
#define itkBinaryOpeningByReconstructionImageFilter_h

#include "itkKernelImageFilter.h"

namespace itk {

/**
 * \class BinaryOpeningByReconstructionImageFilter
 * \brief binary morphological closing of an image.
 *
 * This filter removes small (i.e., smaller than the structuring
 * element) objects in the image. It is defined as:
 * Opening(f) = ReconstructionByDilatation(Erosion(f)).
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa MorphologyImageFilter, OpeningByReconstructionImageFilter, BinaryClosingByReconstructionImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKBinaryMathematicalMorphology
 */

template<typename TInputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT BinaryOpeningByReconstructionImageFilter :
    public KernelImageFilter<TInputImage, TInputImage, TKernel>
{
public:
  /** Standard class typedefs. */
  typedef BinaryOpeningByReconstructionImageFilter     Self;
  typedef KernelImageFilter<TInputImage, TInputImage, TKernel>
                                                       Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryOpeningByReconstructionImageFilter,
               KernelImageFilter);

  typedef TInputImage                              InputImageType;
  typedef TInputImage                              OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;

  /** Declaration of pixel type. */
  typedef typename TInputImage::PixelType PixelType;

  /** Kernel typedef. */
  typedef TKernel KernelType;

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. */
  itkSetMacro(ForegroundValue, PixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. */
  itkGetMacro(ForegroundValue, PixelType);

  /** Set the value in eroded part of the image. Defaults to zero */
  itkSetMacro(BackgroundValue, PixelType);

  /** Set the value in eroded part of the image. Defaults to zero */
  itkGetMacro(BackgroundValue, PixelType);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

protected:
  BinaryOpeningByReconstructionImageFilter();
  ~BinaryOpeningByReconstructionImageFilter() ITK_OVERRIDE {};
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /** BinaryOpeningByReconstructionImageFilter need to make sure they request enough of an
   * input image to account for the structuring element size.  The input
   * requested region is expanded by the radius of the structuring element.
   * If the request extends past the LargestPossibleRegion for the input,
   * the request is cropped by the LargestPossibleRegion. */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleDilateImageFilter GrayscaleErodeImageFilter. */
  void  GenerateData () ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryOpeningByReconstructionImageFilter);

  PixelType  m_ForegroundValue;

  PixelType  m_BackgroundValue;

  bool       m_FullyConnected;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryOpeningByReconstructionImageFilter.hxx"
#endif

#endif
