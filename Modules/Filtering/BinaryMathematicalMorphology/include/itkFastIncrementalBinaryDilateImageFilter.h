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
#ifndef itkFastIncrementalBinaryDilateImageFilter_h
#define itkFastIncrementalBinaryDilateImageFilter_h

#include <vector>
#include <queue>
#include "itkBinaryDilateImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 * \class FastIncrementalBinaryDilateImageFilter
 * \brief Fast binary dilation
 *
 * FastIncrementalBinaryDilateImageFilter is a binary dilation
 * morphologic operation. This implementation is based on the papers:
 *
 * L.Vincent "Morphological transformations of binary images with
 * arbitrary structuring elements", and
 *
 * N.Nikopoulos et al. "An efficient algorithm for 3d binary
 * morphological transformations with 3d structuring elements
 * for arbitrary size and shape". IEEE Transactions on Image
 * Processing. Vol. 9. No. 3. 2000. pp. 283-286.
 *
 * This filter is maintained for backward compatibility. It is now a
 * subclass of BinaryDilateImageFilter (the fast incremental binary
 * dilate algorithm is now in BinaryDilateImageFilter).
 *
 * \deprecated
 * \sa BinaryDilateImageFilter
 * \ingroup ITKBinaryMathematicalMorphology
 */
template< typename TInputImage, typename TOutputImage, typename TKernel >
class FastIncrementalBinaryDilateImageFilter:
  public BinaryDilateImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  /** Extract dimension from input and output image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Extract the dimension of the kernel */
  itkStaticConstMacro(KernelDimension, unsigned int,
                      TKernel::NeighborhoodDimension);

  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  typedef TKernel      KernelType;

  /** Standard class typedefs. */
  typedef FastIncrementalBinaryDilateImageFilter Self;
  typedef BinaryDilateImageFilter< InputImageType, OutputImageType, KernelType >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastIncrementalBinaryDilateImageFilter, ImageToImageFilter);

protected:
  FastIncrementalBinaryDilateImageFilter() {}
  virtual ~FastIncrementalBinaryDilateImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastIncrementalBinaryDilateImageFilter);
};
} // end namespace itk

#endif
