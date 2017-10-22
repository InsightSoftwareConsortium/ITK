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
#ifndef itkGetAverageSliceImageFilter_h
#define itkGetAverageSliceImageFilter_h

#include "itkAccumulateImageFilter.h"

namespace itk
{
/** \class GetAverageSliceImageFilter
 * \brief Averages a single dimension of an image.
 *
 *    This class averages an image along a dimension and reduces the size
 * of this dimension to 1. The dimension being averaged is set by
 * AveragedOutDimension.
 *
 *   Each pixel is the average of the pixels along the collapsed
 * dimension and reduce the size of the averaged dimension to 1 (only
 * on the averaged dimension).
 *
 *   The dimensions of the InputImage and the OutputImage must be the same.
 *
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 * \sa AccumulateImageFilter
 *
 * \ingroup   IntensityImageFilters     SingleThreaded
 * \ingroup ITKImageStatistics
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT GetAverageSliceImageFilter:public AccumulateImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef GetAverageSliceImageFilter                         Self;
  typedef AccumulateImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                               Pointer;
  typedef SmartPointer< const Self >                         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GetAverageSliceImageFilter, AccumulateImageFilter);

  /** Set the direction in which to reflect the data. */
  itkGetConstMacro(AveragedOutDimension, unsigned int);
  itkSetMacro(AveragedOutDimension, unsigned int);

protected:
  GetAverageSliceImageFilter();
  virtual ~GetAverageSliceImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GetAverageSliceImageFilter);

  unsigned int m_AveragedOutDimension;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGetAverageSliceImageFilter.hxx"
#endif

#endif
