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
#ifndef itkDifferenceImageFilter_h
#define itkDifferenceImageFilter_h

#include "itkConfigure.h" //Needed to determine value of ITKV3_COMPATIBILITY
#ifdef ITKV3_COMPATIBILITY

#include "itkTestingComparisonImageFilter.h"

namespace itk
{
 /** \class DifferenceImageFilter
 * This filter is an alias to the TestingComparisonImageFilter
 * and is only here for backwards compatibility.
 *
 * This class has no implementation, thus no .hxx file is needed.
 * \ingroup ITKTestKernel
 */
template< typename TInputImage, typename TOutputImage >
class DifferenceImageFilter:
  public Testing::ComparisonImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef DifferenceImageFilter                                       Self;
  typedef Testing::ComparisonImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DifferenceImageFilter, Testing::ComparisonImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage                                         InputImageType;
  typedef TOutputImage                                        OutputImageType;
  typedef typename OutputImageType::PixelType                 OutputPixelType;
  typedef typename OutputImageType::RegionType                OutputImageRegionType;
  typedef typename NumericTraits< OutputPixelType >::RealType RealType;
  typedef typename NumericTraits< RealType >::AccumulateType  AccumulateType;

  DifferenceImageFilter() {}
  virtual ~DifferenceImageFilter() {}

protected:

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DifferenceImageFilter);

};
} // end namespace itk
#else
#error For ITKv4 compatibility, use itk::Testing::ComparisonImageFilter instead of itk::DifferenceImageFilter
#endif

#endif
