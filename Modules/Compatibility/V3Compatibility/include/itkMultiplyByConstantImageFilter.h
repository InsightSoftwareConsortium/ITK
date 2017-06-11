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
#ifndef itkMultiplyByConstantImageFilter_h
#define itkMultiplyByConstantImageFilter_h
#include "itkMultiplyImageFilter.h"

#ifndef ITKV3_COMPATIBILITY
#error "This file is only valid when ITKV3_COMPATIBILITY is turned on. Users are encouraged to convert to itk::MultiplyImageFilter in ITKv4"
#endif

namespace itk
{

/** \class MultiplyByConstantImageFilter
 *
 * \brief This class is deprecated.  All features of this
 * class have been incorporated into the MultiplyImageFilter.
 * This class is a thin wrapper around the MultiplyImageFilter
 * that provides the ITKv3 API.
 *
 * Multiply all input pixels by a constant.
 *
 * This filter is templated over the input image type
 * and the output image type.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * Based on filters from the Insight Journal paper:
 * https://hdl.handle.net/1926/510
 *
 * \deprecated
 * \ingroup ITKV3Compatibility
 * \sa MultiplyImageFilter
 */
template <typename TInputImage, typename TConstant, typename TOutputImage>
class MultiplyByConstantImageFilter :
      public
MultiplyImageFilter<TInputImage, Image<TConstant, TInputImage::ImageDimension>, TOutputImage>
{
public:
  typedef MultiplyByConstantImageFilter                             Self;
  typedef MultiplyImageFilter<TInputImage, Image<TConstant, TInputImage::ImageDimension>, TOutputImage>
                                                                    Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** method for creation through object factory */
  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiplyByConstantImageFilter, MultiplyImageFilter);

protected:
  MultiplyByConstantImageFilter() {}
  virtual ~MultiplyByConstantImageFilter() {}
};

}
#endif
