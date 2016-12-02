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
#ifndef itkVectorNeighborhoodInnerProduct_h
#define itkVectorNeighborhoodInnerProduct_h

#include "itkNeighborhoodIterator.h"
#include "itkVector.h"
#include "itkConstSliceIterator.h"
#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class VectorNeighborhoodInnerProduct
 *  \brief Defines the inner product operation between an itk::Neighborhood
 *         and an itk::NeighborhoodOperator.
 *
 * This is an explicit implementation of what should really be a partial
 * template specialization of NeighborhoodInnerProduct for itkVector.
 *
 * This class defines the inner product operation between an itk::Neighborhood
 * and and itk::NeighborhoodOperator.  The operator() method is overloaded
 * to support various types of neighborhoods as well as inner products with
 * slices of neighborhoods.
 *
 * \ingroup Operators
 *
 * \ingroup ITKCommon
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT VectorNeighborhoodInnerProduct
{
public:
  /** Standard typedefs */
  typedef VectorNeighborhoodInnerProduct Self;

  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Extract the pixel type and scalar type from the image template parameter.
    */
  typedef typename TImage::PixelType    PixelType;
  typedef typename PixelType::ValueType ScalarValueType;
  typedef Neighborhood< PixelType, itkGetStaticConstMacro(ImageDimension) >
  NeighborhoodType;

  /** Extract the image and vector dimension from the image template parameter.
    */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      PixelType::Dimension);

  /** Operator typedef */
  typedef Neighborhood< ScalarValueType,
                        itkGetStaticConstMacro(ImageDimension) > OperatorType;

  /** Conversion operator. */
  PixelType operator()(const std::slice & s,
                       const ConstNeighborhoodIterator< TImage > & it,
                       const OperatorType & op) const;

  /** Conversion operator. */
  PixelType operator()(const ConstNeighborhoodIterator< TImage > & it,
                       const OperatorType & op) const
  {
    return this->operator()(std::slice(0, it.Size(), 1), it, op);
  }

  PixelType operator()(const std::slice & s, const NeighborhoodType & N,
                       const OperatorType & op) const;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorNeighborhoodInnerProduct.hxx"
#endif

#endif
