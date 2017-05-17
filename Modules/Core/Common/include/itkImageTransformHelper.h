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
#ifndef itkImageTransformHelper_h
#define itkImageTransformHelper_h

#include "itkConceptChecking.h"
#include "itkImageBase.h"
#include "itkMatrix.h"
#include "itkMath.h"

namespace itk
{
/** \class ImageTransformHelper
 *  \brief Fast index/physical index computation
 * \ingroup ITKCommon
 */
template< unsigned int NImageDimension, unsigned int R, unsigned int C, typename TPointValue = double, typename TMatrixValue=double >
class ITK_TEMPLATE_EXPORT ImageTransformHelper
{
public:
  typedef ImageBase< NImageDimension >                             ImageType;
  typedef typename ImageType::IndexType                            IndexType;
  typedef typename ImageType::SpacingType                          SpacingType;
  typedef Matrix< TMatrixValue, NImageDimension, NImageDimension > MatrixType;
  typedef typename ImageType::PointType                            OriginType;
  typedef Point< double, NImageDimension >                         DoublePoint;
  typedef Point< float, NImageDimension >                          FloatPoint;
  typedef Concept::Detail::UniqueType_bool< false >                UniqueTypeBoolFalse;
  typedef Concept::Detail::UniqueType_bool< true >                 UniqueTypeBoolTrue;

  //
  // Methods with DoublePoint
  //

  // IndexToPhysicalPoint with full matrix
  //
  //
  inline static void TransformIndexToPhysicalPoint(
    const MatrixType & matrix, const OriginType  & origin,
    const IndexType & index, DoublePoint & point)
  {
    ImageTransformHelper< NImageDimension, R, C, TPointValue, TMatrixValue >::
    TransformIndexToPhysicalPointRow(
      matrix, origin,
      index, point,
      Concept::Detail::UniqueType_bool< ( R + 1 == 0 ) >() );
  }

  inline static void TransformIndexToPhysicalPointRow(
    const MatrixType & matrix, const OriginType  & origin,
    const IndexType & index, DoublePoint & point,
    const UniqueTypeBoolFalse &)
  {
    point[R] = origin[R];

    // Start column
    ImageTransformHelper< NImageDimension, R, C, TPointValue, TMatrixValue >
    ::TransformIndexToPhysicalPointCol(
      matrix,
      index, point,
      Concept::Detail::UniqueType_bool< ( C + 1 == 0 ) >() );
    // Do Next Row
    ImageTransformHelper< NImageDimension, R - 1, C, TPointValue, TMatrixValue >
    ::TransformIndexToPhysicalPointRow(
      matrix, origin,
      index, point,
      Concept::Detail::UniqueType_bool< ( R == 0 ) >() );
  }

  inline static void TransformIndexToPhysicalPointRow(
    const MatrixType &, const OriginType  &,
    const IndexType &, DoublePoint &,
    const UniqueTypeBoolTrue &)
  {
    // Do last row
  }

  inline static void TransformIndexToPhysicalPointCol(
    const MatrixType & matrix,
    const IndexType & index, DoublePoint & point,
    const UniqueTypeBoolFalse &)
  {
    point[R] = point[R] + matrix[R][C] * index[C];

    // Do next dimension
    ImageTransformHelper< NImageDimension, R, C - 1, TPointValue, TMatrixValue >
    ::TransformIndexToPhysicalPointCol(
      matrix,
      index, point,
      Concept::Detail::UniqueType_bool< ( C == 0 ) >() );
  }

  inline static void TransformIndexToPhysicalPointCol(
    const MatrixType &,
    const IndexType &, DoublePoint &,
    const UniqueTypeBoolTrue &)
  {}

  // PhysicalPointToIndex with full matrix
  //
  //
  inline static void TransformPhysicalPointToIndex(
    const MatrixType & matrix, const OriginType  & origin,
    const DoublePoint & point, IndexType  & index)
  {
    DoublePoint rindex;

    ImageTransformHelper< NImageDimension, R, C, TPointValue, TMatrixValue >::
    TransformPhysicalPointToIndexRow(
      matrix, origin,
      point, rindex, index,
      Concept::Detail::UniqueType_bool< ( R + 1 == 0 ) >() );
  }

  inline static void TransformPhysicalPointToIndexRow(
    const MatrixType & matrix, const OriginType  & origin,
    const DoublePoint & point, DoublePoint & rindex, IndexType & index,
    const UniqueTypeBoolFalse &)
  {
    rindex[R] = 0.0;
    // Start column
    ImageTransformHelper< NImageDimension, R, C, TPointValue, TMatrixValue >
    ::TransformPhysicalPointToIndexCol(
      matrix, origin,
      point, rindex, index,
      Concept::Detail::UniqueType_bool< ( C + 1 == 0 ) >() );
    // Do next row
    ImageTransformHelper< NImageDimension, R - 1, C, TPointValue, TMatrixValue >
    ::TransformPhysicalPointToIndexRow(
      matrix, origin,
      point, rindex, index,
      Concept::Detail::UniqueType_bool< ( R == 0 ) >() );
  }

  inline static void TransformPhysicalPointToIndexRow(
    const MatrixType &, const OriginType &,
    const DoublePoint &, DoublePoint &, IndexType &,
    const UniqueTypeBoolTrue &)
  {
    // Do last row
  }

  inline static void TransformPhysicalPointToIndexCol(
    const MatrixType & matrix, const OriginType  & origin,
    const DoublePoint & point, DoublePoint & rindex, IndexType & index,
    const UniqueTypeBoolFalse &)
  {
    rindex[R] = rindex[R] + matrix[R][C] * ( point[C] - origin[C] );

    // Do next dimension
    ImageTransformHelper< NImageDimension, R, C - 1, TPointValue, TMatrixValue >
    ::TransformPhysicalPointToIndexCol(
      matrix, origin,
      point, rindex, index,
      Concept::Detail::UniqueType_bool< ( C == 0 ) >() );
  }

  inline static void TransformPhysicalPointToIndexCol(
    const MatrixType &, const OriginType  &,
    const DoublePoint &, DoublePoint & rindex, IndexType & index,
    const UniqueTypeBoolTrue &)
  {
    index[R] = Math::RoundHalfIntegerUp< IndexValueType >(rindex[R]);
  }

  //
  // Methods with FloatPoint
  //

  // IndexToPhysicalPoint with full matrix
  //
  //
  inline static void TransformIndexToPhysicalPoint(
    const MatrixType & matrix, const OriginType  & origin,
    const IndexType & index, FloatPoint & point)
  {
    ImageTransformHelper< NImageDimension, R, C, TPointValue, TMatrixValue >::
    TransformIndexToPhysicalPointRow(
      matrix, origin,
      index, point,
      Concept::Detail::UniqueType_bool< ( R + 1 == 0 ) >() );
  }

  inline static void TransformIndexToPhysicalPointRow(
    const MatrixType & matrix, const OriginType  & origin,
    const IndexType & index, FloatPoint & point,
    const UniqueTypeBoolFalse &)
  {
    point[R] = origin[R];

    // Start column
    ImageTransformHelper< NImageDimension, R, C, TPointValue, TMatrixValue >
    ::TransformIndexToPhysicalPointCol(
      matrix,
      index, point,
      Concept::Detail::UniqueType_bool< ( C + 1 == 0 ) >() );
    // Do Next Row
    ImageTransformHelper< NImageDimension, R - 1, C, TPointValue, TMatrixValue >
    ::TransformIndexToPhysicalPointRow(
      matrix, origin,
      index, point,
      Concept::Detail::UniqueType_bool< ( R == 0 ) >() );
  }

  inline static void TransformIndexToPhysicalPointRow(
    const MatrixType &, const OriginType  &,
    const IndexType &, FloatPoint &,
    const UniqueTypeBoolTrue &)
  {
    // Do last row
  }

  inline static void TransformIndexToPhysicalPointCol(
    const MatrixType & matrix,
    const IndexType & index, FloatPoint & point,
    const UniqueTypeBoolFalse &)
  {
    point[R] = point[R] + matrix[R][C] * index[C];

    // Do next dimension
    ImageTransformHelper< NImageDimension, R, C - 1, TPointValue, TMatrixValue >
    ::TransformIndexToPhysicalPointCol(
      matrix,
      index, point,
      Concept::Detail::UniqueType_bool< ( C == 0 ) >() );
  }

  inline static void TransformIndexToPhysicalPointCol(
    const MatrixType &,
    const IndexType &, FloatPoint &,
    const UniqueTypeBoolTrue &)
  {}

  // PhysicalPointToIndex with full matrix
  //
  //
  inline static void TransformPhysicalPointToIndex(
    const MatrixType & matrix, const OriginType  & origin,
    const FloatPoint & point, IndexType  & index)
  {
    FloatPoint rindex;

    ImageTransformHelper< NImageDimension, R, C, TPointValue, TMatrixValue >::
    TransformPhysicalPointToIndexRow(
      matrix, origin,
      point, rindex, index,
      Concept::Detail::UniqueType_bool< ( R + 1 == 0 ) >() );
  }

  inline static void TransformPhysicalPointToIndexRow(
    const MatrixType & matrix, const OriginType  & origin,
    const FloatPoint & point, FloatPoint & rindex, IndexType & index,
    const UniqueTypeBoolFalse &)
  {
    rindex[R] = 0.0;
    // Start column
    ImageTransformHelper< NImageDimension, R, C, TPointValue, TMatrixValue >
    ::TransformPhysicalPointToIndexCol(
      matrix, origin,
      point, rindex, index,
      Concept::Detail::UniqueType_bool< ( C + 1 == 0 ) >() );
    // Do next row
    ImageTransformHelper< NImageDimension, R - 1, C, TPointValue, TMatrixValue >
    ::TransformPhysicalPointToIndexRow(
      matrix, origin,
      point, rindex, index,
      Concept::Detail::UniqueType_bool< ( R == 0 ) >() );
  }

  inline static void TransformPhysicalPointToIndexRow(
    const MatrixType &, const OriginType &,
    const FloatPoint &, FloatPoint &, IndexType &,
    const UniqueTypeBoolTrue &)
  {
    // Do last row
  }

  inline static void TransformPhysicalPointToIndexCol(
    const MatrixType & matrix, const OriginType  & origin,
    const FloatPoint & point, FloatPoint & rindex, IndexType & index,
    const UniqueTypeBoolFalse &)
  {
    rindex[R] = rindex[R] + matrix[R][C] * ( point[C] - origin[C] );

    // Do next dimension
    ImageTransformHelper< NImageDimension, R, C - 1, TPointValue, TMatrixValue >
    ::TransformPhysicalPointToIndexCol(
      matrix, origin,
      point, rindex, index,
      Concept::Detail::UniqueType_bool< ( C == 0 ) >() );
  }

  inline static void TransformPhysicalPointToIndexCol(
    const MatrixType &, const OriginType  &,
    const FloatPoint &, FloatPoint & rindex, IndexType & index,
    const UniqueTypeBoolTrue &)
  {
    index[R] = Math::RoundHalfIntegerUp< IndexValueType >(rindex[R]);
  }
};
} // end namespace itk

#endif
