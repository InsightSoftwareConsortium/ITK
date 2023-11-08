// This file is part of Eigen, a lightweight C++ template library
// for linear algebra.
//
// Copyright (C) 2009-2010 Gael Guennebaud <gael.guennebaud@inria.fr>
//
// This Source Code Form is subject to the terms of the Mozilla
// Public License v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef EIGEN_CWISE_UNARY_VIEW_H
#define EIGEN_CWISE_UNARY_VIEW_H

// IWYU pragma: private
#include "./InternalHeaderCheck.h"

namespace Eigen {

namespace internal {
template<typename ViewOp, typename MatrixType, typename StrideType>
struct traits<CwiseUnaryView<ViewOp, MatrixType, StrideType> >
 : traits<MatrixType>
{
  typedef typename result_of<
                     ViewOp(const typename traits<MatrixType>::Scalar&)
                   >::type Scalar;
  typedef typename MatrixType::Nested MatrixTypeNested;
  typedef remove_all_t<MatrixTypeNested> MatrixTypeNested_;
  enum {
    FlagsLvalueBit = is_lvalue<MatrixType>::value ? LvalueBit : 0,
    Flags = traits<MatrixTypeNested_>::Flags & (RowMajorBit | FlagsLvalueBit | DirectAccessBit), // FIXME DirectAccessBit should not be handled by expressions
    MatrixTypeInnerStride =  inner_stride_at_compile_time<MatrixType>::ret,
    // need to cast the sizeof's from size_t to int explicitly, otherwise:
    // "error: no integral type can represent all of the enumerator values
    InnerStrideAtCompileTime = StrideType::InnerStrideAtCompileTime == 0
                             ? (MatrixTypeInnerStride == Dynamic
                               ? int(Dynamic)
                               : int(MatrixTypeInnerStride) * int(sizeof(typename traits<MatrixType>::Scalar) / sizeof(Scalar)))
                             : int(StrideType::InnerStrideAtCompileTime),

    OuterStrideAtCompileTime = StrideType::OuterStrideAtCompileTime == 0
                             ? (outer_stride_at_compile_time<MatrixType>::ret == Dynamic
                               ? int(Dynamic)
                               : outer_stride_at_compile_time<MatrixType>::ret * int(sizeof(typename traits<MatrixType>::Scalar) / sizeof(Scalar)))
                             : int(StrideType::OuterStrideAtCompileTime)
  };
};
}

template<typename ViewOp, typename MatrixType, typename StrideType, typename StorageKind>
class CwiseUnaryViewImpl;

/** \class CwiseUnaryView
  * \ingroup Core_Module
  *
  * \brief Generic lvalue expression of a coefficient-wise unary operator of a matrix or a vector
  *
  * \tparam ViewOp template functor implementing the view
  * \tparam MatrixType the type of the matrix we are applying the unary operator
  *
  * This class represents a lvalue expression of a generic unary view operator of a matrix or a vector.
  * It is the return type of real() and imag(), and most of the time this is the only way it is used.
  *
  * \sa MatrixBase::unaryViewExpr(const CustomUnaryOp &) const, class CwiseUnaryOp
  */
template<typename ViewOp, typename MatrixType, typename StrideType>
class CwiseUnaryView : public CwiseUnaryViewImpl<ViewOp, MatrixType, StrideType, typename internal::traits<MatrixType>::StorageKind>
{
  public:

    typedef typename CwiseUnaryViewImpl<ViewOp, MatrixType, StrideType, typename internal::traits<MatrixType>::StorageKind>::Base Base;
    EIGEN_GENERIC_PUBLIC_INTERFACE(CwiseUnaryView)
    typedef typename internal::ref_selector<MatrixType>::non_const_type MatrixTypeNested;
    typedef internal::remove_all_t<MatrixType> NestedExpression;

    explicit EIGEN_DEVICE_FUNC inline CwiseUnaryView(MatrixType& mat, const ViewOp& func = ViewOp())
      : m_matrix(mat), m_functor(func) {}

    EIGEN_INHERIT_ASSIGNMENT_OPERATORS(CwiseUnaryView)

    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE EIGEN_CONSTEXPR
    Index rows() const EIGEN_NOEXCEPT { return m_matrix.rows(); }
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE EIGEN_CONSTEXPR
    Index cols() const EIGEN_NOEXCEPT { return m_matrix.cols(); }

    /** \returns the functor representing unary operation */
    EIGEN_DEVICE_FUNC const ViewOp& functor() const { return m_functor; }

    /** \returns the nested expression */
    EIGEN_DEVICE_FUNC const internal::remove_all_t<MatrixTypeNested>&
    nestedExpression() const { return m_matrix; }

    /** \returns the nested expression */
    EIGEN_DEVICE_FUNC std::remove_reference_t<MatrixTypeNested>&
    nestedExpression() { return m_matrix; }

  protected:
    MatrixTypeNested m_matrix;
    ViewOp m_functor;
};

// Generic API dispatcher
template<typename ViewOp, typename XprType, typename StrideType, typename StorageKind>
class CwiseUnaryViewImpl
  : public internal::generic_xpr_base<CwiseUnaryView<ViewOp, XprType, StrideType> >::type
{
public:
  typedef typename internal::generic_xpr_base<CwiseUnaryView<ViewOp, XprType, StrideType> >::type Base;
};

template<typename ViewOp, typename MatrixType, typename StrideType>
class CwiseUnaryViewImpl<ViewOp,MatrixType,StrideType,Dense>
  : public internal::dense_xpr_base< CwiseUnaryView<ViewOp, MatrixType, StrideType> >::type
{
  public:

    typedef CwiseUnaryView<ViewOp, MatrixType,StrideType> Derived;
    typedef typename internal::dense_xpr_base< CwiseUnaryView<ViewOp, MatrixType,StrideType> >::type Base;

    EIGEN_DENSE_PUBLIC_INTERFACE(Derived)
    EIGEN_INHERIT_ASSIGNMENT_OPERATORS(CwiseUnaryViewImpl)

    EIGEN_DEVICE_FUNC inline Scalar* data() { return &(this->coeffRef(0)); }
    EIGEN_DEVICE_FUNC inline const Scalar* data() const { return &(this->coeff(0)); }

    EIGEN_DEVICE_FUNC EIGEN_CONSTEXPR inline Index innerStride() const
    {
      return StrideType::InnerStrideAtCompileTime != 0
             ? int(StrideType::InnerStrideAtCompileTime)
             : derived().nestedExpression().innerStride() * sizeof(typename internal::traits<MatrixType>::Scalar) / sizeof(Scalar);
    }

    EIGEN_DEVICE_FUNC EIGEN_CONSTEXPR inline Index outerStride() const
    {
      return StrideType::OuterStrideAtCompileTime != 0
             ? int(StrideType::OuterStrideAtCompileTime)
             : derived().nestedExpression().outerStride() * sizeof(typename internal::traits<MatrixType>::Scalar) / sizeof(Scalar);
    }
  protected:
    EIGEN_DEFAULT_EMPTY_CONSTRUCTOR_AND_DESTRUCTOR(CwiseUnaryViewImpl)
};

} // end namespace Eigen

#endif // EIGEN_CWISE_UNARY_VIEW_H
