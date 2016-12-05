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
#ifndef itkSymmetricSecondRankTensor_h
#define itkSymmetricSecondRankTensor_h

// Undefine an eventual SymmetricSecondRankTensor macro
#ifdef SymmetricSecondRankTensor
#undef SymmetricSecondRankTensor
#endif

#include "itkIndent.h"
#include "itkFixedArray.h"
#include "itkMatrix.h"
#include "itkSymmetricEigenAnalysis.h"

namespace itk
{
/** \class SymmetricSecondRankTensor
 * \brief Represent a symmetric tensor of second rank.
 *
 * This class implements a ND symmetric tensor of second rank.
 *
 * Since SymmetricSecondRankTensor is a subclass of FixedArray,
 * you can access its components as:
 *
 * typedef itk::SymmetricSecondRankTensor< float >    TensorPixelType;
 * TensorPixelType tensor;
 *
 *   tensor[0] = 1.233;
 *   tensor[1] = 1.456;
 *
 * for convenience the indexed access is also available as
 *
 *   tensor(0,0) = 1.233;
 *   tensor(2,0) = 1.233;
 *
 * The Tensor in principle represents a NxN matrix, but given that it is always
 * symmetric the representation can be compacted into a N*(N+1)/2 elements
 * array that derives from the itk::FixedArray<T>
 *
 * \author Jeffrey Duda from School of Engineering at University of Pennsylvania
 * \author Torsten Rohlfing from SRI International Neuroscience Program.
 *
 * This class was mostly based on files that Jeffrey Duda, Torsten Rohlfing and
 * Martin Styner contributed to the ITK users list during a discussion on
 * support for DiffusionTensorImages. The funding for creating this class was
 * largely provided by NAMIC (National Alliance for Medical Image Computing)
 * (http://www.na-mic.org). A discussion on the design of this class can be
 * found in the WIKI pages of NAMIC:
 *
 * http://www.na-mic.org/Wiki/index.php/NAMIC_Wiki:DTI:ITK-DiffusionTensorPixelType
 *
 * \sa DiffusionTensor3D
 *
 * \ingroup ImageObjects   TensorObjects   Geometry
 * \ingroup ITKCommon
 */

template< typename TComponent, unsigned int NDimension = 3 >
class ITK_TEMPLATE_EXPORT SymmetricSecondRankTensor:public
  FixedArray< TComponent, NDimension *( NDimension + 1 ) / 2 >
{
public:
  /** Standard class typedefs. */
  typedef SymmetricSecondRankTensor                                    Self;
  typedef FixedArray< TComponent, NDimension *( NDimension + 1 ) / 2 > Superclass;

  /** Dimension of the vector space. */
  itkStaticConstMacro(Dimension, unsigned int, NDimension);
  itkStaticConstMacro( InternalDimension, unsigned int, ( NDimension * ( NDimension + 1 ) / 2 ) );

  /** Convenience typedefs. */
  typedef FixedArray< TComponent,
                      itkGetStaticConstMacro(InternalDimension) > BaseArray;

  /** Array of eigen-values. */
  typedef FixedArray< TComponent, NDimension > EigenValuesArrayType;

  /** Matrix of eigen-vectors. */
  typedef Matrix< TComponent, NDimension, NDimension > MatrixType;
  typedef Matrix< TComponent, NDimension, NDimension > EigenVectorsMatrixType;

  /**  Define the component type. */
  typedef TComponent                                    ComponentType;
  typedef typename Superclass::ValueType                ValueType;
  typedef typename NumericTraits< ValueType >::RealType AccumulateValueType;
  typedef typename NumericTraits< ValueType >::RealType RealValueType;

  typedef SymmetricEigenAnalysis< MatrixType,
                                  EigenValuesArrayType, EigenVectorsMatrixType >  SymmetricEigenAnalysisType;

  /** Default constructor has nothing to do. */
  SymmetricSecondRankTensor() { this->Fill(0); }

  SymmetricSecondRankTensor (const ComponentType & r) { this->Fill(r); }

  /** Constructor to enable casting...  */
  template< typename TCoordRepB >
  SymmetricSecondRankTensor(const SymmetricSecondRankTensor< TCoordRepB, NDimension > & pa):
    BaseArray(pa) {}

  typedef ComponentType ComponentArrayType[itkGetStaticConstMacro(InternalDimension)];

  /** Pass-through constructor for the Array base class. */
  SymmetricSecondRankTensor(const ComponentArrayType r):BaseArray(r) {}

  /** Templated Pass-through assignment  for the Array base class. */
  template< typename TCoordRepB >
  Self & operator=(const SymmetricSecondRankTensor< TCoordRepB, NDimension > & pa)
  {
    BaseArray::operator=(pa);
    return *this;
  }

  /** Pass-through assignment operator for the Array base class. */
  Self & operator=(const ComponentType & r);

  Self & operator=(const ComponentArrayType r);

  /** Aritmetic operations between pixels. Return a new
    SymmetricSecondRankTensor. */
  Self operator+(const Self & vec) const;

  Self operator-(const Self & vec) const;

  const Self & operator+=(const Self & vec);

  const Self & operator-=(const Self & vec);

  /** Arithmetic operations between tensors and scalars */
  Self operator *(const RealValueType & scalar) const;

  Self operator/(const RealValueType & scalar) const;

  const Self & operator*=(const RealValueType & scalar);

  const Self & operator/=(const RealValueType & scalar);

  /** Return the number of components. */
  static unsigned int GetNumberOfComponents()
  {
    return itkGetStaticConstMacro(InternalDimension);
  }

  /** Return the value for the Nth component. */
  ComponentType GetNthComponent(int c) const { return this->operator[](c); }

  /** Set the Nth component to v. */
  void SetNthComponent(int c, const ComponentType & v) {  this->operator[](c) = v; }

  /** Matrix notation, in const and non-const forms. */
  ValueType & operator()(unsigned int row, unsigned int col);

  const ValueType & operator()(unsigned int row, unsigned int col) const;

  /** Set the tensor to an identity tensor. This has 1 in its diagonal elements and
   * zero elsewhere. */
  void SetIdentity();

  /** Get Trace value */
  AccumulateValueType GetTrace() const;

  /** Return an array containing EigenValues. */
  void ComputeEigenValues(EigenValuesArrayType & eigenValues) const;

  /** Return an array containing EigenValues, and a matrix containing Eigen
   * vectors. */
  void ComputeEigenAnalysis(EigenValuesArrayType & eigenValues,
                            EigenVectorsMatrixType & eigenVectors) const;

  /** Returns the tensor rotated by the provided matrix.
   *  ResultingTensor = Matrix * ThisTensor * Matrix.GetTranspose()
   */
  template<typename TMatrixValueType>
  Self Rotate( const Matrix<TMatrixValueType, NDimension, NDimension> & m) const;
  template<typename TMatrixValueType>
  Self Rotate( const vnl_matrix_fixed<TMatrixValueType, NDimension, NDimension> & m) const
  {
    return this->Rotate( static_cast<Matrix<TMatrixValueType, NDimension, NDimension> >(m) );
  }
  template<typename TMatrixValueType>
  Self Rotate( const vnl_matrix<TMatrixValueType> & m) const
  {
    return this->Rotate( static_cast<Matrix<TMatrixValueType> >(m) );
  }

  /** Pre-Multiply by a Matrix as ResultingMatrix = Matrix * ThisTensor. */
  MatrixType PreMultiply(const MatrixType & m) const;

  /** Post-Multiply by a Matrix as ResultingMatrix = ThisTensor * Matrix. */
  MatrixType PostMultiply(const MatrixType & m) const;

private:
};

/** This extra typedef is necessary for preventing an Internal Compiler Error in
 * Microsoft Visual C++ 6.0. This typedef is not needed for any other compiler. */
typedef std::ostream OutputStreamType;
typedef std::istream InputStreamType;

template< typename TComponent, unsigned int NDimension  >
OutputStreamType & operator<<(OutputStreamType & os,
                                         const SymmetricSecondRankTensor< TComponent, NDimension > & c);

template< typename TComponent, unsigned int NDimension  >
InputStreamType & operator>>(InputStreamType & is,
                                        SymmetricSecondRankTensor< TComponent, NDimension > & c);
} // end namespace itk

#include "itkNumericTraitsTensorPixel.h"

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSymmetricSecondRankTensor.hxx"
#endif

#endif
