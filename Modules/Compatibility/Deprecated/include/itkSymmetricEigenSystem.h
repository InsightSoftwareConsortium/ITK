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
#ifndef itkSymmetricEigenSystem_h
#define itkSymmetricEigenSystem_h

#include "itkObject.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "itkObjectFactory.h"
#include "itkObject.h"
#include "itkFixedArray.h"
#include "itkMatrix.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class SymmetricEigenSystem
 * \brief wrapper of the vnl_symmetric_eigensystem algorithm
 *
 * This class is not thread-safe. If you are interested in thread-safety please
 * use the class SymmetricEigenAnalysis in Insight/Code/Common.
 *
 * \warning THIS CLASS IS DEPRECATED AND IT IS SCHEDULED FOR BEING REMOVED
 *   FROM THE TOOLKIT IN RELEASE 2.4
 *
 * \sa SymmetricEigenAnalysis
 *
 * \ingroup Numerics Deprecated
 * \ingroup ITKDeprecated
 */

template< typename TMatrixElement, int VNumberOfRows >
class ITK_TEMPLATE_EXPORT SymmetricEigenSystem:public Object
{
public:
  /** Standard "Self" typedef. */
  typedef SymmetricEigenSystem       Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SymmetricEigenSystem, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** 1D array typedef */
  typedef FixedArray< TMatrixElement, VNumberOfRows > ArrayType;

  /** 2D array typedef */
  typedef FixedArray< ArrayType, VNumberOfRows > Array2DType;

  /** Array for eigen vectors */
  typedef Array2DType EigenVectorArrayType;

  /** Array type for eigen values */
  typedef ArrayType EigenValueArrayType;

  /** Matrix Type */
  typedef Matrix< TMatrixElement, VNumberOfRows, VNumberOfRows > MatrixType;

  /** Internal eigen system type. */
  typedef vnl_symmetric_eigensystem< TMatrixElement > InternalEigenSystemType;

  /** Set/Get the target matrix for the eigen analysis */
  itkSetObjectMacro(Matrix, MatrixType);
  MatrixType * GetMatrix()
  { return m_Matrix; }

  /** Set/Get the absolute order flag.
   * By setting this flag true, after the calculation of eigen vectors
   * and values, if the absolute eigen value of eigen vector[j > i] is
   * greater that of eigen vector[i], reorder the eigen vectors so that
   * every absolute eigen values of eigen vector[j < i] is always greater than or
   * equal to that of the eigen vectors[i] */
  itkSetMacro(UseAbsoluteOrder, bool);
  itkGetMacro(UseAbsoluteOrder, bool);

  /** returns the eigen vectors in a 2D array */
  EigenVectorArrayType * GetEigenVectors()
  { return &m_EigenVectors; }

  /** returns the eigen values in an 1D array */
  EigenValueArrayType * GetEigenValues()
  { return &m_EigenValues; }

  /** dummy method that calls the GenerateData method to
   * produce the eigen vectors and values. */
  void Update()
  { this->GenerateData(); }

protected:
  SymmetricEigenSystem();
  virtual ~SymmetricEigenSystem();
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Produces the eigen vectors and values. */
  void GenerateData();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SymmetricEigenSystem);

  /** the target matrix */
  MatrixType *m_Matrix;

  /** eigen vectors output */
  EigenVectorArrayType m_EigenVectors;

  /** eigen values output */
  EigenValueArrayType m_EigenValues;

  /** flag for absolute ordering of eigen vectors and
   * eigen values */
  bool m_UseAbsoluteOrder;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSymmetricEigenSystem.hxx"
#endif

#endif
