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
=========================================================================*/

#ifndef itkVectorFieldPCA_h
#define itkVectorFieldPCA_h

#include "itkObject.h"
#include "itkPointSet.h"
#include "itkKernelFunctionBase.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class VectorFieldPCA
 * \brief Produce the principle components of a vector valued function.
 *
 * This calculator produces a set of basis functions composed of the
 * principal components of a set of vector valued functions.
 *
 * Specify an itk::KernelFunction for Kernel PCA.  The Kernel Function
 * can take as input an optional point set.
 *
 * This class is templated over the types of the vector valued functions,
 * the output point types, and optionally the point set type.
 *
 * \author Michael Bowers, Laurent Younes
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Principal Components Analysis of Scalar, Vector, and Mesh Vertex Data"
 * http://www.insight-journal.org/browse/publication/878
 *
 * \ingroup ITKStatistics
 * \ingroup PrincipalComponentsAnalysis
 */

template <typename TRealValueType = double>
class ITK_TEMPLATE_EXPORT GaussianDistanceKernel : public KernelFunctionBase<TRealValueType>
{
public:
  /** Standard class type alias. */
  using Self = GaussianDistanceKernel;
  using Superclass = KernelFunctionBase<TRealValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianDistanceKernel, KernelFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /**
   * \brief Set and get the Kernel sigma.
   */
  void
  SetKernelSigma(double s)
  {
    m_KernelSigma = s;
    m_OneOverMinusTwoSigmaSqr = -1.0 / (2.0 * s * s);
  }
  itkGetMacro(KernelSigma, double);

  /**
   * \brief Evaluate the function. Input is the squared distance
   */
  inline TRealValueType
  Evaluate(const TRealValueType & u) const override
  {
    return (std::exp(u * m_OneOverMinusTwoSigmaSqr));
  }

protected:
  GaussianDistanceKernel() = default;
  ~GaussianDistanceKernel() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }

private:
  double m_KernelSigma;
  double m_OneOverMinusTwoSigmaSqr;
};

template <typename TVectorFieldElementType,
          typename TPCType,
          typename TPointSetPixelType = float,
          typename TPointSetCoordRepType = float,
          typename KernelFunctionType = KernelFunctionBase<TPointSetCoordRepType>,
          class TPointSetType =
            PointSet<TPointSetPixelType, 3, DefaultStaticMeshTraits<TPointSetPixelType, 3, 3, TPointSetCoordRepType>>>
class ITK_TEMPLATE_EXPORT VectorFieldPCA : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorFieldPCA);

  /** Standard class type alias. */
  using Self = VectorFieldPCA;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorFieldPCA, Object);

  /** Type definitions for the PointSet. */
  using InputPointSetType = TPointSetType;

  /** Definitions for points of the PointSet. */
  using InputPointType = typename InputPointSetType::PointType;

  /** Definitions for the PointsContainer. */
  using PointsContainer = typename InputPointSetType::PointsContainer;
  using PointsContainerIterator = typename PointsContainer::Iterator;

  /** Pointer types for the PointSet. */
  using InputPointSetPointer = typename InputPointSetType::Pointer;

  /** Const Pointer type for the PointSet. */
  using InputPointSetConstPointer = typename InputPointSetType::ConstPointer;

  /**
   * \brief Input PointSet dimension
   */
  itkStaticConstMacro(InputMeshDimension, unsigned int, TPointSetType::PointDimension);

  /** type for the vector fields. */
  using VectorFieldType = vnl_matrix<TVectorFieldElementType>;
  using VectorFieldSetType = VectorContainer<unsigned int, VectorFieldType>;

  using VectorFieldSetTypePointer = typename VectorFieldSetType::Pointer;
  using VectorFieldSetTypeConstPointer = typename VectorFieldSetType::ConstPointer;

  /** types for the output. */
  using MatrixType = vnl_matrix<TPCType>;
  using VectorType = vnl_vector<TPCType>;

  using BasisSetType = VectorContainer<unsigned int, MatrixType>;
  using ResSetType = VectorContainer<unsigned int, VectorType>;

  using BasisSetTypePointer = typename BasisSetType::Pointer;
  using KernelFunctionPointer = typename KernelFunctionType::Pointer;

  /**
   * \brief Set and get the input point set.
   */
  itkSetMacro(PointSet, InputPointSetPointer);
  itkGetMacro(PointSet, InputPointSetPointer);

  /**
   * \brief Set and get the vector fields for the analysis.
   */
  itkSetMacro(VectorFieldSet, VectorFieldSetTypePointer);
  itkGetMacro(VectorFieldSet, VectorFieldSetTypePointer);

  /**
   * \brief Set and get the PCA count.
   */
  itkSetMacro(ComponentCount, unsigned int);
  itkGetMacro(ComponentCount, unsigned int);

  /**
   * \brief Set pointer to the Kernel object.
   */
  itkSetMacro(KernelFunction, KernelFunctionPointer);

  /**
  * \brief Compute the PCA decomposition of the input point set.
      If a Kernel and a Kernel Sigma are set ,
      the calculator will perform Kernel PCA.
  */
  void
  Compute();

  /**
   * \brief Return the results.
   */
  itkGetConstReferenceMacro(AveVectorField, MatrixType);
  itkGetConstReferenceMacro(PCAEigenValues, VectorType);
  itkGetConstObjectMacro(BasisVectors, BasisSetType);

protected:
  VectorFieldPCA();
  ~VectorFieldPCA() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Kernel PCA. */
  void
  KernelPCA();

  /** Compute Momentum SCP. */
  void
  ComputeMomentumSCP();

private:
  VectorType m_PCAEigenValues;

  BasisSetTypePointer       m_BasisVectors;
  VectorFieldSetTypePointer m_VectorFieldSet;
  InputPointSetPointer      m_PointSet;
  KernelFunctionPointer     m_KernelFunction;

  // Problem dimensions
  unsigned int m_ComponentCount{ 0 };
  unsigned int m_SetSize{ 0 };
  unsigned int m_VectorDimCount{ 0 };
  unsigned int m_VertexCount{ 0 };
  unsigned int m_PointDim{ 0 };

  MatrixType m_V0;
  MatrixType m_AveVectorField;
  MatrixType m_K;

  bool m_PCACalculated{ false };
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorFieldPCA.hxx"
#endif

#endif
