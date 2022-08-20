/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#ifndef itkTriangleMeshCurvatureCalculator_h
#define itkTriangleMeshCurvatureCalculator_h

#include <iostream>
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkVectorContainer.h"

namespace itk
{
/** \class TriangleMeshCurvatureCalculatorEnum
 * \brief Different modes of operation. Currently only Gaussian curvature is supported.
 * Using enum for future extension to Mean, Max and Min curvature.
 * \ingroup ITKMesh
 */
class TriangleMeshCurvatureCalculatorEnums
{
public:
  enum class Curvatures : uint8_t
  {
    GaussCurvature,
    MeanCurvature,
    MinCurvature,
    MaxCurvature
  };
};

extern ITKMesh_EXPORT std::ostream &
                      operator<<(std::ostream & out, const TriangleMeshCurvatureCalculatorEnums::Curvatures value);

/**  \class TriangleMeshCurvatureCalculator
 * \brief
 * Calculator to compute curvature of a triangle mesh. Set the input triangle mesh and the
 * required curvature type first. Default curvature type is Gauss. After computing curvature the result
 * can be obtained using the getter method. It throws exception if the input mesh is not set.
 * The implementation is the same as in VTK.
 * \ingroup ITKMesh
 */
template <typename TInputMesh>
class ITK_TEMPLATE_EXPORT TriangleMeshCurvatureCalculator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TriangleMeshCurvatureCalculator);

  /** Standard class type aliases. */
  using Self = TriangleMeshCurvatureCalculator;

  /** Standard "Superclass" type alias. */
  using Superclass = Object;

  /** Smart pointer type alias support. */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TriangleMeshCurvatureCalculator, Object);

  /** Convenient type alias for this Calculator. */
  using InputMeshType = TInputMesh;
  using PointIdentifier = typename InputMeshType::PointIdentifier;
  using MeshConstPointer = typename InputMeshType::ConstPointer;
  using MeshPointType = typename InputMeshType::PointType;
  using MeshPointIdConstIterator = typename InputMeshType::CellTraits::PointIdConstIterator;
  using MeshPointsContainerConstPointer = typename InputMeshType::PointsContainerConstPointer;
  using CellsContainerConstPointer = typename InputMeshType::CellsContainerConstPointer;
  using CellsContainerConstIterator = typename InputMeshType::CellsContainer::ConstIterator;
  using CellType = typename InputMeshType::CellType;
  using TriangleCellType = itk::TriangleCell<CellType>;
  using DoubleVectorContainer = typename itk::VectorContainer<PointIdentifier, double>;
  using DoubleVectorContainerPointer = typename DoubleVectorContainer::Pointer;
  using CurvaturesEnum = TriangleMeshCurvatureCalculatorEnums::Curvatures;

  /** Methods specify mode of operation for the calculator. */
  itkSetMacro(CurvatureType, TriangleMeshCurvatureCalculatorEnums::Curvatures);
  itkGetConstMacro(CurvatureType, TriangleMeshCurvatureCalculatorEnums::Curvatures);

  /** Set the input mesh. */
  itkSetObjectMacro(TriangleMesh, InputMeshType);

  /** Get Gauss curvature. */
  itkGetConstObjectMacro(GaussCurvatureData, DoubleVectorContainer);

  /** Set the curvature type to Gauss. */
  void
  SetCurvatureTypeToGaussian()
  {
    this->SetCurvatureType(TriangleMeshCurvatureCalculatorEnums::Curvatures::GaussCurvature);
  }

  /** Compute the selected curvature type. */
  void
  Compute();

protected:
  TriangleMeshCurvatureCalculator() = default;
  ~TriangleMeshCurvatureCalculator() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Discrete Gauss curvature (K) computation */
  void
  ComputeGaussCurvature(const InputMeshType * input);

private:
  CurvaturesEnum               m_CurvatureType = CurvaturesEnum::GaussCurvature;
  DoubleVectorContainerPointer m_GaussCurvatureData;
  MeshConstPointer             m_TriangleMesh;
};

} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTriangleMeshCurvatureCalculator.hxx"
#endif

#endif
