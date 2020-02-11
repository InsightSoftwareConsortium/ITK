/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkFEMScatteredDataPointSetToImageFilter_h
#define itkFEMScatteredDataPointSetToImageFilter_h

#include "itkPointSetToImageFilter.h"
#include "itkVectorContainer.h"
#include "vnl/vnl_matrix.h"

#include "itkMesh.h"
#include "itkFEMObject.h"
#include "itkFEMElement3DC0LinearTetrahedronStrain.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"
#include "itkFEMElement2DC0LinearTriangularStrain.h"
#include "itkFEMElement2DC0LinearQuadrilateralStrain.h"
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMRobustSolver.h"
#include "itkFEMLoadNoisyLandmark.h"
#include "itkImageToRectilinearFEMObjectFilter.h"
#include "itkTriangleCell.h"
#include "itkTetrahedronCell.h"
#include "itkQuadrilateralCell.h"
#include "itkHexahedronCell.h"

namespace itk
{
namespace fem
{
/** \class FEMScatteredDataPointSetToImageFilter
 * \brief Scattered data approximation to interpolation
 * in the presence of outliers
 *
 * Given a 2- or 3-D scattered and noisy data, this filter is able to approximate
 * the data while rejecting outliers, and then it advances toward interpolation.
 *
 * This filter also takes the confidence and structural information into account,
 * if users can provide a scalar to represent the confidence and a tensor
 * to represent the structural information for each feature point.
 *
 * This filter requires a point set, of which each point is associated with a
 * 2-D or 3-D displacement, as input, and outputs a deformation field. Two
 * optional point sets are supported.
 * Confidence point set: this point set defines the confidence associated with
 * each point, which will make the solver behavior like a weighted Least Square
 * Tensor point set: this point set defines a tensor associated with each point,
 * such as a structural tensor.
 *
 * The purpose of this filter is to facilitate the use of the FEMRobustSolver,
 * which does the concrete work on finding the solution. See FEMRobustSolver for
 * details.
 *
   \code

    const unsigned int ParametricDimension = 3;
    const unsigned int DataDimension = 3;

    using PixelType = int;
    using InputImageType = itk::Image<PixelType, ParametricDimension>;
    using RealType = float;
    using VectorType = itk::Vector<RealType, DataDimension>;
    using MatrixType = itk::Matrix<RealType, DataDimension,
    DataDimension>;
    using VectorImageType = itk::Image<VectorType, ParametricDimension>;
    using PointSetType = itk::PointSet <VectorType,
    ParametricDimension>;

    using TensorPointSetType = itk::PointSet <MatrixType,
     ParametricDimension>;

    using ConfidencePointSetType = itk::PointSet <RealType, ParametricDimension>;


    using MeshType = itk::Mesh< VectorType, ParametricDimension >;

    using FilterType = itk::FEMScatteredDataPointSetToImageFilter
    <PointSetType, MeshType, VectorImageType,
    ConfidencePointSetType, TensorPointSetType>;

    FilterType::Pointer filter = FilterType::New();

    filter->SetInput(displacementPointSet);
    filter->SetConfidencePointSet(confidencePointSet); //optional
    filter->SetTensorPointSet(tensorPointSet); //optional
    filter->SetMesh(aITKMesh);
    filter->Update();

    DeformationField::Pointer = filter->GetOutput();

   \endcode
 *
 * \author Yixun Liu
 *
 * \par REFERENCE
 * O. Clatz, H. Delingette, I.-F. Talos, A. Golby, R. Kikinis, F. Jolesz,
 * N. Ayache, and S. Warfield, "Robust non-rigid registration to capture brain
 * shift from intra-operative MRI", IEEE Trans. Med. Imag.,
 * 24(11);1417-27, 2005.
 *
 * \par REFERENCE
 * Yixun Liu, Andriy Fedorov, Ron Kikinis and Nikos Chrisochoides,
 * "Real-time Non-rigidRegistration of Medical Images on a Cooperative Parallel
 * Architecture", IEEE International Conference on Bioinformatics & Biomedicine,
 * pp. 401-404, November 2009.
 *
 * \sa FEMRobustSolver
 *
 * \ingroup ITKFEM
 */

template <typename TInputPointSet,
          typename TInputMesh,
          typename TOutputImage,
          typename TInputConfidencePointSet,
          typename TInputTensorPointSet>
class ITK_TEMPLATE_EXPORT FEMScatteredDataPointSetToImageFilter
  : public PointSetToImageFilter<TInputPointSet, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FEMScatteredDataPointSetToImageFilter);

  using Self = FEMScatteredDataPointSetToImageFilter;
  using Superclass = PointSetToImageFilter<TInputPointSet, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Extract dimension from the output image. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Displacement point set type alias support */
  using PointSetType = TInputPointSet;
  using PointType = typename PointSetType::PointType;
  using PointsContainer = typename PointSetType::PointsContainer;
  using PointsIterator = typename PointsContainer::ConstIterator;
  using PointDataType = typename PointSetType::PixelType;
  using PointDataContainerType = typename PointSetType::PointDataContainer;
  using PointDataIterator = typename PointDataContainerType::ConstIterator;

  /** Confidence point set type alias support */
  using ConfidencePointSetType = TInputConfidencePointSet;
  using ConfidencePointsIterator = typename ConfidencePointSetType::PointsContainer::ConstIterator;
  using ConfidencePointDataType = typename ConfidencePointSetType::PixelType;
  using ConfidencePointDataContainerType = typename ConfidencePointSetType::PointDataContainer;

  /** Tensor point set type alias support */
  using TensorPointSetType = TInputTensorPointSet;
  using TensorPointsIterator = typename TensorPointSetType::PointsContainer::ConstIterator;
  using TensorPointDataType = typename TensorPointSetType::PixelType;
  using TensorPointDataContainerType = typename TensorPointSetType::PointDataContainer;
  using TensorPointDataIterator = typename TensorPointDataContainerType::Iterator;

  /** Mesh type alias support */
  using MeshType = TInputMesh;
  using CellType = typename MeshType::CellType;
  using CellAutoPointer = typename CellType::CellAutoPointer;
  using CellsContainer = typename MeshType::CellsContainer;
  using CellIterator = typename CellsContainer::ConstIterator;

  using TriangleType = TriangleCell<CellType>;
  using TetrahedronType = TetrahedronCell<CellType>;
  using QuadrilateralType = QuadrilateralCell<CellType>;
  using HexahedronType = HexahedronCell<CellType>;
  using PointIdIterator = typename CellType::PointIdIterator;

  /** Image type alias support */
  using ImageType = TOutputImage;
  using PixelType = typename ImageType::PixelType;
  using RegionType = typename ImageType::RegionType;
  using SizeType = typename ImageType::SizeType;
  using IndexType = typename ImageType::IndexType;
  using SpacingType = typename ImageType::SpacingType;
  using ContinuousIndexType = ContinuousIndex<SpacePrecisionType, ImageDimension>;

  using ImageToRectilinearFEMObjectFilterType = ImageToRectilinearFEMObjectFilter<ImageType>;

  /** FEMObject type alias support */
  using FEMObjectType = FEMObject<ImageDimension>;

  /** FEM solver type alias support */
  using FEMSolverType = RobustSolver<ImageDimension>;

  /** FEM element type alias support */
  using FEMTetrahedronType = Element3DC0LinearTetrahedronStrain;
  using FEMHexahedronType = Element3DC0LinearHexahedronStrain;
  using FEM2DTriangleType = Element2DC0LinearTriangularStrain;
  using FEM2DQuadrilateralType = Element2DC0LinearQuadrilateralStrain;

  /** FEM node type alias support */
  using NodeType = Element::Node;

  /** FEM Load type alias support */
  using LoadType = LoadNoisyLandmark;

  /** FEM material type alias support */
  using MaterialType = MaterialLinearElasticity;
  using MaterialPointerType = MaterialType::Pointer;

  /** FEM element type alias support */
  using FEMVectorType = Element::VectorType;
  using FEMMatrixType = Element::MatrixType;

  /** FEM container type alias support */
  using LoadContainerType = typename FEMObjectType::LoadContainerType;
  using NodeContainerType = typename FEMObjectType::NodeContainerType;
  using ElementContainerType = typename FEMObjectType::ElementContainerType;
  using MaterialContainerType = typename FEMObjectType::MaterialContainerType;

  /** Helper functions */
  itkSetConstObjectMacro(ConfidencePointSet, ConfidencePointSetType);

  itkSetConstObjectMacro(TensorPointSet, TensorPointSetType);

  itkSetObjectMacro(Mesh, MeshType);
  itkGetModifiableObjectMacro(Mesh, MeshType);

  itkSetObjectMacro(FEMSolver, FEMSolverType);
  itkGetModifiableObjectMacro(FEMSolver, FEMSolverType);

  /** Get/Set the number of voxels/pixels in each dimension used during the mesh generation */
  itkGetConstReferenceMacro(PixelsPerElement, ContinuousIndexType);
  itkSetMacro(PixelsPerElement, ContinuousIndexType);

  /** Set/Get the spacing of the rectilinear element */
  void
  SetElementSpacing(const SpacingType & elementSpacing);
  itkGetConstReferenceMacro(SpacingPerElement, SpacingType);

  /** Get the number of element in each dimension of the generated mesh */
  itkGetConstReferenceMacro(NumberOfElements, SizeType);

protected:
  FEMScatteredDataPointSetToImageFilter();
  ~FEMScatteredDataPointSetToImageFilter() override;

  /** Generate 2D/3D rectilinear mesh */
  void
  GenerateRectilinearMesh();

  /** Generate a 2D quadrilateral mesh */
  void
  Generate2DQuadrilateralMesh();

  /** generate 3D hexahedral mesh */
  void
  Generate3DHexahedralMesh();

  /** Initialize FEMObject from a mesh and feature points */
  void
  InitializeFEMObject(FEMObjectType * femObject);

  /** Initialize Materials */
  void
  InitializeMaterials(FEMObjectType * femObject);

  /** Initialize Nodes */
  void
  InitializeNodes(FEMObjectType * femObject);

  /** Initialize Elements */
  void
  InitializeElements(FEMObjectType * femObject);

  /** Initialize Loads */
  void
  InitializeLoads(FEMObjectType * femObject);

  /** Run the solver and call ProduceDeformationField to produce deformation field */
  void
  GenerateData() override;

  void
  ProduceDeformationField();

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  typename FEMObjectType::Pointer      m_FEMObject;
  typename FEMSolverType::Pointer      m_FEMSolver;
  typename FEMSolverType::ConstPointer m_FEMDeformedObject;
  typename MeshType::Pointer           m_Mesh;

  typename ConfidencePointSetType::ConstPointer m_ConfidencePointSet;
  typename TensorPointSetType::ConstPointer     m_TensorPointSet;

  /** Rectilinear mesh */
  SizeType            m_NumberOfElements;
  ContinuousIndexType m_PixelsPerElement;
  SpacingType         m_SpacingPerElement;

  /** Material */
  MaterialPointerType m_Material;
};

} // end namespace fem
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFEMScatteredDataPointSetToImageFilter.hxx"
#endif

#endif // itkFEMScatteredDataPointSetToImageFilter_h
