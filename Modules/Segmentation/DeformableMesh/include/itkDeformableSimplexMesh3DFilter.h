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
#ifndef itkDeformableSimplexMesh3DFilter_h
#define itkDeformableSimplexMesh3DFilter_h

#include "itkMeshToMeshFilter.h"
#include "itkSimplexMesh.h"
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkArray.h"

#include <set>

namespace itk
{
/**
 *\class DeformableSimplexMesh3DFilter
 * \brief Three-dimensional deformable model for image segmentation
 *
 * DeformableSimplexMesh3DFilter is a discrete three-dimensional deformable model, which
 * can be used to deform a 3-D SimplexMesh.
 *
 * The mesh deformation is constrained by internal forces. The internal force can be scaled
 * via SetAlpha (typical values are 0.01 < alpha < 0.3). The external force is derived from
 * the image one wants to delineate. Therefore an image of type GradientImageType needs to
 * be set by calling SetGradientImage(...). The external forces are scaled
 * via SetBeta (typical values are 0.01 < beta < 1). One still needs to play around with
 * these values.
 *
 * To control the smoothness of the mesh a rigidity parameter can be adjusted. Low values (1 or 0)
 * allow areas with high curvature. Higher values (around 7 or 8) will make the mesh smoother.
 *
 * By setting the gamma parameter the regularity of the mesh is controlled. Low values (< 0.03)
 * produce more regular mesh. Higher values ( 0.3 < gamma < 0.2) will allow to move the vertices to
 * regions of higher curvature.
 *
 * This approach for segmentation follows that of Delingette et al. (1997).
 *
 * This filter currently assumes that the spacing of the input image is 1.
 *
 * The user has to set the number of iterations for mesh evolution.
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 *
 * \ingroup MeshFilters
 * \ingroup MeshSegmentation
 * \ingroup ITKDeformableMesh
 */
template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT DeformableSimplexMesh3DFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DeformableSimplexMesh3DFilter);

  /** Standard "Self" type alias. */
  using Self = DeformableSimplexMesh3DFilter;

  /** Standard "Superclass" type alias. */
  using Superclass = MeshToMeshFilter<TInputMesh, TOutputMesh>;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformableSimplexMesh3DFilter, MeshToMeshFilter);

  /** Some type alias. */
  using InputMeshType = TInputMesh;
  using OutputMeshType = TOutputMesh;

  using InputPointsContainerPointer = typename InputMeshType::PointsContainerPointer;
  using InputPointsContainer = typename InputMeshType::PointsContainer;
  using InputPointsContainerConstIterator = typename InputMeshType::PointsContainer::ConstIterator;

  /** Other definitions. */
  using PointType = typename SimplexMeshGeometry::PointType;
  using VectorType = typename PointType::VectorType;
  using CovariantVectorType = CovariantVector<typename VectorType::ValueType, 3>;
  using PixelType = typename InputMeshType::PixelType;

  /** Image and Image iterator definition. */
  using GradientType = CovariantVector<PixelType, 3>;
  using GradientImageType = Image<GradientType, 3>;
  using BinaryOutput = Image<unsigned char, 3>;
  using MagnitudeOutput = Image<float, 3>;

  using GradientImagePointer = typename GradientImageType::Pointer;
  using GradientIndexType = typename GradientImageType::IndexType;
  using GradientPixelType = typename GradientImageType::PixelType;
  using GradientIndexValueType = typename GradientIndexType::IndexValueType;
  using GradientImageSizeType = typename GradientImageType::SizeType;

  /* Mesh pointer definition. */
  using InputMeshPointer = typename InputMeshType::Pointer;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  using MeshPointType = typename InputMeshType::PointType;
  using CellsContainerPointer = typename InputMeshType::CellsContainerPointer;
  using CellsContainerIterator = typename InputMeshType::CellsContainer::Iterator;
  using InputNeighbors = typename InputMeshType::NeighborListType;
  using InputNeighborsIterator = typename InputMeshType::NeighborListType::iterator;

  using NeighborSetType = std::set<IdentifierType>;
  using IndexSetType = std::set<IdentifierType>;
  using VertexNeighborListType = itk::MapContainer<IdentifierType, NeighborSetType>;
  using NeighborSetIterator = typename NeighborSetType::iterator;
  using IndexSetIterator = typename IndexSetType::iterator;

  using GeometryMapType = typename InputMeshType::GeometryMapType;
  using GeometryMapPointer = typename GeometryMapType::Pointer;
  using GeometryMapIterator = typename GeometryMapType::Iterator;

  /** Routines. */

  /** Set/Get routines. */

  /**
   *  Setter for gradient image
   */
  void
  SetGradient(const GradientImageType * gradientImage);

  /**
   *  Getter for gradient image
   */
  const GradientImageType *
  GetGradient() const;

  /**
   * Set number of iterations for deformation process
   */
  itkSetMacro(Iterations, int);
  itkGetConstMacro(Iterations, int);

  /** Set scalar factor for internal force */
  itkSetMacro(Alpha, double);

  /** Get internal force scaling factor */
  itkGetConstMacro(Alpha, double);

  /** Set external force scaling factor */
  itkSetMacro(Beta, double);

  /** Get external force scaling factor */
  itkGetConstMacro(Beta, double);

  /** Set reference metrics update scaling factor */
  itkSetMacro(Gamma, double);

  /** Get reference metrics update scaling factor */
  itkGetConstMacro(Gamma, double);

  /** Set reference metrics update scaling factor */
  itkSetMacro(Damping, double);

  /** Get reference metrics update scaling factor */
  itkGetConstMacro(Damping, double);

  /** control smoothness of the mesh */
  itkSetMacro(Rigidity, unsigned int);

  /** control smoothness of the mesh */
  itkGetConstMacro(Rigidity, unsigned int);

  itkSetObjectMacro(Data, GeometryMapType);
  itkGetModifiableObjectMacro(Data, GeometryMapType);

  /** Width, height and depth of image */
  itkGetConstMacro(ImageWidth, int);
  itkGetConstMacro(ImageHeight, int);
  itkGetConstMacro(ImageDepth, int);

  /** current iteration number */
  itkGetConstMacro(Step, int);

protected:
  DeformableSimplexMesh3DFilter();
  ~DeformableSimplexMesh3DFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** */
  void
  GenerateData() override;

  /**
   * Initializes the datastructures necessary for mesh
   * deformation with the values from the passed input
   * mesh.
   */
  virtual void
  Initialize();

  /**
   * Compute geometric properties like curvature and
   * normals, which are necessary for the computation
   * of the internal force components for each point of the mesh.
   */
  virtual void
  ComputeGeometry();

  /**
   * Computes the displacement of each point. Therefore
   * internal and external forces are computed and multiplied
   * by the constants (alpha and beta) set by the user.
   */
  virtual void
  ComputeDisplacement();

  /**
   * Compute the internal force component
   */
  virtual void
  ComputeInternalForce(SimplexMeshGeometry * data);

  /**
   * Compute the external force component
   * Pass in the gradient image, to avoid inner loop calls to GetGradient()
   */
  virtual void
  ComputeExternalForce(SimplexMeshGeometry * data, const GradientImageType * gradientImage);

  /**
   * At the and of the deformation the output mesh is created
   * by creating a new mesh
   */
  virtual void
  ComputeOutput();

  /**
   * Method updates the reference metrics for each mesh point
   */
  virtual void
  UpdateReferenceMetrics();

  /**
   *  L function implemented following the paper of Delingette
   */
  bool
  L_Func(const double r, const double d, const double phi, double & output);

  /**
   *  Method computes the barycentric coordinates of the passed point
   */
  PointType
  ComputeBarycentricCoordinates(PointType p, SimplexMeshGeometry * data);

  /** Parameters definitions. */

  /**
   * Scalar defining the influence of the internal forces
   * Values should lie between 0.001 and 0.3. Higher values
   * increase the stiffness of the mesh
   */
  double m_Alpha;

  /**
   * Scalar defining the influence of the external force components
   * The choice for this parameter strongly depends on the underlying
   * data. Typical value range from 0.00001 to 0.3
   *
   */
  double m_Beta;

  /**
   * Gamma influences the distribution of the mesh points. It should
   * lie between 0.01 and 0.2. Smaller values force the mesh to be
   * more regular. When increasing gamma, mesh points will have higher
   * density in places of high curvature.
   */
  double m_Gamma;
  double m_Damping;
  /**
   * This scalar determines the smoothness of the surface model. Values
   * should range from 0 to 10. It determines the radius of the neighborhood
   * during internal force computation using the curvature shape constraint.
   * The higher the rigidity the higher the smoothness.
   */
  unsigned int m_Rigidity;

  // definition of internal parameters
  /** Number of iterations */
  int m_Step;
  /** Image size */
  int m_ImageWidth;
  /** Image size */
  int m_ImageHeight;
  /** Image size */
  int m_ImageDepth;

  /** This threshold decides when to stop the model. */
  int m_Iterations;

  /**
   * map stores a Geometry object for every mesh point
   */
  GeometryMapPointer m_Data;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDeformableSimplexMesh3DFilter.hxx"
#endif

#endif // itkDeformableSimplexMesh3DFilter_h
