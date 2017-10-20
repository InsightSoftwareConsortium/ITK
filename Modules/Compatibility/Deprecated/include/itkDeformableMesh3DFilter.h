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
#ifndef itkDeformableMesh3DFilter_h
#define itkDeformableMesh3DFilter_h
#if !defined( ITK_LEGACY_REMOVE )

#include "itkMeshToMeshFilter.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkMath.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkCovariantVector.h"

namespace itk
{
/** \class DeformableMesh3D
 * \brief
 *
 * The DeformableMesh3DFilter is used to deform a mesh (deformable model)
 * under a potential force in 2D or 3D.
 * The potential force is derived from the gradient information in the
 * medical image and it will make the model deform to fit to the boundary
 * features.
 *
 * Computations are handled with points assumed to lie in index space,
 * not physical space. Reverting to physical space compromises backward
 * compatibility.
 *
 * Inputs are:
 *  (1) A deformable triangular model (Mesh). Import using the SetInput method.
 *  (2) A gradient map that make the model deform to fit to the estimated boundary.
 *  the gradient should be based on the 2nd derive of the original image. So the
 *  nodes on the model surface will stop at the edge features in the original
 *  image.
 *
 * \deprecated
 * \ingroup ITKDeprecated
 * \ingroup MeshFilters
 * \ingroup MeshSegmentation
 */
template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT DeformableMesh3DFilter:public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard "Self" typedef. */
  typedef DeformableMesh3DFilter Self;

  /** Standard "Superclass" typedef. */
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformableMesh3DFilter, MeshToMeshFilter);

  /** Some typedefs. */
  typedef TInputMesh                                               InputMeshType;
  typedef typename InputMeshType::PointsContainerPointer           InputPointsContainerPointer;
  typedef typename InputMeshType::PointsContainerConstPointer      InputPointsContainerConstPointer;
  typedef typename InputMeshType::PointsContainer::Iterator        InputPointsContainerIterator;
  typedef typename InputMeshType::PointsContainer::ConstIterator   InputPointsContainerConstIterator;
  typedef typename InputMeshType::PointDataContainerPointer        InputPointDataContainerPointer;
  typedef typename InputMeshType::PointDataContainer::Iterator     InputPointDataContainerIterator;
  typedef typename InputMeshType::CellsContainerPointer            InputCellsContainerPointer;
  typedef typename InputMeshType::CellsContainer::Iterator         InputCellsContainerIterator;
  typedef typename InputMeshType::CellsContainerConstPointer       InputCellsContainerConstPointer;
  typedef typename InputMeshType::CellsContainer::ConstIterator    InputCellsContainerConstIterator;
  typedef typename InputMeshType::CellDataContainerPointer         InputCellDataContainerPointer;
  typedef typename InputMeshType::CellDataContainer::Iterator      InputCellDataContainerIterator;
  typedef typename InputMeshType::CellDataContainerConstPointer    InputCellDataContainerConstPointer;
  typedef typename InputMeshType::CellDataContainer::ConstIterator InputCellDataContainerConstIterator;

  typedef TOutputMesh                                        OutputMeshType;
  typedef typename OutputMeshType::PointsContainerPointer    OutputPointsContainerPointer;
  typedef typename OutputMeshType::CellsContainer            OutputCellsContainer;
  typedef typename OutputMeshType::CellsContainerPointer     OutputCellsContainerPointer;
  typedef typename OutputMeshType::PointsContainer::Iterator OutputPointsContainerIterator;

  /** Other definitions. */
  typedef typename InputMeshType::CellType   CellType;
  typedef typename InputMeshType::CellTraits CellTraits;
  typedef typename InputMeshType::PointType  InputPointType;
  typedef typename InputMeshType::PixelType  PixelType;

  /** Image and Image iterator definition. */
  typedef Image< unsigned char, 3 >                 PotentialImageType;
  typedef ImageRegionIterator< PotentialImageType > PotentialIterator;
  typedef CovariantVector< PixelType, 3 >           GradientType;
  typedef Image< GradientType, 3 >                  GradientImageType;
  typedef ImageRegionIterator< GradientImageType >  GradientIterator;
  typedef typename GradientImageType::SizeType      ImageSizeType;
  typedef typename GradientImageType::IndexType     ImageIndexType;

  typedef itk::CellInterface< PixelType, CellTraits > TCellInterface;
  typedef itk::TriangleCell< TCellInterface >         TriCell;

  typedef CovariantVector< int, 3 >    int3DVector;
  typedef CovariantVector< double, 2 > double2DVector;
  typedef CovariantVector< double, 3 > double3DVector;

  /* Mesh pointer definition. */
  typedef typename InputMeshType::Pointer      InputMeshPointer;
  typedef typename InputMeshType::ConstPointer InputMeshConstPointer;
  typedef typename OutputMeshType::Pointer     OutputMeshPointer;
  typedef typename GradientImageType::Pointer  GradientImagePointer;

  /* Stiffness Matrix Type definition */
  typedef vnl_matrix_fixed< double, 4, 4 > StiffnessMatrixType;
  typedef StiffnessMatrixType *            StiffnessMatrixRawPointer;

  /** Routines. */
  void SetStiffnessMatrix(StiffnessMatrixType *stiff, int i);

  /** Set/Get routines. */
  itkSetMacro(Gradient, GradientImagePointer);
  itkGetConstMacro(Gradient, GradientImagePointer);

  itkSetMacro(StepThreshold, int);
  itkGetConstMacro(StepThreshold, int);

  itkSetMacro(Stiffness, double2DVector);
  itkGetConstMacro(Stiffness, double2DVector);

  itkSetMacro(TimeStep, double);
  itkGetConstMacro(TimeStep, double);

  itkSetMacro(Scale, double3DVector);

  itkSetMacro(PotentialMagnitude, PixelType);
  itkSetMacro(GradientMagnitude, PixelType);
  itkSetMacro(PotentialOn, unsigned short);
  itkSetMacro(ObjectLabel, unsigned char);

  itkGetConstMacro(Normals, InputMeshPointer);

protected:
  DeformableMesh3DFilter();
  ~DeformableMesh3DFilter();
  ITK_DISALLOW_COPY_AND_ASSIGN(DeformableMesh3DFilter);
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

private:
  /** Meshes for Forces,Displacement,Normals, etc. */
  InputMeshPointer m_Forces;
  InputMeshPointer m_Normals;
  InputMeshPointer m_Displacements;
  InputMeshPointer m_Derives;
  InputMeshPointer m_Locations;

  void Initialize();

  void SetDefaultStiffnessMatrix();

  void SetMeshStiffness();

  void Advance();     // update data for next iteration

  void ComputeDt();   // compute point positions

  void ComputeOutput();

  void GradientFit(); // fit the model with gradient information

  void ComputeNormals();

  void PotentialFit();

  /** Three different kinds of stiffness matrix. */
  StiffnessMatrixType        m_StiffnessMatrix[10];
  StiffnessMatrixRawPointer *m_K;

  /** Parameters definitions. */
  double2DVector m_Stiffness;
  double         m_TimeStep;         /** Time step of each iteration */
  double3DVector m_Scale;

  int m_Step;                /** Number of iterations */
  int m_NumberOfNodes;
  int m_NumberOfCells;
  int m_ImageWidth;            /** Image size */
  int m_ImageHeight;
  int m_ImageDepth;
  int m_StepThreshold;      /** This threshold decides when to stop the model.
                              */

  unsigned short m_ModelXUpLimit;
  unsigned short m_ModelXDownLimit;
  unsigned short m_ModelYUpLimit;
  unsigned short m_ModelYDownLimit;
  unsigned short m_ModelZUpLimit;
  unsigned short m_ModelZDownLimit;
  unsigned short m_PotentialOn;
  unsigned char  m_ObjectLabel;
  PixelType      m_GradientMagnitude;
  PixelType      m_PotentialMagnitude;

  /** To compute force derived from gradient data. */
  GradientImagePointer        m_Gradient;
  PotentialImageType::Pointer m_Potential;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableMesh3DFilter.hxx"
#endif

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
