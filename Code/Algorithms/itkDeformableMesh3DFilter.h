/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableMesh3DFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformableMesh3DFilter_h
#define __itkDeformableMesh3DFilter_h

#include "itkMeshToMeshFilter.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_math.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkTriangleCell.h"
#include "itkImage.h"
#include "itkSimpleImageRegionIterator.h"
#include <itkCovariantVector.h>

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
 * Inputs are:
 *  (1) A deformable triangular model (Mesh). Import using the SetInput method.
 *  (2) A potential map volume that provides the estimated bounday positions. 
 *      This map is a binary volume with object and background labelled differently
 *      (equivalent to a mask). Such potential map can be derived from 
 *      GibbsPriorFilter or any other segmentation filters.
 * Once the nodes of the model are stopped at the estimated boundary locations,
 * the gradient force is turned on, using the GradientFit method, 
 * to refine the nodes positions using the gradient information from the 
 * original data.
 *
 * \ingroup MeshFilters
 * \ingroup MeshSegmentation */
template <class TInputMesh, class TOutputMesh>
class DeformableMesh3DFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  /** Standard "Self" typedef. */
  typedef DeformableMesh3DFilter  Self;

  /** Standard "Superclass" typedef. */
  typedef MeshToMeshFilter<TInputMesh, TOutputMesh> Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformableMesh3DFilter,MeshToMeshFilter);

  /** Some typedefs. */
  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  typedef typename InputMeshType::PointsContainerPointer
      InputPointsContainerPointer;
  typedef typename InputMeshType::PointsContainer::Iterator
      InputPointsContainerIterator;
  typedef typename InputMeshType::PointDataContainerPointer
      InputPointDataContainerPointer;
  typedef typename InputMeshType::PointDataContainer::Iterator
      InputPointDataContainerIterator;
  typedef typename InputMeshType::CellsContainerPointer
      InputCellsContainerPointer;
  typedef typename InputMeshType::CellsContainer::Iterator
      InputCellsContainerIterator;
  typedef typename InputMeshType::CellDataContainerPointer
      InputCellDataContainerPointer;
  typedef typename InputMeshType::CellDataContainer::Iterator
      InputCellDataContainerIterator;
  typedef typename OutputMeshType::PointsContainerPointer
      OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainer::Iterator
      OutputPointsContainerIterator;

  /** Other definitions. */
  typedef typename InputMeshType::Cell              Cell;
  typedef typename InputMeshType::CellTraits        CellTraits;
  typedef itk::CellInterface<double, CellTraits>    TCellInterface;
  typedef itk::TriangleCell<TCellInterface>         TriCell;
  typedef typename InputMeshType::PointType         InputPointType;
  typedef typename InputMeshType::PixelType         PixelType;

  /** Image and Image iterator definition. */
  typedef CovariantVector<PixelType, 3>                GradientType;
  typedef Image<GradientType, 3>                       GradientImageType;
  typedef SimpleImageRegionIterator<GradientImageType> GradientIterator;
  typedef typename GradientImageType::SizeType         ImageSizeType;
  typedef typename GradientImageType::IndexType        ImageIndexType;
  typedef CovariantVector<int, 3>                      int3DVector;
  typedef CovariantVector<double, 2>                   double2DVector;
  typedef CovariantVector<double, 3>                   double3DVector;

  /* Mesh pointer definition. */
  typedef typename InputMeshType::Pointer     InputMeshPointer;
  typedef typename OutputMeshType::Pointer    OutputMeshPointer;
  typedef typename GradientImageType::Pointer GradientImagePointer;

  /** Routines. */
  void SetStiffnessMatrix( vnl_matrix_fixed<double, 4, 4> *stiff, int i );

  /** Set/Get routines. */
  itkSetMacro(Gradient, GradientImagePointer);
  itkGetMacro(Gradient, GradientImagePointer);

  itkSetMacro(StepThreshold, int);
  itkGetMacro(StepThreshold, int);

  itkSetMacro(Resolution, int3DVector);
  itkGetMacro(Resolution, int3DVector);

  itkSetMacro(Stiffness, double2DVector);
  itkGetMacro(Stiffness, double2DVector);

  itkSetMacro(TimeStep, double);
  itkGetMacro(TimeStep, double);

  itkSetMacro(NormalUpdate, unsigned char);
  itkGetMacro(NormalUpdate, unsigned char);

  itkSetMacro(Center, ImageIndexType);

  itkSetMacro(Scale, double3DVector);

protected:
  DeformableMesh3DFilter();
  ~DeformableMesh3DFilter() {}
  DeformableMesh3DFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateData();

private:
  /** Meshes for Forces,Displacement,Normals, etc. */
  InputMeshPointer  m_Forces; 
  InputMeshPointer  m_Normals;
  InputMeshPointer  m_Displacements;
  InputMeshPointer  m_Derives;
  InputMeshPointer  m_Locations;
  InputMeshPointer  m_Input;
  OutputMeshPointer m_Output;

  void Initialize();
  void SetDefaultStiffnessMatrix();
  void SetMeshStiffness();;
  void Advance();     // update data for next iteration
  void ComputeDt();   // compute point positions
  void ComputeOutput();
  void GradientFit(); // fit the model with gradient information
  void ComputeNormals();

  /** Three different kinds of stiffness matrix. */
  vnl_matrix_fixed<double, 4, 4> Stiffness[10];
  vnl_matrix_fixed<double, 4, 4> **K;
  
  /** Parameters definitions. */
  double2DVector    m_Stiffness;
  double            m_TimeStep;      /** Time step of each iteration */
  int3DVector       m_Resolution;
  ImageIndexType    m_Center;
  double3DVector    m_Scale;
  double    m_MiniT;         /** To stop nodes near a boundary position. */
  int       m_Step;          /** Nnumber of iterations */
  int       m_NumNodes;
  int       m_NumCells;
  int       m_ImageWidth;      /** Image size */
  int       m_ImageHeight;
  int       m_ImageDepth;
  int       m_StepThreshold;/** This threshold decides when to stop the model. */
  unsigned short m_ModelXUpLimit;
  unsigned short m_ModelXDownLimit;
  unsigned short m_ModelYUpLimit;
  unsigned short m_ModelYDownLimit;
  unsigned short m_ModelZUpLimit;
  unsigned short m_ModelZDownLimit;
  unsigned char     m_NormalUpdate;

  /** To compute force derived from gradient data. */
  GradientImagePointer  m_Gradient; 
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableMesh3DFilter.txx"
#endif

#endif
