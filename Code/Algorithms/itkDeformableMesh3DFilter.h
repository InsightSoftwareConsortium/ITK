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
 * under a balloon force and a potential force in 2D or 3D.
 * The balloon force is oriented along the normal of the surface of the model. 
 * The potential force is derived from the estimated boundary positions.
 * These two force will cancel each other at the boundary positions, stopping the 
 * deformation of the model at this locations
 * Input Parameters are:
 *  (1) A deformable model (Mesh). This mesh is input using the SetInput method.
 *  (2) A filter with the potential map volume that provides 
 *      the estimated bounday positions. This map is a binary volume with 
 *      object and background labelled differently (equivalent to a mask).
 *      Such potential map can be derived from GibbsPriorFilter or any other
 *      segmentation filters.
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
  typedef typename InputMeshType::PointType         InputPointType;
  typedef typename InputMeshType::PixelType         PixelType;
  typedef CellInterface<PixelType,CellTraits>       CellInterfaceType;
  typedef TriangleCell<CellInterfaceType>           TriCell;

  /** Image and Image iterator definition. */
  typedef CovariantVector<PixelType, 3>        GradientType;
  typedef Image<GradientType, 3>               GradientImageType;
  typedef Image<unsigned short, 3>             ImageType;
  typedef SimpleImageRegionIterator<ImageType> ImageIterator;
  typedef SimpleImageRegionIterator<GradientImageType> GradientIterator;
  typedef ImageType::SizeType PotentialSizeType;

  /* Mesh pointer definition. */
  typedef typename InputMeshType::Pointer   InputMeshPointer;
  typedef typename OutputMeshType::Pointer  OutputMeshPointer;
  typedef typename ImageType::Pointer       ImagePointer;
  typedef typename ImageType::IndexType     IndexType;
  typedef typename GradientImageType::Pointer       GradientImagePointer;

  /** Routines. */
  void ComputeForce();
  void Initialize();
  void SetStiffnessMatrix();
  void Advance();     // update data for next iteration
  void Reset();       // reset all data
  void ComputeDt();   // compute point positions
  void ComputeOutput();
  void NodeAddition();
  void SliceAddition(int i, double dis);
  void Resample();    // resample the nodes on model
  void GapSearch();
  void GradientFit(); // fit the model with gradient information
  void ComputeNormals();
  void ACDSearch();   // remove weird structures on the model surface
  void ComputeShrinkForce();  // force in case we shrink the model
  void InitialFit();          // locate the model near the objects
  void ComputeSliceDistance(int i, int j); /** compute the distance between 2 
                                            *  neighboring slices, add 1 slcie 
                                            *  if the distance is larger than 
                                            *  m_SliceDistanceThreshold. */

  /** Set/Get routines. */
  itkSetMacro(ImageOutput, ImagePointer);
  itkGetMacro(ImageOutput, ImagePointer);

  itkSetMacro(Potential, ImagePointer);
  itkGetMacro(Potential, ImagePointer);

  itkSetMacro(Gradient, GradientImagePointer);
  itkGetMacro(Gradient, GradientImagePointer);

  itkSetMacro(FirstSlice, int);
  itkGetMacro(FirstSlice, int);

  itkSetMacro(NeighborRadius, int);

  itkSetMacro(StepThreshold, int);

  itkSetMacro(XResolution, int);
  itkSetMacro(YResolution, int);
  itkSetMacro(ZResolution, int);

  itkSetMacro(StiffnessV, double);
  itkSetMacro(StiffnessH, double);

  itkSetMacro(TimeStep, double);

  itkSetMacro(ZDistance, double);

  itkSetMacro(Center, IndexType);

  itkGetMacro(Normals, InputMeshPointer);

  itkSetMacro(SliceDistanceThreshold, double);

  itkSetMacro(ModelDistanceToBoundaryThreshold, double);

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

  /** Three different kinds of stiffness matrix. */
  vnl_matrix_fixed<double, 4, 4> NStiffness;
  vnl_matrix_fixed<double, 4, 4> SStiffness;
  vnl_matrix_fixed<double, 4, 4> CStiffness;
  vnl_matrix_fixed<double, 4, 4> **K;
  
  /** Parameters definitions. */
  double    m_StiffnessV, m_StiffnessH;
  double    m_TimeStep;      /** Time step of each iteration */
  int       m_XResolution, m_YResolution, m_ZResolution;
  IndexType m_Center;
  double    m_MiniT;         /** To stop nodes near a boundary position. */
  int       m_Step;          /** Nnumber of iterations */
  int       m_NumNodes;
  int       m_NumCells;
  int       m_NumNewNodes;   /** To add new nodes. */
  int       *m_GapLocations;
  int       m_NewNode;
  int       m_NewNodesExisted;
  int       m_NewNodeLimit;
  int       m_ImageWidth;      /** Image size */
  int       m_ImageHeight;
  int       m_ImageDepth;
  int       m_ModelXUpLimit; /** Records the size of the model. */
  int       m_ModelXDownLimit;
  int       m_ModelYUpLimit;
  int       m_ModelYDownLimit;
  int       m_ModelZUpLimit;
  int       m_ModelZDownLimit;
  int       **m_ACD;         /** To remove unstable structures on the model surface. */
  int       m_ModelRestart;
  int       m_StepThreshold;/** This threshold decides when to stop the model. */
  int       m_FirstSlice;    /** This variable helps relocating the model when trying to load it. */ 
  int       m_NeighborRadius;/** Defines the spatial range for the gradient fit */
  unsigned short   m_ObjectLabel;
  double    m_ZDistance;     /** the distance between 2 image slices (in pixels) */
  double    m_ModelDistanceToBoundaryThreshold;
  double    m_ModelDistanceToBoundary;
  double    m_SliceDistanceThreshold;

  /** To compute force derived from potential data. */
  ImagePointer          m_Potential;
  /** To compute force derived from gradient data. */
  GradientImagePointer  m_Gradient; 
  /**  Output a binary mask with segmented object. */
  ImagePointer          m_ImageOutput; 
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableMesh3DFilter.txx"
#endif

#endif
