/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableMesh3DFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
  typedef TriangleCell<PixelType, CellTraits>  TriCell;

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
  void SliceAddition(int i);
  void NodesRearrange();
  void GapSearch();
  void GradientFit(); // fit the model with gradient information
  void ComputeNormals();
  void ACDSearch();   // remove weird structures on the model surface
  void ComputeShrinkForce();  // force in case we shrink the model
  void InitialFit();          // locate the model near the objects

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

  itkSetMacro(StepThreshold1, int);
  itkSetMacro(StepThreshold2, int);

  itkSetMacro(XResolution, int);
  itkSetMacro(YResolution, int);
  itkSetMacro(ZResolution, int);

  itkSetMacro(StiffnessV, double);
  itkSetMacro(StiffnessH, double);

  itkSetMacro(TimeStep, double);

  itkSetMacro(Center, IndexType);

  itkGetMacro(Normals, InputMeshPointer);

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
  int       m_StepThreshold1;/** This threshold decides when to switch from potential fit to gradient fit. */
  int       m_StepThreshold2;/** This threshold decides when to stop the model. */
  int       m_FirstSlice;    /** This variable helps relocating the model when trying to load it. */ 
  int       m_NeighborRadius;/** Defines the spatial range for the gradient fit */
 unsigned short        m_ObjectLabel;

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
