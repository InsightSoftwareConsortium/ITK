/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkBalloonForceFilter_h
#define __itkBalloonForceFilter_h

#include "itkMeshToMeshFilter.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_math.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkTriangleCell.h"
#include "itkImage.h"

namespace itk
{

/** \class MeshToMeshFilter
 * \brief 
 *
 * MeshToMeshFilter is the base class for all process objects that output
 * mesh data, and require mesh data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 */
template <class TInputMesh, class TOutputMesh>
class ITK_EXPORT BalloonForceFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef BalloonForceFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef MeshToMeshFilter<TInputMesh, TOutputMesh> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(BalloonForceFilter,MeshToMeshFilter);

  /** 
   * Some typedefs.
   */
  typedef TInputMesh InputMeshType;
  typedef itk::Image<unsigned short, 3> ImageType;
  typedef typename InputMeshType::Pointer InputMeshPointer;
  typedef typename ImageType::Pointer ImagePointer;
  typedef typename ImageType::IndexType IndexType;

  typedef itk::Vector<float, 3> FloatVector;
  typedef itk::Vector<int, 3> IntVector;
  typedef typename TInputMesh::Cell  Cell;
  typedef typename TInputMesh::CellTraits  CellTraits;
  typedef itk::TriangleCell<float, CellTraits>	   TriCell;
  typedef typename TInputMesh::PointType  IPT;
  typedef typename TInputMesh::PixelType  PT;

  /** 
   * Some functions.
   */
  void ComputeForce();
  void Initialize();
  void SetStiffnessMatrix();
  void Advance();
  void SetStiffness(double a, double b);
  void SetResolution(int a, int b, int c);
  void SetCenter(int a, int b, int c);
  void Reset();
  void ComputeDt();
  void ComputeOutput();
  void SetForces(TInputMesh* force);
  void SetLocations(TInputMesh* location);
  void SetNormals(TInputMesh* normals);
  void SetDisplacements(TInputMesh* displace);
  void SetDerives(TInputMesh* derive);
  void SetPotential(ImagePointer potential);
  void NodeAddition(int i);
  void GapSearch();

protected:
  BalloonForceFilter();
  ~BalloonForceFilter() {};
  BalloonForceFilter(const BalloonForceFilter&) {};
  void operator=(const BalloonForceFilter&) {};
//  void PrintSelf(std::ostream& os, Indent indent);

  /** 
   * These meshes are defined to hold the vectors as force, etc.
   */
  InputMeshPointer Forces; 
  InputMeshPointer Normals;
  InputMeshPointer Displacements;
  InputMeshPointer Derives;
  InputMeshPointer Locations;
  InputMeshPointer m_Input;
  InputMeshPointer m_Output;

  /** 
   * Three different kinds of stiffness matrix.
   */
  vnl_matrix_fixed<double, 4, 4> NStiffness;
  vnl_matrix_fixed<double, 4, 4> SStiffness;
  vnl_matrix_fixed<double, 4, 4> CStiffness;
  vnl_matrix_fixed<double, 4, 4> **K;
  
  double Stiffness[2];
  double TimeStep;
  int Resolution[3];
  int Center[3];
  float m_MiniT;
  int m_Step;
  int m_NumNodes;
  int m_NumCells;
  int m_NumNewNodes;
  float m_NewNodes[200][4];

  ImagePointer Potential;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBalloonForceFilter.txx"
#endif

#endif
