/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBalloonForce3DFilter.h
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
#ifndef __itkBalloonForce3DFilter_h
#define __itkBalloonForce3DFilter_h

#include "itkMeshToMeshFilter.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_math.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkTriangleCell.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

/** \class BalloonForce3DFilter
 * \brief 
 *
 * BalloonForce3DFilter is used to apply balloon force and the potential
 * force onto the deformable model.
 * The balloon force is vertical to the surface of the model. The potential 
 * force is given out by the estimated boundary points. These two will meet 
 * a balance at the boundary, thus the deformable model will fit to the 
 * boundary.
 * Users should use deformable model as input using setInput and also provide
 * the filter with a potential image which will provide the estimated bounday.
 * The image should be a binary image with object and background labelled 
 * differently. These images can be given by GibbsPriorFilter or any other
 * segmentation filters.
 * When the nodes on the model stops at the estimated boundary, using the
 * GapSearch method to add new nodes at region the object has a complicate
 * shape.
 *
 * \ingroup MeshFilters
 * \ingroup MeshSegmentation
 */
template <class TInputMesh, class TOutputMesh>
class ITK_EXPORT BalloonForce3DFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef BalloonForce3DFilter  Self;

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
  itkTypeMacro(BalloonForce3DFilter,MeshToMeshFilter);

  /** 
   * Some typedefs.
   */
  typedef typename Superclass::OutputMeshType OutputMeshType;
  typedef TInputMesh InputMeshType;
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

  typedef Image<unsigned short, 3>		ImageType;
  typedef typename InputMeshType::Pointer	InputMeshPointer;
  typedef typename OutputMeshType::Pointer	OutputMeshPointer;
  typedef typename ImageType::Pointer		ImagePointer;
  typedef typename ImageType::IndexType		IndexType;
  typedef ImageRegionIteratorWithIndex<ImageType>	ImageIterator;

  typedef itk::Vector<float, 3>				FloatVector;
  typedef itk::Vector<int, 3>				IntVector;

  typedef typename InputMeshType::Cell			Cell;
  typedef typename InputMeshType::CellTraits	CellTraits;
  typedef itk::TriangleCell<float, CellTraits>	TriCell;
  typedef typename InputMeshType::PointType		IPT;
  typedef typename InputMeshType::PixelType		PT;

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
  void SetGradient(ImagePointer gradient);
//  void SetImageOutput(ImagePointer outputimg);
  void NodeAddition(int i, int res, IPT z);
  void NodesRearrange();
  void GapSearch();
  void GradientFit();
  void ComputeNormals();
  void ACDSearch();
//  void PotentialModify(int i, int j);

//  ImagePointer GetImageOutput();

  itkSetMacro(ImageOutput, ImagePointer);
  itkGetMacro(ImageOutput, ImagePointer);

protected:
  BalloonForce3DFilter();
  ~BalloonForce3DFilter() {};
  BalloonForce3DFilter(const BalloonForce3DFilter&) {};
  void operator=(const BalloonForce3DFilter&) {};
  virtual void GenerateData();
//  void PrintSelf(std::ostream& os, Indent indent);

/** 
 * These meshes are defined to hold the vectors as force, etc.
 */
  InputMeshPointer	m_Forces; 
  InputMeshPointer	m_Normals;
  InputMeshPointer	m_Displacements;
  InputMeshPointer	m_Derives;
  InputMeshPointer	m_Locations;
  InputMeshPointer	m_Input;
  OutputMeshPointer m_Output;

  /** 
   * Three different kinds of stiffness matrix.
   */
  vnl_matrix_fixed<double, 4, 4> NStiffness;
  vnl_matrix_fixed<double, 4, 4> SStiffness;
  vnl_matrix_fixed<double, 4, 4> CStiffness;
  vnl_matrix_fixed<double, 4, 4> **K;
  
  double	m_Stiffness[2];
  double	TimeStep;
  int		m_Resolution[3];
  IndexType	m_Center;
  float		m_MiniT;
  int		m_Step;
  int		m_NumNodes;
  int		m_NumCells;
  int		m_NumNewNodes;
  int		*m_GapLocations;
//  float		m_NewNodes[200][4];
  float		**m_NewNodes;
  int		m_NewNodesExisted;
  int		m_NewNodeLimit;
  int		m_imgWidth;
  int		m_imgHeight;
  int		m_imgDepth;
  int		m_ModelXUpLimit;
  int		m_ModelXDownLimit;
  int		m_ModelYUpLimit;
  int		m_ModelYDownLimit;
  int		**m_ACD;
  int		m_ModelRestart;

  ImagePointer		m_Potential;	// for calculate of image force from ptoential
  ImagePointer		m_Gradient;		// for calculate of image force from gradient

  // for Gibbs Prior Model parameters' recalculation 
  ImagePointer		m_ImageOutput; 
  unsigned short	m_ObjectLabel;

  typedef ImageType::SizeType PotentialSizeType;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBalloonForce3DFilter.txx"
#endif

#endif
