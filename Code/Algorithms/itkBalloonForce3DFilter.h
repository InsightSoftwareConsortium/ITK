/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBalloonForce3DFilter.h
  Language:  C++:   
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
#include "itkSimpleImageRegionIterator.h"
#include <itkCovariantVector.h>

namespace itk
{

/** \class BalloonForce3DFilter
 * \brief 
 *
 * BalloonForce3DFilter is a simplified form of the 3D deformable model.
 * There are 2 forces applied on the surface of model. The balloon force
 * and the gradient force.
 * (1) The balloon force is a 2D force. In each slice, the balloon force is 
 *     oriented along the normal direction of the model.
 * (2) The gradient force is derived from boundary points locations in the 
 *     original data. At boundary positions, the magnitude of the gradient 
 *     is large and the gradient force, proportional to the gradient magnitude,
 *     will attract the model to these boundary locations.
 * The combination of the two forces will drive the model nodes towards the 
 * boundary positions and stop them as they reach a boundary position 
 * (i.e high gradient position) 
 * The model motion is driven by the following equation::
 *        d' + Kd = f_{int} + f_{ext}
 * The model nodes positions are updated using the following iterative scheme:
 *      d_{n+1} = d_{n} + timestep * d'
 * Input parameters are:
 *  (1) An original volume
 *  (2) A Deformable mesh (it can be derived from ShpereSource) 
 *  (3) A potential map. This is a binary volume with object and background 
 *   labelled differently (equivalent to a mask). 
 *   This volume can be derived from GibbsPriorFilter or any other 
 *   segmentation method.
 *
 * A gradient volume is derived from a GaussainRecursiveFilter applied to 
 * the original data. This filter first applies a smoothing Gaussian filter 
 * to the volume and then computes the gradient in 2D for each slice.
 * 
 * In order to speed up the deformation process, this method initially 
 * deforms the model with only the balloon force (BalloonForceFilter method) 
 * so that the model expands inside the object binary mask. When the nodes on 
 * the model get close to the object mask boundary, the gradient force is turned on
 * (GradientFit method) to refine the the model nodes position using the gradient
 * information in the original volume.
 * The BalloonForce3DFilter treats the 3D segmentation as series of 2D slices.
 *
 * \ingroup MeshFilters
 * \ingroup MeshSegmentation */
template <class TInputMesh, class TOutputMesh>
class BalloonForce3DFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:

  /** Standard "Self" typedef. */
  typedef BalloonForce3DFilter  Self;

  /** Standard "Superclass" typedef. */
  typedef MeshToMeshFilter<TInputMesh, TOutputMesh> Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(BalloonForce3DFilter,MeshToMeshFilter);

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

  /** typedefs for cell and point.*/
  typedef typename InputMeshType::Cell              Cell;
  typedef typename InputMeshType::CellTraits        CellTraits;
  typedef typename InputMeshType::PointType         InputPointType;
  typedef typename InputMeshType::PixelType         PixelType;
  typedef TriangleCell<PixelType, CellTraits>  TriCell;

  /** image and image iterator definition */
  typedef CovariantVector<PixelType, 3>        GradientType;
  typedef Image<GradientType, 3>               GradientImageType;
  typedef Image<unsigned short, 3>             ImageType;
  typedef typename ImageType::Pointer          ImagePointer;
  typedef typename ImageType::IndexType        IndexType;
  typedef SimpleImageRegionIterator<ImageType> ImageIterator;
  typedef SimpleImageRegionIterator<GradientImageType> GradientIterator;
  typedef ImageType::SizeType PotentialSizeType;
  typedef typename GradientImageType::Pointer       GradientImagePointer;

  /** Two typedef for vectors.*/
  typedef Vector<float, 3>     FloatVector;
  typedef Vector<int, 3>       IntVector;

  /* Mesh pointer definition */
  typedef typename InputMeshType::Pointer   InputMeshPointer;
  typedef typename OutputMeshType::Pointer  OutputMeshPointer;

  /** Routines. */
  void ComputeForce(); /** compute the balloon force. */
  void Initialize();   /** initialize the model. */
  void SetStiffnessMatrix(); /** set the stiffness matrix. */
  void Advance();      /** update data for next iteration. */
  void ComputeDt();    /** compute point positions. */
  void ComputeOutput(); /** compute the output. */
  void NodeAddition();  /** add new nodes to the model. */
  void NodesRearrange();  /** rearrange the model nodes after each iteration. */
  void GradientFit(); /** fit the model with gradient information */
  void ComputeNormals();  /** compute the normals on each point on the model surface. */
  void ACDSearch();   /** remove weird structures on the model surface. */
  void ComputeShrinkForce();  /** compute force in case we shrink the model. */
  void InitialFit();          /** method used to locate the model near the object boundary. */

  /** Set/Get routines */
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
  BalloonForce3DFilter();
  ~BalloonForce3DFilter() {}
  BalloonForce3DFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateData();

private:
  /** These meshes are defined to hold the vectors as force, etc. */
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
  
  /** Parameters definition */
  double    m_StiffnessV, m_StiffnessH;
  double    m_TimeStep;      /** Time step of each iteration. */
  int       m_XResolution, m_YResolution, m_ZResolution; /** resolutions in 3 direction */
  IndexType m_Center;        /** model center */
  double    m_MiniT;         /** help to stop nodes near a boundary position. */
  int       m_Step;          /** Number of iterations. */
  int       m_NumNodes;      /** number of nodes on the model surface. */
  int       m_NumCells;      /** number of cells on the model surface. */
  int       m_NumNewNodes;   /** To add new nodes. */
  int       *m_GapLocations;
  int       m_imgWidth;      /** Image size. */
  int       m_imgHeight;
  int       m_imgDepth;
  int       m_ModelXUpLimit; /** Records the size of the model. */
  int       m_ModelXDownLimit;
  int       m_ModelYUpLimit;
  int       m_ModelYDownLimit;
  int       m_NewNode;       /** sign to add new node */
  int       **m_ACD;         /** To remove unstable structures on the model surface. */
  int       m_StepThreshold1;/** This threshold decides when to switch from potential fit to gradient fit. */
  int       m_StepThreshold2;/** This threshold decides when to stop the model. */
  int       m_FirstSlice;    /** This variable helps relocating the model when trying to load it. */ 
  int       m_NeighborRadius;/** Defines the spatial range for the gradient fit */
  unsigned short        m_ObjectLabel; /** the potential label for the object. */

  /** To compute the force derived from the potential data.*/
  ImagePointer          m_Potential;
  /** To compute the force derived from the gradient data.*/
  GradientImagePointer  m_Gradient; 
  /** Output a binary mask with segmented object. */
  ImagePointer          m_ImageOutput; 
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBalloonForce3DFilter.txx"
#endif

#endif
