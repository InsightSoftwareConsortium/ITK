/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformableMeshFilter_h
#define __itkDeformableMeshFilter_h

#include "itkMeshToMeshFilter.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_math.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkTriangleCell.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include <itkCovariantVector.h>

namespace itk
{

/** \class DeformableMeshFilter
 * \brief 
 *
 * DeformableMeshFilter is used to apply balloon force and the potential
 * force onto the 2D or 3D deformable model.
 * The balloon force is vertical to the surface of the model. The potential 
 * force is given out by the estimated boundary points. These two will meet 
 * a balance at the boundary, thus the deformable model will fit to the 
 * boundary.
 * Users should use deformable model as input using SetInput and also provide
 * the filter with a potential image which will provide the estimated bounday.
 * The image should be a binary image with object and background labelled 
 * differently. These image can be given by GibbsPriorFilter or any other
 * segmentation filters.
 * When the nodes on the model stopped at the estimated boundary, using the
 * GradientFit method to fit the model using the gradient information in the
 * original image.
 *
 * \ingroup MeshFilters
 * \ingroup MeshSegmentation
 */
template <class TInputMesh, class TOutputMesh>
class DeformableMeshFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  /** Standard class typedefs. */
  typedef DeformableMeshFilter  Self;
  typedef MeshToMeshFilter<TInputMesh, TOutputMesh> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformableMeshFilter,MeshToMeshFilter);

  /** Some convenient typedefs. */
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
  typedef Image<unsigned short, 3>   ImageType;
  typedef CovariantVector<float, 3> GradientType;
  typedef Image<GradientType, 3>  GradientImageType;
  typedef ImageRegionIterator<ImageType> ImageIterator;
  typedef ImageRegionIterator<GradientImageType> GradientIterator;
  typedef typename InputMeshType::Pointer InputMeshPointer;
  typedef typename OutputMeshType::Pointer  OutputMeshPointer;
  typedef typename ImageType::Pointer   ImagePointer;
  typedef typename ImageType::IndexType   IndexType;
  typedef typename GradientImageType::Pointer   GradientPointer;

  /** Vector related typedefs. */
  typedef Vector<float, 3>       FloatVector;
  typedef Vector<int, 3>       IntVector;

  /** Cell related typedefs. */
  typedef typename InputMeshType::CellType    CellType;
  typedef typename InputMeshType::CellTraits  CellTraits;
  typedef CellInterface<float, CellTraits>    CellInterfaceType;
  typedef TriangleCell<CellInterfaceType>     TriCell;
  typedef typename InputMeshType::PointType   IPT;
  typedef typename InputMeshType::PixelType   PT;

  /** Some functions. */
  void ComputeForce();
  void Initialize();
  void SetStiffnessMatrix();
  void Advance();             // update data for next iteration
  void SetStiffness(double a, double b);
  void SetResolution(int a, int b, int c);
  void SetCenter(int a, int b, int c);
  void Reset();               // reset all data
  void ComputeDt();             // compute point positions
  void ComputeOutput();
  void SetPotential(ImagePointer potential);
  void SetGradient(GradientPointer gradient);
  void NodeAddition();
  void SliceAddition(int i);
  void NodesRearrange();          
  void GapSearch();       
  void GradientFit();           // fit the model with gradient information
  void ComputeNormals();
  void ACDSearch();             // remove weird structures on the model surface
  void ComputeShrinkForce();        // force in case we shrink the model
  void InitialFit();              // locate the model near the objects

  /** Set the output image. */
  itkSetMacro(ImageOutput, ImagePointer);
  itkGetMacro(ImageOutput, ImagePointer);

  /** Set the first slice. */
  itkSetMacro(FirstSlice, int);
  itkGetMacro(FirstSlice, int);

  /** Set algorithm related parameters. */
  itkSetMacro(NeighborRadius, int);
  itkSetMacro(StepThreshold1, int);
  itkSetMacro(StepThreshold2, int);
  itkGetMacro(Resolution, int*);
  itkGetMacro(Normals, InputMeshPointer);

protected:
  DeformableMeshFilter();
  ~DeformableMeshFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateData();

private:
  DeformableMeshFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  /** These meshes are defined to hold the vectors as force, etc.*/
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
  
  double  m_Stiffness[2];
  double  TimeStep;       // the time step of each iteration
  int   m_Resolution[3];
  IndexType m_Center;
  float   m_MiniT;        // variabel help to stop the model when near potential estimation
  int   m_Step;         // the number of iteration 
  int   m_NumNodes;
  int   m_NumCells;
  int   m_NumNewNodes;      // for adding new nodes, now disabled for further tests
  int   *m_GapLocations;
  bool   m_NewNode;
  int   m_NewNodesExisted;
  int   m_NewNodeLimit;
  int   m_ImageWidth;       // input image size
  int   m_ImageHeight;
  int   m_ImageDepth;
  int   m_ModelXUpLimit;    // the following 6 variables record the size of the model
  int   m_ModelXDownLimit;
  int   m_ModelYUpLimit;
  int   m_ModelYDownLimit;
  int   m_ModelZUpLimit;
  int   m_ModelZDownLimit;
  int   **m_ACD;        // help to remove the weird structure on the model surface
  int   m_ModelRestart;
  int   m_StepThreshold1;   // the threshold decide when to transfer from potential fit to gradient fit
  int   m_StepThreshold2;   // the threshold decide when to stop the model
  int   m_FirstSlice;     // variable help to relocate the model when try to load 
  int   m_NeighborRadius;   // the gradient fit range

  ImagePointer    m_Potential;  // for calculate of image force from potential
  GradientPointer m_Gradient;   // for calculate of image force from gradient

  // for Gibbs Prior Model parameters' recalculation 
  ImagePointer    m_ImageOutput; 
  unsigned short  m_ObjectLabel;

  typedef ImageType::SizeType PotentialSizeType;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableMeshFilter.txx"
#endif

#endif
