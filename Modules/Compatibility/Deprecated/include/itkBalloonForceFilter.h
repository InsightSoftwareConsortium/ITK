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
#ifndef itkBalloonForceFilter_h
#define itkBalloonForceFilter_h
#if !defined( ITK_LEGACY_REMOVE )

#include "itkMeshToMeshFilter.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkMath.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include <itkCovariantVector.h>

namespace itk
{
/** \class BalloonForceFilter
 * \brief
 *
 * BalloonForceFilter is used to apply balloon force and the potential
 * force onto the 2D surface model embedded in a 3D space.
 *
 * The balloon force is normal to the surface of the model. The potential
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
 * \deprecated
 * \ingroup ITKDeprecated
 * \ingroup MeshFilters
 * \ingroup MeshSegmentation
 */
template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT BalloonForceFilter:public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef BalloonForceFilter                          Self;
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BalloonForceFilter, MeshToMeshFilter);

  /** Some typedefs for the input and output types. */
  typedef TInputMesh  InputMeshType;
  typedef TOutputMesh OutputMeshType;

  /** Typedefs for containers and their iterators. */
  typedef typename InputMeshType::PointsContainerPointer
  InputPointsContainerPointer;
  typedef typename InputMeshType::PointsContainer::Iterator
  InputPointsContainerIterator;
  typedef typename InputMeshType::PointsContainerConstPointer
  InputPointsContainerConstPointer;
  typedef typename InputMeshType::PointsContainer::ConstIterator
  InputPointsContainerConstIterator;
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
  typedef typename InputMeshType::CellsContainerConstPointer
  InputCellsContainerConstPointer;
  typedef typename InputMeshType::CellsContainer::ConstIterator
  InputCellsContainerConstIterator;
  typedef typename InputMeshType::CellDataContainerConstPointer
  InputCellDataContainerConstPointer;
  typedef typename InputMeshType::CellDataContainer::ConstIterator
  InputCellDataContainerConstIterator;

  typedef typename OutputMeshType::PointsContainerPointer
  OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainer::Iterator
  OutputPointsContainerIterator;

  /** Image types. */
  typedef typename InputMeshType::PointType IPixelType;
  typedef typename InputMeshType::PixelType PixelType;

  typedef Image< unsigned short, 2 >            ImageType;
  typedef CovariantVector< PixelType, 2 >       GradientType;
  typedef Image< GradientType, 2 >              GradientImageType;
  typedef typename InputMeshType::Pointer       InputMeshPointer;
  typedef typename InputMeshType::ConstPointer  InputMeshConstPointer;
  typedef typename OutputMeshType::Pointer      OutputMeshPointer;
  typedef typename ImageType::Pointer           ImagePointer;
  typedef typename ImageType::IndexType         IndexType;
  typedef typename GradientImageType::Pointer   GradientImagePointer;
  typedef typename GradientImageType::IndexType GradientIndexType;
  typedef ImageRegionIterator< ImageType >      ImageIterator;
  typedef Vector< float, 3 >                    FloatVector;
  typedef Vector< int, 3 >                      IntVector;
  typedef Vector< double, 2 >                   Double2Vector;
  typedef Vector< int, 2 >                      Int2Vector;

  /** Cell related types. */
  typedef typename InputMeshType::CellType             CellType;
  typedef typename InputMeshType::CellTraits           CellTraits;
  typedef::itk::CellInterface< PixelType, CellTraits > CellInterface;
  typedef::itk::TriangleCell< CellInterface >          TriCell;

  /** Some functions. */
  void ComputeForce();

  void Initialize();

  void SetStiffnessMatrix();

  void Advance();             // update data for next iteration

  void Reset();               // reset all data

  void ComputeDt();             // compute point positions

  void ComputeOutput();

  void NodeAddition(int i, int res, IPixelType z); // (following 3) for adding
                                                   // new nodes, now disabled
                                                   // for further tests

  void NodesRearrange();

  void GapSearch();

  void GradientFit();           // fit the model with gradient information

  void ComputeNormals();

  void ACDSearch();             // remove weird structures on the model surface

  /** Set the output image. */
  itkSetMacro(ImageOutput, ImagePointer);
  itkGetConstMacro(ImageOutput, ImagePointer);
  itkSetMacro(Gradient, GradientImagePointer);

  /** Set/Get information for the algorithm. */
  itkSetMacro(Stiffness, Double2Vector);
  itkSetMacro(TimeStep, double);
  itkSetMacro(GradientBegin, int);
  itkSetMacro(Resolution, int);
  itkSetMacro(Center, IndexType);
  itkGetConstMacro(Normals, InputMeshPointer);
  itkSetMacro(DistanceForGradient, float);
  itkSetMacro(DistanceToStop, float);
  itkSetMacro(Potential, ImagePointer);
  itkGetConstMacro(Locations, InputMeshPointer);
  itkGetConstMacro(Displacements, InputMeshPointer);
  itkGetConstMacro(Derives, InputMeshPointer);
  itkGetConstMacro(Forces, InputMeshPointer);

protected:
  BalloonForceFilter();
  ~BalloonForceFilter();
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BalloonForceFilter);

  /** These meshes are defined to hold the vectors as force, etc. */
  InputMeshPointer  m_Forces;
  InputMeshPointer  m_Normals;
  InputMeshPointer  m_Displacements;
  InputMeshPointer  m_Derives;
  InputMeshPointer  m_Locations;
  InputMeshPointer  m_Input;
  OutputMeshPointer m_Output;

  /** Three different kinds of stiffness matrix. */
  vnl_matrix_fixed< double, 4, 4 >   m_NStiffness;
  vnl_matrix_fixed< double, 4, 4 >   m_SStiffness;
  vnl_matrix_fixed< double, 4, 4 >   m_CStiffness;
  vnl_matrix_fixed< double, 4, 4 > **m_K;

  Double2Vector m_Stiffness;
  double        m_TimeStep;        // the time step of each iteration
  int           m_Resolution;
  IndexType     m_Center;
  float         m_MiniT;         // variabel help to stop the model when near
                                 // potential estimation
  int           m_Step;          // the number of iteration

  unsigned int m_NumberOfNodes;
  unsigned int m_NumberOfCells;
  unsigned int m_NumNewNodes;        // for adding new nodes, now disabled for
                                     // further tests
  int *        m_GapLocations;
  float **     m_NewNodes;
  int          m_NewNodesExisted;
  unsigned int m_NewNodeLimit;
  unsigned int m_ImageWidth;         // input image size
  unsigned int m_ImageHeight;
  unsigned int m_ImageDepth;

  int   m_ModelXUpLimit;    // the following 4 variables record the size of the
                            // model
  int   m_ModelXDownLimit;
  int   m_ModelYUpLimit;
  int   m_ModelYDownLimit;
  int **m_ACD;        // help to remove the weird structure on the model surface
  int   m_ModelRestart;
  int   m_GradientBegin;

  Int2Vector m_StepThreshold;     // the threshold decide when to transfer from
                                  // potential fit to gradient fit

  // and the threshold decide when to stop the model
  float m_DistanceToBoundary;
  float m_DistanceToStop;
  float m_DistanceForGradient;

  ImagePointer         m_Potential;     // for calculate of image force from
                                        // potential
  GradientImagePointer m_Gradient;      // for calculate of image force from
                                        // gradient

  // for Gibbs Prior Model parameters' recalculation
  ImagePointer   m_ImageOutput;
  unsigned short m_ObjectLabel;

  typedef ImageType::SizeType ImageSizeType;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBalloonForceFilter.hxx"
#endif

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
