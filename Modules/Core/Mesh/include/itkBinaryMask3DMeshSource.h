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
#ifndef __itkBinaryMask3DMeshSource_h
#define __itkBinaryMask3DMeshSource_h

#include "vnl/vnl_matrix_fixed.h"
#include "itkMesh.h"
#include "itkImageToMeshFilter.h"
#include "itkTriangleCell.h"
#include "itkCovariantVector.h"
#include "itkDefaultStaticMeshTraits.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
/** \class BinaryMask3DMeshSource
 *
 *
 * \par
 * This class tries to construct a 3D mesh surface based on a binary mask.
 * It can be used to integrate a region-based segmentation method and a deformable
 * model into one hybrid framework.
 *
 * \par
 * To construct a mesh, we need to construct elements in a voxel and combine
 * those elements later to form the final mesh. Before go through every voxel
 * in the 3D volume, we first construct 2 look up tables. The index of these 2
 * tables are the on-off combination of the 8 nodes that form the voxel. So
 * both of these tables has the size of \$2^8\$ bytes. According to previous
 * work, all those \$2^8\$ combination of the nodes can be grouped into 16
 * final combinations. In the first table, we record the final combination that
 * can be transformed from the current combination. The entries of the second
 * table are made up of the transforming sequence that is necessary for the
 * current combination transform to one of the final combinations.
 *
 * \par
 * We then go through the 3D volume voxel by voxel, using those two tables we have defined
 * to construct elements within each voxel. We then merge all these mesh elements into
 * one 3D mesh.
 *
 * \par PARAMETERS
 * The ObjectValue parameter is used to identify the object. In most applications,
 * pixels in the object region are assigned to "1", so the default value of ObjectValue is
 * set to "1"
 *
 * \par REFERENCE
 * W. Lorensen and H. Cline, "Marching Cubes: A High Resolution 3D Surface Construction Algorithm",
 * Computer Graphics 21, pp. 163-169, 1987.
 *
 * \par INPUT
 * The input should be a 3D binary image.
 *
 * \ingroup ITKMesh
 */
template< class TInputImage, class TOutputMesh >
class ITK_EXPORT BinaryMask3DMeshSource:public ImageToMeshFilter< TInputImage, TOutputMesh >
{
public:
  /** Standard "Self" typedef. */
  typedef BinaryMask3DMeshSource                        Self;
  typedef ImageToMeshFilter< TInputImage, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                          Pointer;
  typedef SmartPointer< const Self >                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMask3DMeshSource, ImageToMeshFilter);

  /** Hold on to the type information specified by the template parameters. */
  typedef TOutputMesh                         OutputMeshType;
  typedef typename OutputMeshType::MeshTraits OMeshTraits;
  typedef typename OutputMeshType::PointType  OPointType;
  typedef typename OMeshTraits::PixelType     OPixelType;

  /** Some convenient typedefs. */
  typedef typename OutputMeshType::Pointer                OutputMeshPointer;
  typedef typename OutputMeshType::CellTraits             CellTraits;
  typedef typename OutputMeshType::PointsContainerPointer PointsContainerPointer;
  typedef typename OutputMeshType::PointsContainer        PointsContainer;
  typedef typename OutputMeshType::CellsContainerPointer  CellsContainerPointer;
  typedef typename OutputMeshType::CellsContainer         CellsContainer;
  typedef CovariantVector< double, 2 >                    doubleVector;
  typedef CovariantVector< int, 2 >                       intVector;

  /** Define the triangular cell types which forms the surface of the model
   * and will be used in FEM application. */
  typedef CellInterface< OPixelType, CellTraits > TCellInterface;
  typedef TriangleCell< TCellInterface >          TriCell;
  typedef typename TriCell::SelfAutoPointer       TriCellAutoPointer;

  /** Input Image Type Definition. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::PixelType    InputPixelType;
  typedef typename InputImageType::SpacingType  SpacingType;
  typedef typename InputImageType::PointType    OriginType;
  typedef typename InputImageType::RegionType   RegionType;
  typedef typename InputImageType::SizeType     SizeType;

  /** Type definition for the classified image index type. */
  typedef typename InputImageType::IndexType           InputImageIndexType;

  typedef ImageRegionConstIterator< InputImageType > InputImageIterator;

  typedef itk::IdentifierType                   IdentifierType;
  typedef itk::SizeValueType                    SizeValueType;

  itkSetMacro(ObjectValue, InputPixelType);

  itkGetConstMacro(NumberOfNodes, SizeValueType);
  itkGetConstMacro(NumberOfCells, SizeValueType);

  /** accept the input image */
  using Superclass::SetInput;
  virtual void SetInput(const InputImageType *inputImage);

  void SetRegionOfInterest( const RegionType & iRegion )
    {
    if( iRegion != m_RegionOfInterest )
      {
      this->m_RegionOfInterest = iRegion;
      this->m_RegionOfInterestProvidedByUser = true;
      this->Modified();
      }
    }

  itkGetConstReferenceMacro(RegionOfInterest, RegionType);

protected:
  BinaryMask3DMeshSource();
  ~BinaryMask3DMeshSource();
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();


  bool       m_RegionOfInterestProvidedByUser;
  RegionType m_RegionOfInterest;

  virtual void GenerateOutputInformation(){}  // do nothing

private:
  BinaryMask3DMeshSource(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented

  typedef typename InputImageType::SizeType InputImageSizeType;

  void CreateMesh();

  void XFlip(uint8_t *tp);     // 7 kinds of transformation

  void YFlip(uint8_t *tp);

  void ZFlip(uint8_t *tp);

  void XRotation(uint8_t *tp);

  void YRotation(uint8_t *tp);

  void ZRotation(uint8_t *tp);

  void inverse(uint8_t *tp);

  void InitializeLUT(); // initialize the look up table before the mesh
                        // construction

  void AddCells(uint8_t celltype, uint8_t celltran, int index);

  void AddNodes(int index,
                uint8_t *nodesid,
                IdentifierType *globalnodesid,
                IdentifierType **currentrowtmp,
                IdentifierType **currentframetmp);

  void CellTransfer(uint8_t *nodesid, uint8_t celltran);

  IdentifierType SearchThroughLastRow(int index, int start, int end);

  IdentifierType SearchThroughLastFrame(int index, int start, int end);

  uint8_t m_LUT[256][2]; // the two lookup tables

  IdentifierType m_LastVoxel[14];
  IdentifierType m_CurrentVoxel[14];

  IdentifierType **m_LastRow;
  IdentifierType **m_LastFrame;
  IdentifierType **m_CurrentRow;
  IdentifierType **m_CurrentFrame;

  uint16_t m_CurrentRowIndex;
  uint16_t m_CurrentFrameIndex;
  uint16_t m_LastRowNum;
  uint16_t m_LastFrameNum;
  uint16_t m_CurrentRowNum;
  uint16_t m_CurrentFrameNum;
  uint8_t  m_AvailableNodes[14];

  double m_LocationOffset[14][3];

  SizeValueType m_NumberOfNodes;
  SizeValueType m_NumberOfCells;

  int m_NodeLimit;
  int m_CellLimit;
  int m_ImageWidth;
  int m_ImageHeight;
  int m_ImageDepth;
  int m_ColFlag;
  int m_RowFlag;
  int m_FrameFlag;
  int m_LastRowIndex;
  int m_LastVoxelIndex;
  int m_LastFrameIndex;

  uint8_t  m_PointFound;
  InputPixelType m_ObjectValue;

  /** temporary variables used in CreateMesh to avoid thousands of
   *  calls to GetInput() and GetOutput()
   */
  OutputMeshType       *m_OutputMesh;
  const InputImageType *m_InputImage;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryMask3DMeshSource.hxx"
#endif

#endif
