/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkBinaryMask3DMeshSource_h
#define itkBinaryMask3DMeshSource_h

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
template <typename TInputImage, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT BinaryMask3DMeshSource : public ImageToMeshFilter<TInputImage, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryMask3DMeshSource);

  /** Standard "Self" type alias. */
  using Self = BinaryMask3DMeshSource;
  using Superclass = ImageToMeshFilter<TInputImage, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMask3DMeshSource, ImageToMeshFilter);

  /** Hold on to the type information specified by the template parameters. */
  using OutputMeshType = TOutputMesh;
  using OMeshTraits = typename OutputMeshType::MeshTraits;
  using OPointType = typename OutputMeshType::PointType;
  using OPixelType = typename OMeshTraits::PixelType;

  /** Some convenient type alias. */
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using CellTraits = typename OutputMeshType::CellTraits;
  using PointsContainerPointer = typename OutputMeshType::PointsContainerPointer;
  using PointsContainer = typename OutputMeshType::PointsContainer;
  using CellsContainerPointer = typename OutputMeshType::CellsContainerPointer;
  using CellsContainer = typename OutputMeshType::CellsContainer;
  using doubleVector = CovariantVector<double, 2>;
  using intVector = CovariantVector<int, 2>;

  /** Define the triangular cell types which forms the surface of the model
   * and will be used in FEM application. */
  using TCellInterface = CellInterface<OPixelType, CellTraits>;
  using TriCell = TriangleCell<TCellInterface>;
  using TriCellAutoPointer = typename TriCell::SelfAutoPointer;

  /** Input Image Type Definition. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputPixelType = typename InputImageType::PixelType;
  using SpacingType = typename InputImageType::SpacingType;
  using OriginType = typename InputImageType::PointType;
  using RegionType = typename InputImageType::RegionType;
  using SizeType = typename InputImageType::SizeType;

  /** Type definition for the classified image index type. */
  using InputImageIndexType = typename InputImageType::IndexType;

  using InputImageIterator = ImageRegionConstIterator<InputImageType>;

  using IdentifierType = itk::IdentifierType;
  using SizeValueType = itk::SizeValueType;

  itkSetMacro(ObjectValue, InputPixelType);

  itkGetConstMacro(NumberOfNodes, SizeValueType);
  itkGetConstMacro(NumberOfCells, SizeValueType);

  /** accept the input image */
  using Superclass::SetInput;
  virtual void
  SetInput(const InputImageType * image);

  void
  SetRegionOfInterest(const RegionType & iRegion)
  {
    if (iRegion != m_RegionOfInterest)
    {
      this->m_RegionOfInterest = iRegion;
      this->m_RegionOfInterestProvidedByUser = true;
      this->Modified();
    }
  }

  itkGetConstReferenceMacro(RegionOfInterest, RegionType);

protected:
  BinaryMask3DMeshSource();
  ~BinaryMask3DMeshSource() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;


  bool       m_RegionOfInterestProvidedByUser{ false };
  RegionType m_RegionOfInterest;

  void
  GenerateOutputInformation() override
  {} // do nothing override

private:
  using InputImageSizeType = typename InputImageType::SizeType;

  void
  CreateMesh();

  void
  XFlip(unsigned char * x); // 7 kinds of transformation

  void
  YFlip(unsigned char * x);

  void
  ZFlip(unsigned char * x);

  void
  XRotation(unsigned char * x);

  void
  YRotation(unsigned char * x);

  void
  ZRotation(unsigned char * x);

  void
  inverse(unsigned char * x);

  void
  InitializeLUT(); // initialize the look up table before the mesh
                   // construction

  void
  AddCells(unsigned char celltype, unsigned char celltran, int index);

  void
  AddNodes(int               index,
           unsigned char *   nodesid,
           IdentifierType *  globalnodesid,
           IdentifierType ** currentrowtmp,
           IdentifierType ** currentframetmp);

  void
  CellTransfer(unsigned char * nodesid, unsigned char celltran);

  IdentifierType
  SearchThroughLastRow(int index, int start, int end);

  IdentifierType
  SearchThroughLastFrame(int index, int start, int end);

  unsigned char m_LUT[256][2]; // the two lookup tables

  IdentifierType m_LastVoxel[14];
  IdentifierType m_CurrentVoxel[14];

  IdentifierType ** m_LastRow{ nullptr };
  IdentifierType ** m_LastFrame{ nullptr };
  IdentifierType ** m_CurrentRow{ nullptr };
  IdentifierType ** m_CurrentFrame{ nullptr };

  int           m_CurrentRowIndex{ 0 };
  int           m_CurrentFrameIndex{ 0 };
  int           m_LastRowNum{ 0 };
  int           m_LastFrameNum{ 0 };
  int           m_CurrentRowNum{ 200 };
  int           m_CurrentFrameNum{ 2000 };
  unsigned char m_AvailableNodes[14];

  double m_LocationOffset[14][3];

  SizeValueType m_NumberOfNodes{ 0 };
  SizeValueType m_NumberOfCells{ 0 };

  int m_NodeLimit{ 2000 };
  int m_CellLimit{ 4000 };
  int m_ImageWidth{ 0 };
  int m_ImageHeight{ 0 };
  int m_ImageDepth{ 0 };
  int m_ColFlag{ 0 };
  int m_RowFlag{ 0 };
  int m_FrameFlag{ 0 };
  int m_LastRowIndex{ 0 };
  int m_LastVoxelIndex{ 0 };
  int m_LastFrameIndex{ 0 };

  unsigned char  m_PointFound{ 0 };
  InputPixelType m_ObjectValue;

  /** temporary variables used in CreateMesh to avoid thousands of
   *  calls to GetInput() and GetOutput()
   */
  OutputMeshType *       m_OutputMesh;
  const InputImageType * m_InputImage;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryMask3DMeshSource.hxx"
#endif

#endif
