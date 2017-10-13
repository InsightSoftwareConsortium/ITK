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
#ifndef itkVTKPolyDataWriter_h
#define itkVTKPolyDataWriter_h

#include "itkIntTypes.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"

namespace itk
{
/** \class VTKPolyDataWriter
 * \brief
 * Writes an itkMesh to a file in VTK file format.
 *
 * Caveat: The input to itkVTKPolyDataWriter must be a triangle mesh.
 *         Use vtkTriangleFilter to convert your mesh to a triangle mesh.
 *
 * This class may be deprecated in the future. The MeshFileWriter is
 * preferred.
 *
 * \ingroup ITKMesh
 *
 * \sa MeshFileWriter
 */
template< typename TInputMesh >
class ITK_TEMPLATE_EXPORT VTKPolyDataWriter:public Object
{
public:
  /** Standard "Self" typedef. */
  typedef VTKPolyDataWriter          Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKPolyDataWriter, Object);

  /** Write the Input mesh to the Output file.
   * Use either Update() or Write(). */
  void Update();

  void Write();

  /** Hold on to the type information specified by the template parameters.
   */
  typedef TInputMesh                              InputMeshType;
  typedef typename InputMeshType::PixelType       PixelType;
  typedef typename InputMeshType::PointType       PointType;
  typedef typename InputMeshType::CellType        CellType;
  typedef typename InputMeshType::PointIdentifier PointIdentifier;

  /** Some convenient typedefs. */
  typedef typename InputMeshType::ConstPointer InputMeshPointer;
  typedef typename InputMeshType::CellTraits   CellTraits;

  /** Define the triangular cell types which form the surface  */
  typedef CellInterface< PixelType, CellTraits > CellInterfaceType;
  typedef TriangleCell< CellInterfaceType >      TriangleCellType;

  typedef typename InputMeshType::PointsContainer PointsContainer;
  typedef typename InputMeshType::CellsContainer  CellsContainer;

  typedef typename PointsContainer::ConstIterator PointIterator;
  typedef typename CellsContainer::ConstIterator  CellIterator;

  typedef typename CellType::PointIdIterator PointIdIterator;

  /** Set the Input */
  void SetInput(const InputMeshType *input);

  /** Set/Get the name of the file where data are written. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

protected:
  VTKPolyDataWriter();
  virtual ~VTKPolyDataWriter() ITK_OVERRIDE;

  virtual void GenerateData();

  std::string m_FileName;

  InputMeshPointer m_Input;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKPolyDataWriter);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKPolyDataWriter.hxx"
#endif

#endif
