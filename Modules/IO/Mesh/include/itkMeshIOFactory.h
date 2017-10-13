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
#ifndef itkMeshIOFactory_h
#define itkMeshIOFactory_h
#include "ITKIOMeshExport.h"

#include "itkObject.h"
#include "itkMeshIOBase.h"

namespace itk
{
/** \class MeshIOFactory
 * \brief Create instances of MeshIO objects using an object factory.
 *
 * Below are the supported mesh file format:
 * BYU Geometry File Format(*.byu)
 * Freesurfer curvature file format (*.fcv)
 * Freesurfer surface file format (ASCII *.fia and BINARY *.fsb)
 * Geometry format under the neuroimaging informatics technology initiative (*.gii)
 * Object file format (*.obj)
 * VTK legacy file format (*.vtk)
 *
 * \ingroup ITKIOMesh
 */
class ITKIOMesh_EXPORT MeshIOFactory:public Object
{
public:
  /** Standard class typedefs. */
  typedef MeshIOFactory              Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class Methods used to interface with the registered factories */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshIOFactory, Object);

  /** Convenient typedefs. */
  typedef::itk::MeshIOBase::Pointer MeshIOBasePointer;

  /** Mode in which the files is intended to be used */
  typedef enum { ReadMode, WriteMode } FileModeType;

  /** Create the appropriate MeshIO depending on the particulars of the file. */
  static MeshIOBasePointer CreateMeshIO(const char *path, FileModeType mode);

  /** Register Built-in factories */
  static void RegisterBuiltInFactories();

protected:
  MeshIOFactory();
  ~MeshIOFactory() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MeshIOFactory);
};
} // end namespace itk

#endif
