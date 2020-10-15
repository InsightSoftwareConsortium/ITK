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
#ifndef itkMeshIOFactory_h
#define itkMeshIOFactory_h
#include "ITKIOMeshBaseExport.h"

#include "itkObject.h"
#include "itkMeshIOBase.h"
#include "ITKIOMeshBaseExport.h"

namespace itk
{
/**
 *\class MeshIOFactory
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
 * \ingroup ITKIOMeshBase
 */
class ITKIOMeshBase_EXPORT MeshIOFactory : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MeshIOFactory);

  /** Standard class type aliases. */
  using Self = MeshIOFactory;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class Methods used to interface with the registered factories */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshIOFactory, Object);

  /** Convenient type alias. */
  using MeshIOBasePointer = ::itk::MeshIOBase::Pointer;

  using IOFileModeEnum = itk::IOFileModeEnum;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr IOFileModeEnum ReadMode = IOFileModeEnum::ReadMode;
  static constexpr IOFileModeEnum WriteMode = IOFileModeEnum::WriteMode;
#endif

  /** Create the appropriate MeshIO depending on the particulars of the file. */
  static MeshIOBasePointer
  CreateMeshIO(const char * path, IOFileModeEnum mode);

protected:
  MeshIOFactory();
  ~MeshIOFactory() override;
};

} // end namespace itk

#endif
