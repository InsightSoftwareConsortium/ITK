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
#ifndef itkFEMSpatialObjectReader_h
#define itkFEMSpatialObjectReader_h

#include "itkSpatialObjectReader.h"
#include "itkMetaFEMObjectConverter.h"

namespace itk
{
/** \class FEMSpatialObjectReader
 *
 * \brief Read any SpatialObject file with conversion for FEM Objects
 *
 * \ingroup ITKFEM
 */
template< unsigned int NDimensions = 3,
          typename PixelType = unsigned char,
          typename TMeshTraits = DefaultStaticMeshTraits< PixelType, NDimensions, NDimensions >
          >
class FEMSpatialObjectReader : public SpatialObjectReader<NDimensions,PixelType,TMeshTraits>
{
public:
  typedef FEMSpatialObjectReader                                 Self;
  typedef SpatialObjectReader<NDimensions,PixelType,TMeshTraits> Superclass;
  typedef SmartPointer< Self >                                   Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Superclass, Self);

  /** Method for creation through the object factory */
  itkNewMacro(Self);

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(FEMSpatialObjectReader);

  std::string m_FileName;

  FEMSpatialObjectReader();
  virtual ~FEMSpatialObjectReader() ITK_OVERRIDE {}

};

template< unsigned int NDimensions,
          typename PixelType,
          typename TMeshTraits >
FEMSpatialObjectReader< NDimensions, PixelType, TMeshTraits >
::FEMSpatialObjectReader()
{
  this->RegisterMetaConverter("FEMObject","FEMObjectSpatialObject",
                              MetaFEMObjectConverter<NDimensions>::New());
}

}

#endif // itkFEMSpatialObjectReader_h
