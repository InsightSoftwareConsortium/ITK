/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMObjectSpatialObject_h
#define itkFEMObjectSpatialObject_h

#include "itkFEMObject.h"
#include "itkSpatialObject.h"

namespace itk
{

/** \class FEMObjectSpatialObject
 * \brief Implementation spatial object that can hold a FEMObject.
 *
 * This class was created to hold a FEMObject as a SpatialObject.
 * This was originally done to provide an I/O mechanism for FE
 * problems. However, other SpatialObject functionality should be
 * supported by this class.
 *
 * \sa SpatialObject CompositeSpatialObject FEMObject
 * \ingroup ITKFEM
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT FEMObjectSpatialObject : public SpatialObject<TDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FEMObjectSpatialObject);

  using Self = FEMObjectSpatialObject<TDimension>;
  using Superclass = SpatialObject<TDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using FEMObjectType = itk::fem::FEMObject<TDimension>;
  using FEMObjectPointer = typename FEMObjectType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FEMObjectSpatialObject, SpatialObject);

  /** Set the FEM object in the spatial object. */
  void
  SetFEMObject(FEMObjectType * femobject);

  /** Get a pointer to the FEM object currently attached to the object. */
  FEMObjectType *
  GetFEMObject()
  {
    return m_FEMObject.GetPointer();
  }
  const FEMObjectType *
  GetFEMObject() const
  {
    return m_FEMObject.GetPointer();
  }

  /** Returns the latest modified time of the object and its component. */
  ModifiedTimeType
  GetMTime() const override;

protected:
  FEMObjectPointer m_FEMObject;

  FEMObjectSpatialObject();
  ~FEMObjectSpatialObject() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFEMObjectSpatialObject.hxx"
#endif

#endif // itkFEMObjectSpatialObject_h
