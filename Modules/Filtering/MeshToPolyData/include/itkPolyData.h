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
#ifndef itkPolyData_h
#define itkPolyData_h

#include "itkDataObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/** \class PolyData
 *
 * \brief Geometry class compatible with vtk.js PolyData
 *
 * \ingroup MeshToPolyData
 */
template< typename TPixel >
class ITK_TEMPLATE_EXPORT PolyData: public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolyData);

  using Self = PolyData;
  using Superclass = DataObject;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolyData, DataObject);

  /** Type of PointData or CellData */
  using PixelType = TPixel;
protected:
  PolyData();
  ~PolyData() override = default;

  void PrintSelf(std::ostream & os, Indent indent) const override;
private:
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolyData.hxx"
#endif

#endif // itkPolyData_h
