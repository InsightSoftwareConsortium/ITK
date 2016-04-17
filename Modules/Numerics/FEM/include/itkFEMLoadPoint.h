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

#ifndef itkFEMLoadPoint_h
#define itkFEMLoadPoint_h

#include "itkFEMLoadElementBase.h"
#include "ITKFEMExport.h"
#include "vnl/vnl_vector.h"

namespace itk
{
namespace fem
{
/**
 * \class LoadPoint
 * \brief This load is applied on a point in an element.
 *
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LoadPoint : public LoadElement
{
public:
  /** Standard class typedefs. */
  typedef LoadPoint                Self;
  typedef LoadElement              Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadPoint, LoadElement);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  /** Default constructor. */
  LoadPoint() :
    m_Point(2, NumericTraits<Float>::ZeroValue() ),
    m_ForcePoint(2, NumericTraits<Float>::ZeroValue() )
  {
    // Default initialization of 2D point and force vector
  }

  /** Set the point where the load acts. */
  void SetPoint(const vnl_vector<Float> p);

  /** Get the point where the load acts. */
  vnl_vector<Float> GetPoint();

  /** Set the force vector. */
  void SetForce(const vnl_vector<Float> f);

  /** Get the force vector. */
  vnl_vector<Float> GetForce();

  /** Apply the load to the specified element.
  * Modified version from the one in itk::fem::LoadLandmark. */
  virtual void ApplyLoad(Element::ConstPointer element, Element::VectorType & Fe) ITK_OVERRIDE;

protected:

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /** Point of which the load acts in global the coordinate system. */
  vnl_vector<Float> m_Point;

  /** The actual load vector. */
  vnl_vector<Float> m_ForcePoint;

};

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLoadPoint_h
