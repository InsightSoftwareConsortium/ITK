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

#ifndef itkFEMLoadLandmark_h
#define itkFEMLoadLandmark_h

#include "itkFEMLoadElementBase.h"
#include "ITKFEMExport.h"

#include "vnl/vnl_vector.h"

namespace itk
{
namespace fem
{
/**
 * \class LoadLandmark
 * \brief This load is derived from the motion of a specific landmark
 *
 * This load depends on the motion of a point from an undeformed
 * configuration to a deformed configuration.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LoadLandmark : public LoadElement
{
public:
  /** Standard class typedefs. */
  typedef LoadLandmark             Self;
  typedef LoadElement              Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadLandmark, LoadElement);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  /**
   * Methods to access the most recent solution vector
   */
  virtual void SetSolution(Solution::ConstPointer ptr) ITK_OVERRIDE
  {
    m_Solution = ptr;
  }
  virtual Solution::ConstPointer GetSolution() ITK_OVERRIDE
  {
    return m_Solution;
  }
  Float GetSolution(unsigned int i, unsigned int v = 0)
  {
    return m_Solution->GetSolutionValue(i, v);
  }

  /**
   * Access the location of the point load
   */
  Element::VectorType & GetPoint()
  {
    return m_Point;
  }

  /**
   * Set the force vector
   */
  void SetPoint(const vnl_vector<Float> & pt)
  {
    m_Point = pt;
  }

  /**
   * Access the location of the point load
   */
  Element::VectorType & GetSource()
  {
    return m_Source;
  }

  const Element::VectorType & GetSource() const
  {
    return m_Source;
  }

  /**
   * Get the force vector
   */
  Element::VectorType & GetForce()
  {
    return m_Force;
  }

  const Element::VectorType & GetForce() const
  {
    return m_Force;
  }

  /**
   * Set the force vector
   */
  void SetForce(const vnl_vector<Float> & force)
  {
    if( m_Force.size() != force.size() )
      {
      m_Force.set_size( force.size() );
      }
    for( unsigned int i = 0; i < force.size(); i++ )
      {
      m_Force[i] = force[i];
      }
  }

  /**
   * Set the force vector
   */
  void SetSource(const vnl_vector<Float> & source)
  {
    if( m_Source.size() != source.size() )
      {
      m_Source.set_size( source.size() );
      }
    for( unsigned int i = 0; i < source.size(); i++ )
      {
      m_Source[i] = source[i];
      }
  }

  /**
   * Access the location of the point load
   */
  Element::VectorType & GetTarget()
  {
    return m_Target;
  }
  const Element::VectorType & GetTarget() const
  {
    return m_Target;
  }

  /**
   * Set the force vector
   */
  void SetTarget(const vnl_vector<Float> & target)
  {
    if( m_Target.size() != target.size() )
      {
      m_Target.set_size( target.size() );
      }
    for( unsigned int i = 0; i < target.size(); i++ )
      {
      m_Target[i] = target[i];
      }
  }

  void ScalePointAndForce(double *spacing, double fwt)
  {
    for( unsigned int i = 0; i < m_Target.size(); i++ )
      {
      m_Target[i] /= spacing[i];
      m_Source[i] /= spacing[i];
      this->m_Eta *= fwt;
      }
  }

  /**
   * Set the element containing the landmark
   */
  void SetContainedElement(const Element * e)
  {
    this->m_Element[0] = e;
  }

  /**
   * Get the element containing the landmark
   */
  const Element * GetContainedElement() const
  {
    return this->m_Element[0];
  }

  /**
   * Assign the LoadLandmark to an element
   */
  virtual bool AssignToElement(Element::ArrayType::Pointer elements);

  virtual bool AssignToElement(Element::ArrayType1::Pointer elements);

  virtual Element::ConstPointer GetAssignedElement(Element::ArrayType1::Pointer elements);

  /**
   * Default constructors
   */
  LoadLandmark():
    m_Eta(0),
    m_Target(0),
    m_Source(0),
    m_Force(0),
    m_Solution(ITK_NULLPTR)
  {
  }

  /** Get/Set the eta parameter, square root of the variance, for the load */
  void SetEta(double e);

  double GetEta() const;

  /** Apply the load to the specified element */
  virtual void ApplyLoad(Element::ConstPointer element, Element::VectorType & Fe) ITK_OVERRIDE;

protected:

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /**
   * Square root of the variance (eta)
   */
  double m_Eta;

  /**
   * Point in __local coordinates__ in the undeformed configuration
   */
  vnl_vector<Float> m_Point;

  /**
   * Point in __global coordinates__ in the deformed configuration
   */
  vnl_vector<Float> m_Target;

  vnl_vector<Float> m_Source;

  vnl_vector<Float> m_Force;

  /**
   * Pointer to the element which contains the undeformed
   * configuration of the landmark
   */
  // Element::ConstPointer m_element;

  /**
   * Pointer to the solution object
   */
  Solution::ConstPointer m_Solution;
};

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLoadLandmark_h
