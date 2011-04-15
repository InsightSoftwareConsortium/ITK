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
#ifndef __itkFEMLoadLandmark_h
#define __itkFEMLoadLandmark_h

#include "itkFEMLoadElementBase.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {

/**
 * \class LoadLandmark
 * \brief This load is derived from the motion of a specific landmark
 *
 * This load depends on the motion of a point from an undeformed
 * configuration to a deformed configuration.
 * \ingroup ITK-FEM
 */
class LoadLandmark : public LoadElement {
  FEM_CLASS(LoadLandmark,LoadElement)
public:

  /**
   * Square root of the variance (eta)
   */
  double eta;

  /**
   * Point in __local coordinates__ in the undeformed configuration
   */
  vnl_vector<Float> m_pt;

  /**
   * Point in __global coordinates__ in the deformed configuration
   */
  vnl_vector<Float> m_target;

  vnl_vector<Float> m_source;

  vnl_vector<Float> m_force;


  /**
   * Pointer to the element which contains the undeformed
   * configuration of the landmark
   */
  //Element::ConstPointer m_element;

  /**
   * Pointer to the solution object
   */
  Solution::ConstPointer m_Solution;

  /**
   * Methods to access the most recent solution vector
   */
  void SetSolution(Solution::ConstPointer ptr) { m_Solution = ptr; }
  Solution::ConstPointer GetSolution() { return m_Solution; }
  Float GetSolution(unsigned int i, unsigned int v=0) { return m_Solution->GetSolutionValue(i,v); }

  /**
   * Access the location of the point load
   */
  const Element::VectorType& GetPoint() const  { return m_pt; }

  /**
   * Set the force vector
   */
  void SetPoint( const  vnl_vector<Float>& pt) { m_pt=pt; }

  /**
   * Access the location of the point load
   */
  Element::VectorType& GetSource()
    {
    return m_source;
    }

  Element::VectorType& GetForce()
    {
    return m_force;
    }

  /**
   * Set the force vector
   */
  void SetForce( const  vnl_vector<Float>& force)
    {
    if (m_force.size() != force.size())
      {
      m_force.set_size(force.size());
      }
    for (unsigned int i=0; i<force.size(); i++)
      {
      m_force[i]=force[i];
      }
    }


  /**
   * Set the force vector
   */
  void SetSource( const  vnl_vector<Float>& source)
    {
    if (m_source.size() != source.size())
      {
      m_source.set_size(source.size());
      }
    for (unsigned int i=0; i<source.size(); i++)
      {
      m_source[i]=source[i];
      }
    }

  /**
   * Access the location of the point load
   */
  const Element::VectorType& GetTarget() const
    {
    return m_target;
    }

  /**
   * Set the force vector
   */
  void SetTarget( const  vnl_vector<Float>& target)
    {
    if (m_target.size() != target.size())
      {
      m_target.set_size(target.size());
      }
    for (unsigned int i=0; i<target.size(); i++)
      {
      m_target[i]=target[i];
      }
    }

  void ScalePointAndForce( double* spacing, double fwt)
    {
    for (unsigned int i=0; i<m_target.size(); i++)
      {
      m_target[i] /= spacing[i];
      m_source[i] /= spacing[i];
      this->eta *= fwt;
      }
    }

  /**
   * Read a LoadLandmark object from the input stream
   */
  virtual void Read( std::istream& f, void* info );

  /**
   * Assign the LoadLandmark to an element
   */
  virtual void AssignToElement( Element::ArrayType::Pointer elements );

  /**
   * Write a LoadLandmark object to the output stream
   */
  virtual void Write( std::ostream& f ) const;

  /**
   * Default constructors
   */
  LoadLandmark() {}
};

FEM_CLASS_INIT(LoadLandmark)

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadLandmark_h
