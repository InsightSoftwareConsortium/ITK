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
#ifndef __itkFEMLoadGrav_h
#define __itkFEMLoadGrav_h

#include "itkFEMLoadElementBase.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {

/**
 * \class LoadGrav
 * \brief Abstract gravity load class.
 *
 * This load is integrated over a whole element. The load vector is returned in a
 * Fg member function defined in a derived class. The Fg function accepts a vector
 * specifying a point in global coordinate system and returns a load vector
 * defined at the point. Derived LoadClasses must define this function.
 * \ingroup ITK-FEM
 */
class LoadGrav : public LoadElement
{
  FEM_ABSTRACT_CLASS(LoadGrav,LoadElement)
public:

  virtual vnl_vector<Float> Fg(vnl_vector<Float>) = 0;

};

/**
 * \class LoadGravConst
 * \brief Constant gravity load class.
 *
 * This is a special case of LoadGrav. The load vector is the same on
 * every point in space.
 * \ingroup ITK-FEM
 */
class LoadGravConst : public LoadGrav
{
  FEM_CLASS(LoadGravConst,LoadGrav)
public:
  vnl_vector<Float> Fg_value;
  virtual vnl_vector<Float> Fg(vnl_vector<Float>)
    {
    return Fg_value;
    }

  /**
   * Read an object from input stream.
   */
  virtual void Read( std::istream& f, void* info );

  /**
   * Write an object to the output stream
   */
  virtual void Write( std::ostream& f ) const;

};

FEM_CLASS_INIT(LoadGravConst)

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadGrav_h
