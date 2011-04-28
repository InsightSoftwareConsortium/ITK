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
#ifndef __itkFEMLoadPoint_h
#define __itkFEMLoadPoint_h

#include "itkFEMLoadElementBase.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {

/**
 * \class LoadPoint
 * \brief This load is applied on a point in an element.
 *
 * FIXME: To be implemented. Nothing works yet
 * \ingroup ITK-FEM
 */
class LoadPoint : public LoadElement {
  FEM_CLASS(LoadPoint,LoadElement)
public:

  /**
   * Point of which the load acts in global coord. sys.
   */
  vnl_vector<Float> point;

  /**
   * the actual load vector
   */
  vnl_vector<Float> Fp;

  /**
   * Default constructor
   */
  LoadPoint() :
    point(2), Fp(2) {}    /**  we initialize 2D point and force vector */

};

FEM_CLASS_INIT(LoadPoint)

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadPoint_h
