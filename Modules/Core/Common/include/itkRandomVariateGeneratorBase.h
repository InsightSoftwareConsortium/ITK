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
#ifndef itkRandomVariateGeneratorBase_h
#define itkRandomVariateGeneratorBase_h

#include "itkObject.h"

namespace itk
{
namespace Statistics
{
/** \class RandomVariateGeneratorBase
 * \brief Defines common interfaces for random variate generators.
 *
 * \ingroup Common
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT RandomVariateGeneratorBase:public Object
{
public:
  /** Standard class typedefs. */
  typedef RandomVariateGeneratorBase Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RandomVariateGeneratorBase, Object);

  /** get a variate using FastNorm function */
  virtual double GetVariate() = 0;

protected:
  RandomVariateGeneratorBase();
  virtual ~RandomVariateGeneratorBase() ITK_OVERRIDE;

private:
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif
