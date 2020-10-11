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
#ifndef itkLevelSetDomainPartitionBase_h
#define itkLevelSetDomainPartitionBase_h

#include "itkObject.h"
#include "itkIntTypes.h"

#include <list>

namespace itk
{
/**
 *\class LevelSetDomainPartitionBase
 *
 * \brief Helper class used to partition domain and efficiently compute overlap.
 * \ingroup ITKLevelSetsv4
 */
template <typename TDomain>
class ITK_TEMPLATE_EXPORT LevelSetDomainPartitionBase : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetDomainPartitionBase);

  using Self = LevelSetDomainPartitionBase;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(LevelSetDomainPartitionBase, LightObject);

  /** Get/Set the number of level set functions */
  itkSetMacro(NumberOfLevelSetFunctions, IdentifierType);
  itkGetMacro(NumberOfLevelSetFunctions, IdentifierType);

  virtual void
  PopulateListDomain() = 0;

protected:
  /** \brief Constructor */
  LevelSetDomainPartitionBase();

  /** \brief Destructor */
  ~LevelSetDomainPartitionBase() override = default;

  virtual void
  AllocateListDomain() = 0;

  using IdentifierListType = std::list<IdentifierType>;
  using IdentifierListIterator = typename IdentifierListType::iterator;
  using IdentifierListConstIterator = typename IdentifierListType::const_iterator;

  IdentifierType m_NumberOfLevelSetFunctions;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetDomainPartitionBase.hxx"
#endif

#endif
