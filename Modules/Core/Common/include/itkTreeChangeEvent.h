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
#ifndef itkTreeChangeEvent_h
#define itkTreeChangeEvent_h

#include "itkMacro.h"
#include "itkEventObject.h"
#include "itkTreeIteratorBase.h"

namespace itk
{
/** \class TreeChangeEvent
 *  \brief Checks if the position of a node in the tree has been changed.
 *
 * \ingroup ITKCommon
 */
template< typename TTreeType >
class ITK_TEMPLATE_EXPORT TreeChangeEvent:public ModifiedEvent
{
public:

  /** Typedefs */
  using Self = TreeChangeEvent;
  using Superclass = ModifiedEvent;

  /** Constructor */
  TreeChangeEvent():
    m_ChangePosition( nullptr )
    {}

  /** Copy constructor */
  TreeChangeEvent(const TreeIteratorBase< TTreeType > & position) { m_ChangePosition = &position; }

  /** Destructor */
  ~TreeChangeEvent() override = default;

  /** Get the event name */
  const char * GetEventName() const override { return "TreeChangeEvent"; }

  /** Check the event */
  bool CheckEvent(const::itk::EventObject *e) const override { return (dynamic_cast< const Self * >( e ) != nullptr); }

  /** Make the event object */
  ::itk::EventObject * MakeObject() const override { return new Self(*m_ChangePosition); }

  /** Get the change position */
  const TreeIteratorBase< TTreeType > & GetChangePosition() const { return *m_ChangePosition; }

  // cppcheck-suppress uninitVar
  TreeChangeEvent(const Self & s):itk::ModifiedEvent(s) {}

protected:

  const TreeIteratorBase< TTreeType > *m_ChangePosition;

private:
  void operator=(const Self &) = delete;
};

/**  \class TreeNodeChangeEvent
 *   \brief Signals that a node has been set to another value. The position of the
 *          changed node is provided.
 * \ingroup ITKCommon
 */
template< typename TTreeType >
class TreeNodeChangeEvent:public TreeChangeEvent< TTreeType >
{
public:
  using Self = TreeNodeChangeEvent;
  using Superclass = TreeChangeEvent< TTreeType >;

  TreeNodeChangeEvent() = default;

  TreeNodeChangeEvent(const TreeIteratorBase< TTreeType > & position):
    TreeChangeEvent< TTreeType >(position) {}

  const char * GetEventName() const override { return "TreeNodeChangeEvent"; }

  bool CheckEvent(const::itk::EventObject *e) const override
  {
    auto eSelf = dynamic_cast< const Self* >( e );
    return eSelf != nullptr;
  }

  ::itk::EventObject * MakeObject() const override { return new Self(*this->m_ChangePosition); }

  TreeNodeChangeEvent(const Self & s):TreeChangeEvent< TTreeType >(s) {}

private:
  void operator=(const Self &) = delete;
};

/** \class TreeAddEvent
 *  \brief Checks if a node has been added to the tree.
 * \ingroup ITKCommon
 */
template< typename TTreeType >
class ITK_TEMPLATE_EXPORT TreeAddEvent:public TreeChangeEvent< TTreeType >
{
public:

  /** Typedefs */
  using Self = TreeAddEvent;
  using Superclass = TreeChangeEvent< TTreeType >;

  /** Constructor */
  TreeAddEvent() = default;

  /** Copy constructor */
  TreeAddEvent(const TreeIteratorBase< TTreeType > & position):
    TreeChangeEvent< TTreeType >(position) {}

  /** Get the name of the event */
  const char * GetEventName() const override { return "TreeAddEvent"; }

  /** Check event function */
  bool CheckEvent(const::itk::EventObject *e) const override { return (dynamic_cast< const Self * >( e ) != nullptr); }

  /** Make the event object */
  ::itk::EventObject * MakeObject() const override { return new Self(*this->m_ChangePosition); }

  TreeAddEvent(const Self & s):TreeChangeEvent< TTreeType >(s) {}

private:
  void operator=(const Self &) = delete;
};

/** \class TreeRemoveEvent
 *  \brief Checks if a node has been removed from the tree.
 * \ingroup ITKCommon
 */
template< typename TTreeType >
class ITK_TEMPLATE_EXPORT TreeRemoveEvent:public TreeChangeEvent< TTreeType >
{
public:

  /** Typedefs */
  using Self = TreeRemoveEvent;
  using Superclass = TreeChangeEvent< TTreeType >;

  /** Constructor */
  TreeRemoveEvent()= default;

  /** Copy constructor */
  TreeRemoveEvent(const TreeIteratorBase< TTreeType > & position):
    TreeChangeEvent< TTreeType >(position) {}

  /** Get the event name */
  const char * GetEventName() const override { return "TreeRemoveEvent"; }

  /** Check the event */
  bool CheckEvent(const::itk::EventObject *e) const override { return (dynamic_cast< const Self * >( e ) != nullptr); }

  /** Make the event object */
  ::itk::EventObject * MakeObject() const override { return new Self(*this->m_ChangePosition); }

  TreeRemoveEvent(const Self & s):TreeChangeEvent< TTreeType >(s) {}

private:
  void operator=(const Self &) = delete;
};

/** \class TreePruneEvent
 *  \brief Signals that a node and all its childs will shortly be
 *         removed. The position of the top-level removed node is provided.
 * \ingroup ITKCommon
 */
template< typename TTreeType >
class ITK_TEMPLATE_EXPORT TreePruneEvent:public TreeRemoveEvent< TTreeType >
{
public:
  using Self = TreePruneEvent;
  using Superclass = TreeRemoveEvent< TTreeType >;

  TreePruneEvent()= default;

  TreePruneEvent(const TreeIteratorBase< TTreeType > & position):
  TreeRemoveEvent< TTreeType >(position) {}

  const char * GetEventName() const override { return "TreePruneEvent"; }

  bool CheckEvent(const::itk::EventObject *e) const override { return (dynamic_cast< const Self * >( e ) != nullptr); }

  ::itk::EventObject * MakeObject() const override { return new Self(*this->m_ChangePosition); }

  TreePruneEvent(const Self & s):TreeRemoveEvent< TTreeType >(s) {}

private:
  void operator=(const Self &) = delete;
};
} // namespace itk

#endif
