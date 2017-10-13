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
  typedef TreeChangeEvent Self;
  typedef ModifiedEvent   Superclass;

  /** Constructor */
  TreeChangeEvent():
    m_ChangePosition( ITK_NULLPTR )
    {}

  /** Copy constructor */
  TreeChangeEvent(const TreeIteratorBase< TTreeType > & position) { m_ChangePosition = &position; }

  /** Destructor */
  virtual ~TreeChangeEvent() ITK_OVERRIDE {}

  /** Get the event name */
  virtual const char * GetEventName() const ITK_OVERRIDE { return "TreeChangeEvent"; }

  /** Check the event */
  virtual bool CheckEvent(const::itk::EventObject *e) const ITK_OVERRIDE { return (dynamic_cast< const Self * >( e ) != ITK_NULLPTR); }

  /** Make the event object */
  virtual::itk::EventObject * MakeObject() const ITK_OVERRIDE { return new Self(*m_ChangePosition); }

  /** Get the change position */
  const TreeIteratorBase< TTreeType > & GetChangePosition() const { return *m_ChangePosition; }

  // cppcheck-suppress uninitVar
  TreeChangeEvent(const Self & s):itk::ModifiedEvent(s) {}

protected:

  const TreeIteratorBase< TTreeType > *m_ChangePosition;

private:
  void operator=(const Self &);
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
  typedef TreeNodeChangeEvent          Self;
  typedef TreeChangeEvent< TTreeType > Superclass;

  TreeNodeChangeEvent() {}

  TreeNodeChangeEvent(const TreeIteratorBase< TTreeType > & position):
    TreeChangeEvent< TTreeType >(position) {}

  virtual const char * GetEventName() const { return "TreeNodeChangeEvent"; }

  virtual bool CheckEvent(const::itk::EventObject *e) const { return dynamic_cast< const Self * >( e ); }

  virtual::itk::EventObject * MakeObject() const { return new Self(*this->m_ChangePosition); }

  TreeNodeChangeEvent(const Self & s):TreeChangeEvent< TTreeType >(s) {}

private:
  void operator=(const Self &);
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
  typedef TreeAddEvent                 Self;
  typedef TreeChangeEvent< TTreeType > Superclass;

  /** Constructor */
  TreeAddEvent() {}

  /** Copy constructor */
  TreeAddEvent(const TreeIteratorBase< TTreeType > & position):
    TreeChangeEvent< TTreeType >(position) {}

  /** Get the name of the event */
  virtual const char * GetEventName() const { return "TreeAddEvent"; }

  /** Check event function */
  virtual bool CheckEvent(const::itk::EventObject *e) const { return (dynamic_cast< const Self * >( e ) != ITK_NULLPTR); }

  /** Make the event object */
  virtual::itk::EventObject * MakeObject() const { return new Self(*this->m_ChangePosition); }

  TreeAddEvent(const Self & s):TreeChangeEvent< TTreeType >(s) {}

private:
  void operator=(const Self &);
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
  typedef TreeRemoveEvent              Self;
  typedef TreeChangeEvent< TTreeType > Superclass;

  /** Constructor */
  TreeRemoveEvent(){}

  /** Copy constructor */
  TreeRemoveEvent(const TreeIteratorBase< TTreeType > & position):
    TreeChangeEvent< TTreeType >(position) {}

  /** Get the event name */
  virtual const char * GetEventName() const { return "TreeRemoveEvent"; }

  /** Check the event */
  virtual bool CheckEvent(const::itk::EventObject *e) const { return (dynamic_cast< const Self * >( e ) != ITK_NULLPTR); }

  /** Make the event object */
  virtual::itk::EventObject * MakeObject() const { return new Self(*this->m_ChangePosition); }

  TreeRemoveEvent(const Self & s):TreeChangeEvent< TTreeType >(s) {}

private:
  void operator=(const Self &);
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
  typedef TreePruneEvent               Self;
  typedef TreeRemoveEvent< TTreeType > Superclass;

  TreePruneEvent(){}

  TreePruneEvent(const TreeIteratorBase< TTreeType > & position):
  TreeRemoveEvent< TTreeType >(position) {}

  virtual const char * GetEventName() const { return "TreePruneEvent"; }

  virtual bool CheckEvent(const::itk::EventObject *e) const { return (dynamic_cast< const Self * >( e ) != ITK_NULLPTR); }

  virtual::itk::EventObject * MakeObject() const { return new Self(*this->m_ChangePosition); }

  TreePruneEvent(const Self & s):TreeRemoveEvent< TTreeType >(s) {}

private:
  void operator=(const Self &);
};
} // namespace itk

#endif
