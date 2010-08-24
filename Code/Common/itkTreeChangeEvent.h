/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTreeChangeEvent.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTreeChangeEvent_h
#define __itkTreeChangeEvent_h

#include "itkMacro.h"
#include <itkEventObject.h>
#include <itkTreeIteratorBase.h>

namespace itk
{
/** \class TreeChangeEvent
 *  \brief This class derives from ModifiedEvent and check if the position of a node
 *  in the tree has been changed
 */
template< class TTreeType >
class TreeChangeEvent:public ModifiedEvent
{
public:

  /** Typedefs */
  typedef TreeChangeEvent Self;
  typedef ModifiedEvent   Superclass;

  /** Constructor */
  TreeChangeEvent()
  {
    m_ChangePosition = NULL;
  }

  /** Copy constructor */
  TreeChangeEvent(const TreeIteratorBase< TTreeType > & position)
  {
    m_ChangePosition = &position;
  }

  /** Destructor */
  virtual ~TreeChangeEvent() {}

  /** Get the event name */
  virtual const char * GetEventName() const
  {
    return "TreeChangeEvent";
  }

  /** Check the event */
  virtual bool CheckEvent(const::itk::EventObject *e) const
  {
    return dynamic_cast< const Self * >( e );
  }

  /** Make the event object */
  virtual::itk::EventObject * MakeObject() const
  {
    return new Self(*m_ChangePosition);
  }

  /** Get the change position */
  const TreeIteratorBase< TTreeType > & GetChangePosition() const
  {
    return *m_ChangePosition;
  }

  TreeChangeEvent(const Self & s):itk::ModifiedEvent(s) {}
protected:

  const TreeIteratorBase< TTreeType > *m_ChangePosition;
private:
  void operator=(const Self &);
};

/**  \class TreeNodeChangeEvent
 * Signals, that a node has been set to another value. Position of the
 * changed node is provided  */
template< class TTreeType >
class TreeNodeChangeEvent:public TreeChangeEvent< TTreeType >
{
public:
  typedef TreeNodeChangeEvent          Self;
  typedef TreeChangeEvent< TTreeType > Superclass;

  TreeNodeChangeEvent() {}

  TreeNodeChangeEvent(const TreeIteratorBase< TTreeType > & position):
    TreeChangeEvent< TTreeType >(position) {}

  virtual const char * GetEventName() const
  {
    return "TreeNodeChangeEvent";
  }

  virtual bool CheckEvent(const::itk::EventObject *e) const
  {
    return dynamic_cast< const Self * >( e );
  }

  virtual::itk::EventObject * MakeObject() const
  {
    return new Self(*this->m_ChangePosition);
  }

  TreeNodeChangeEvent(const Self & s):TreeChangeEvent< TTreeType >(s) {}
private:
  void operator=(const Self &);
};

/** \class TreeAddEvent
 *  \brief This class derives from TreeChangeEvent and check if a node has been
 *  added to the tree
 */
template< class TTreeType >
class TreeAddEvent:public TreeChangeEvent< TTreeType >
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
  virtual const char * GetEventName() const
  {
    return "TreeAddEvent";
  }

  /** Check event function */
  virtual bool CheckEvent(const::itk::EventObject *e) const
  {
    return dynamic_cast< const Self * >( e );
  }

  /** Make the event object */
  virtual::itk::EventObject * MakeObject() const
  {
    return new Self(*this->m_ChangePosition);
  }

  TreeAddEvent(const Self & s):TreeChangeEvent< TTreeType >(s) {}
private:
  void operator=(const Self &);
};

/** \class TreeRemoveEvent
 *  \brief This class derives from TreeChangeEvent and check if a node has been
 *  removed from the tree
 */
template< class TTreeType >
class TreeRemoveEvent:public TreeChangeEvent< TTreeType >
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
  virtual const char * GetEventName() const
  {
    return "TreeRemoveEvent";
  }

  /** Check the event */
  virtual bool CheckEvent(const::itk::EventObject *e) const
  {
    return dynamic_cast< const Self * >( e );
  }

  /** Make the event object */
  virtual::itk::EventObject * MakeObject() const
  {
    return new Self(*this->m_ChangePosition);
  }

  TreeRemoveEvent(const Self & s):TreeChangeEvent< TTreeType >(s) {}
private:
  void operator=(const Self &);
};

/** \class TreePruneEvent
 * Signals that a node and all its childs will shortly be
 * removed. Position of the top-level removed node is provided */
template< class TTreeType >
class TreePruneEvent:public TreeRemoveEvent< TTreeType >
{
public:
  typedef TreePruneEvent               Self;
  typedef TreeRemoveEvent< TTreeType > Superclass;

  /** */
  TreePruneEvent(){}

  /** */
  TreePruneEvent(const TreeIteratorBase< TTreeType > & position):
    TreeRemoveEvent< TTreeType >(position) {}

  /** */
  virtual const char * GetEventName() const
  {
    return "TreePruneEvent";
  }

  /** */
  virtual bool CheckEvent(const::itk::EventObject *e) const
  {
    return dynamic_cast< const Self * >( e );
  }

  /** */
  virtual::itk::EventObject * MakeObject() const
  {
    return new Self(*this->m_ChangePosition);
  }

  TreePruneEvent(const Self & s):TreeRemoveEvent< TTreeType >(s) {}
private:
  void operator=(const Self &);
};
} // namespace itk

#endif
