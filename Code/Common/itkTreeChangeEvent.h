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
#ifndef __TreeChangeEvent_h
#define __TreeChangeEvent_h

#include "itkMacro.h"
#include <itkObject.h>
#include <itkTreeIteratorBase.h>

namespace itk
{

/** \class TreeChangeEvent
 *  \brief This class derives from ModifiedEvent and check if the position of a node
 *  in the tree has been changed
 */
template <class TTreeType>
class TreeChangeEvent : public ModifiedEvent
{ 
public: 

  /** Typedefs */
  typedef TreeChangeEvent Self; 
  typedef ModifiedEvent Superclass; 

  /** Constructor */
  TreeChangeEvent()
    {
    m_ChangePosition = NULL;
    }

  /** Copy constructor */
  TreeChangeEvent(const TreeIteratorBase<TTreeType>& position)
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
  virtual bool CheckEvent(const ::itk::EventObject* e) const 
    { 
    return dynamic_cast<const Self*>(e); 
    } 

  /** Make the event object */
  virtual ::itk::EventObject* MakeObject() const 
    { 
    return new Self( *m_ChangePosition ); 
    } 

  /** Get the change position */
  const TreeIteratorBase<TTreeType>& GetChangePosition() const 
    { 
    return *m_ChangePosition; 
    }

private: 

  TreeChangeEvent(const Self&); 
  void operator=(const Self&); 

protected:

  const TreeIteratorBase<TTreeType>* m_ChangePosition;
};


/** \class TreeAddEvent
 *  \brief This class derives from TreeChangeEvent and check if a node has been
 *  added to the tree
 */
template <class TTreeType>
class TreeAddEvent : public TreeChangeEvent<TTreeType>
{  
public:

  /** Typedefs */
  typedef TreeAddEvent Self; 
  typedef TreeChangeEvent<TTreeType> Superclass; 

  /** Constructor */
  TreeAddEvent() {}

  /** Copy constructor */
  TreeAddEvent( const TreeIteratorBase<TTreeType>& position ) : 
    TreeChangeEvent<TTreeType>(position) {} 

  /** Get the name of the event */
  virtual const char * GetEventName() const 
    { 
    return "TreeAddEvent"; 
    } 

  /** Check event function */
  virtual bool CheckEvent(const ::itk::EventObject* e) const 
    { 
    return dynamic_cast<const Self*>(e); 
    } 

  /** Make the event object */     
  virtual ::itk::EventObject* MakeObject() const 
    { 
    return new Self( *m_ChangePosition ); 
    }  
};


/** \class TreeRemoveEvent
 *  \brief This class derives from TreeChangeEvent and check if a node has been
 *  removed from the tree
 */
template <class TTreeType>
class TreeRemoveEvent : public TreeChangeEvent<TTreeType>
{
public:
 
  /** Typedefs */
  typedef TreeRemoveEvent Self; 
  typedef TreeChangeEvent<TTreeType> Superclass; 

  /** Constructor */
  TreeRemoveEvent(){}

  /** Copy constructor */
  TreeRemoveEvent( const TreeIteratorBase<TTreeType>& position ) : 
  TreeChangeEvent<TTreeType>(position) {} 

  /** Get the event name */
  virtual const char * GetEventName() const 
    { 
    return "TreeRemoveEvent"; 
    } 

  /** Check the event */
  virtual bool CheckEvent(const ::itk::EventObject* e) const 
    { 
    return dynamic_cast<const Self*>(e); 
    } 

  /** Make the event object */     
  virtual ::itk::EventObject* MakeObject() const 
    { 
    return new Self( *m_ChangePosition ); 
    } 
  };
  
} // namespace itk

#endif
