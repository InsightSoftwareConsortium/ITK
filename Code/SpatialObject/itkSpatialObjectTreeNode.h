/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectTreeNode.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectTreeNode_h
#define __itkSpatialObjectTreeNode_h

#include <itkTreeNode.h>
#include <itkSpatialObject.h>
#include <itkScalableAffineTransform.h>

namespace itk
{

template <unsigned int TDimension> class SpatialObject;

template <unsigned int TDimension>
class SpatialObjectTreeNode : public TreeNode< SpatialObject<TDimension> * >
{

public:

  /** Standard typedefs */
  typedef SpatialObject<TDimension>                SpatialObjectType;
  typedef TreeNode< SpatialObject<TDimension> *>   Superclass;
  typedef SpatialObjectTreeNode<TDimension>        Self;
  typedef SmartPointer<Self>                       Pointer;
  typedef SmartPointer<const Self>                 ConstPointer;
  typedef ScalableAffineTransform< double, TDimension>   
                                                   TransformType;
  typedef typename TransformType::Pointer          TransformPointer;
  typedef const TransformType*                     TransformConstPointer;
  typedef typename Superclass::ChildrenListType    ChildrenListType;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );
 
  /** Run-time type information (and related methods). */ 
  itkTypeMacro( SpatialObjectTreeNode, TreeNode );

  /** Set the SpatialObject pointer */
  virtual void SetData(SpatialObjectType* data);

  /** Set/Get the NodeToParenNode transform */
  itkSetObjectMacro(NodeToParentNodeTransform,TransformType);
  itkGetConstReferenceObjectMacro(NodeToParentNodeTransform,TransformType);
 
  /** Set/Get the NodeToParenNode transform */
  itkSetObjectMacro(NodeToWorldTransform,TransformType);
  itkGetConstReferenceObjectMacro(NodeToWorldTransform,TransformType);

  /** Compute the NodeToWorld transform based on the parent*/
  void ComputeNodeToWorldTransform();

  /** Return a list of children (the list should be deleted by the user */
#if !defined(CABLE_CONFIGURATION)
  virtual ChildrenListType* GetChildren( unsigned int depth=0,
                                         char * name=NULL) const;
#endif

protected:

  /** Constructor */
  SpatialObjectTreeNode();
  virtual ~SpatialObjectTreeNode(){};

  TransformPointer m_NodeToParentNodeTransform;
  TransformPointer m_NodeToWorldTransform;

private:

  SpatialObjectTreeNode(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

/** Constructor */
template <unsigned int TDimension>
SpatialObjectTreeNode<TDimension>
::SpatialObjectTreeNode() : TreeNode<SpatialObject<TDimension> *>()
{
  m_NodeToParentNodeTransform = TransformType::New();
  m_NodeToParentNodeTransform->SetIdentity();
  m_NodeToWorldTransform = TransformType::New();
  m_NodeToWorldTransform->SetIdentity();
  this->m_Parent = NULL;
}

  
/** Set the data */
template <unsigned int TDimension>
void
SpatialObjectTreeNode<TDimension>
::SetData(SpatialObjectType* data)
{
  Superclass::Set(data);
  data->SetTreeNode(this); // give the pointer to the node to the spatial object
}
  
/** Compute the NodeToWorld transform based on the parent */
template <unsigned int TDimension>
void SpatialObjectTreeNode<TDimension>
::ComputeNodeToWorldTransform()
{
  m_NodeToWorldTransform->SetMatrix(m_NodeToParentNodeTransform->GetMatrix());
  m_NodeToWorldTransform->SetOffset(m_NodeToParentNodeTransform->GetOffset());
  if(this->HasParent())
    {
    static_cast<Self*>(this->GetParent())->ComputeNodeToWorldTransform();
    m_NodeToWorldTransform->Compose( static_cast<Self*>(this->GetParent())
                                     ->GetNodeToWorldTransform(), false);
    }
}


/** Get children given a name and a depth */
#if !defined(CABLE_CONFIGURATION)
template <unsigned int TDimension>
typename SpatialObjectTreeNode<TDimension>::ChildrenListType* 
SpatialObjectTreeNode<TDimension>
::GetChildren( unsigned int depth, char * name) const
{
  ChildrenListType * children = new ChildrenListType;

  typename ChildrenListType::const_iterator childrenListIt = 
    this->m_Children.begin();
  typename ChildrenListType::const_iterator childrenListEnd = 
    this->m_Children.end();

  while( childrenListIt != childrenListEnd )
    {
    if( name == NULL || strstr(typeid(*((*childrenListIt)->Get())).name(),
                               name) )
      {
      children->push_back(*childrenListIt);
      }
    if( depth > 0 )
      {
      ChildrenListType * nextchildren = 
                         (**childrenListIt).GetChildren(depth-1, name);  
      // Add the child to the current list
      typename ChildrenListType::const_iterator nextIt = nextchildren->begin();
      while(nextIt != nextchildren->end())
        {
        children->push_back(*nextIt);
        nextIt++;
        }
      delete nextchildren;
      }
    childrenListIt++;
    }

  return children;
}
#endif

} // end namespace itk


#endif
