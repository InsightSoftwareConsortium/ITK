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
#ifndef __itkSpatialObjectTreeNode_h
#define __itkSpatialObjectTreeNode_h

#include "itkTreeNode.h"
#include "itkSpatialObject.h"

namespace itk
{
template< unsigned int TDimension >
class SpatialObject;

/** \class SpatialObjectTreeNode
 * \brief TODO
 */
template< unsigned int TDimension >
class ITK_EXPORT SpatialObjectTreeNode:public TreeNode< SpatialObject< TDimension > * >
{
public:

  /** Standard typedefs */
  typedef SpatialObject< TDimension >                   SpatialObjectType;
  typedef TreeNode< SpatialObject< TDimension > * >     Superclass;
  typedef SpatialObjectTreeNode< TDimension >           Self;
  typedef SmartPointer< Self >                          Pointer;
  typedef SmartPointer< const Self >                    ConstPointer;
  typedef ScalableAffineTransform< double, TDimension > TransformType;
  typedef typename TransformType::Pointer               TransformPointer;
  typedef const TransformType *                         TransformConstPointer;
  typedef typename Superclass::ChildrenListType         ChildrenListType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectTreeNode, TreeNode);

  /** Set the SpatialObject pointer */
  virtual void SetData(SpatialObjectType *data);

  /** Set/Get the NodeToParenNode transform */
  itkSetObjectMacro(NodeToParentNodeTransform, TransformType);
  itkGetConstReferenceObjectMacro(NodeToParentNodeTransform, TransformType);

  /** Set/Get the NodeToParenNode transform */
  itkSetObjectMacro(NodeToWorldTransform, TransformType);
  itkGetConstReferenceObjectMacro(NodeToWorldTransform, TransformType);

  /** Compute the NodeToWorld transform based on the parent */
  void ComputeNodeToWorldTransform();

  /** Return a list of children (the list should be deleted by the user */
#if !defined( CABLE_CONFIGURATION )
  virtual ChildrenListType * GetChildren(unsigned int depth = 0,
                                         char *name = NULL) const;

#endif
protected:

  /** Constructor */
  SpatialObjectTreeNode();
  virtual ~SpatialObjectTreeNode(){}
  void PrintSelf(std::ostream & os, Indent indent) const
  {
    this->Superclass::PrintSelf(os, indent);
    os << indent << "NodeToParentNodeTransform: "
       << m_NodeToParentNodeTransform << std::endl;
    os << indent << "NodeToWorldTransform: "
       << m_NodeToWorldTransform << std::endl;
  }

  TransformPointer m_NodeToParentNodeTransform;
  TransformPointer m_NodeToWorldTransform;
private:

  SpatialObjectTreeNode(const Self &); //purposely not implemented
  void operator=(const Self &);        //purposely not implemented
};

/** Constructor */
template< unsigned int TDimension >
SpatialObjectTreeNode< TDimension >
::SpatialObjectTreeNode():TreeNode< SpatialObject< TDimension > * >()
{
  m_NodeToParentNodeTransform = TransformType::New();
  m_NodeToParentNodeTransform->SetIdentity();
  m_NodeToWorldTransform = TransformType::New();
  m_NodeToWorldTransform->SetIdentity();
  this->m_Parent = NULL;
}

/** Set the data */
template< unsigned int TDimension >
void
SpatialObjectTreeNode< TDimension >
::SetData(SpatialObjectType *data)
{
  Superclass::Set(data);
  data->SetTreeNode(this); // give the pointer to the node to the spatial object
}

/** Compute the NodeToWorld transform based on the parent */
template< unsigned int TDimension >
void SpatialObjectTreeNode< TDimension >
::ComputeNodeToWorldTransform()
{
  m_NodeToWorldTransform->SetMatrix( m_NodeToParentNodeTransform->GetMatrix() );
  m_NodeToWorldTransform->SetOffset( m_NodeToParentNodeTransform->GetOffset() );
  if ( this->HasParent() )
    {
    static_cast< Self * >( this->GetParent() )->ComputeNodeToWorldTransform();
    m_NodeToWorldTransform->Compose(static_cast< Self * >( this->GetParent() )
                                    ->GetNodeToWorldTransform(), false);
    }
}

/** Get children given a name and a depth */
#if !defined( CABLE_CONFIGURATION )
template< unsigned int TDimension >
typename SpatialObjectTreeNode< TDimension >::ChildrenListType *
SpatialObjectTreeNode< TDimension >
::GetChildren(unsigned int depth, char *name) const
{
  ChildrenListType *children = new ChildrenListType;

  typename ChildrenListType::const_iterator childrenListIt =
    this->m_Children.begin();
  typename ChildrenListType::const_iterator childrenListEnd =
    this->m_Children.end();

  while ( childrenListIt != childrenListEnd )
    {
    if ( name == NULL || strstr(typeid( *( ( *childrenListIt )->Get() ) ).name(),
                                name) )
      {
      children->push_back(*childrenListIt);
      }
    if ( depth > 0 )
      {
      ChildrenListType *nextchildren =
        ( **childrenListIt ).GetChildren(depth - 1, name);
      // Add the child to the current list
      typename ChildrenListType::const_iterator nextIt = nextchildren->begin();
      while ( nextIt != nextchildren->end() )
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
