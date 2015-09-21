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
#ifndef itkSpatialObjectTreeNode_hxx
#define itkSpatialObjectTreeNode_hxx

#include "itkSpatialObjectTreeNode.h"

namespace itk
{

/** Constructor */
template< unsigned int TDimension >
SpatialObjectTreeNode< TDimension >
::SpatialObjectTreeNode():TreeNode< SpatialObject< TDimension > * >()
{
  m_NodeToParentNodeTransform = TransformType::New();
  m_NodeToParentNodeTransform->SetIdentity();
  m_NodeToWorldTransform = TransformType::New();
  m_NodeToWorldTransform->SetIdentity();
  this->m_Parent = ITK_NULLPTR;
}

template< unsigned int TDimension >
void
SpatialObjectTreeNode< TDimension >
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent );
  os << indent << "NodeToParentNodeTransform: "
     << m_NodeToParentNodeTransform << std::endl;
  os << indent << "NodeToWorldTransform: "
     << m_NodeToWorldTransform << std::endl;
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
#if !defined( ITK_WRAPPING_PARSER )
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
    if ( name == ITK_NULLPTR || strstr(typeid( *( ( *childrenListIt )->Get() ) ).name(),
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

}
#endif
