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

#include "itkDOMNode.h"

#include <sstream>

namespace itk
{

DOMNode::DOMNode() : m_Parent( ITK_NULLPTR )
{
}

void
DOMNode::SetParent( DOMNode* node )
{
  this->m_Parent = node;
}

DOMNode*
DOMNode::GetParent()
{
  return this->m_Parent;
}

const DOMNode*
DOMNode::GetParent() const
{
  return this->m_Parent;
}

/** Retrieve an attribute by key (return an empty string if not found). */
std::string
DOMNode::GetAttribute( const std::string& key ) const
{
  // "id" is a special attribute
  if ( StringTools::MatchWith(key,"id") )
    {
    return this->GetID();
    }

  AttributesContainer::const_iterator i = this->m_Attributes.find( key );
  if ( i == this->m_Attributes.end() )
    {
    return "";
    }
  else
    {
    return i->second;
    }
}

/** Check whether has an attribute. */
bool
DOMNode::HasAttribute( const std::string& key ) const
{
  AttributesContainer::const_iterator i = this->m_Attributes.find( key );
  return ( i != this->m_Attributes.end() );
}

/** Add or replace an attribute. */
void
DOMNode::SetAttribute( const std::string& key, const std::string& value )
{
  // "id" is a special attribute
  if ( StringTools::MatchWith(key,"id") )
    {
    this->SetID( value );
    return;
    }

  AttributesContainer::iterator i = this->m_Attributes.find( key );
  if ( i == this->m_Attributes.end() )
    {
    // add a new attribute
    this->m_Attributes[key] = value;
    // keep track of the adding order
    i = this->m_Attributes.find( key );
    this->m_OrderedAttributes.push_back( &(*i) );
    }
  else
    {
    // replace an existing attribute
    i->second = value;
    }
}

/** Remove an attribute by key (throw exception if not found). */
void
DOMNode::RemoveAttribute( const std::string& key )
{
  AttributesContainer::iterator i = this->m_Attributes.find( key );
  if ( i == this->m_Attributes.end() )
    {
    itkExceptionMacro( "attribute does not exist" );
    }

  // remove it from the ordered container first
  for ( OrderedAttributesContainer::iterator j = this->m_OrderedAttributes.begin(); j != this->m_OrderedAttributes.end(); ++j )
    {
    if ( (*j)->first == key )
      {
      this->m_OrderedAttributes.erase( j );
      break;
      }
    }

  // then remove it from the map container
  this->m_Attributes.erase( i );
}

/**
 * Return all attributes, in the order of being parsed or added (default),
 * or alphabetic order of the attribute keys (keepOriginalOrder = false).
 */
void
DOMNode::GetAllAttributes( AttributesListType& output, bool keepOriginalOrder ) const
{
  if ( keepOriginalOrder )
    {
    for ( OrderedAttributesContainer::const_iterator i = this->m_OrderedAttributes.begin(); i != this->m_OrderedAttributes.end(); ++i )
      {
      output.push_back( *(*i) );
      }
    }
  else
    {
    for ( AttributesContainer::const_iterator i = this->m_Attributes.begin(); i != this->m_Attributes.end(); ++i )
      {
      output.push_back( *i );
      }
    }
}

/** Remove all attributes. */
void
DOMNode::RemoveAllAttributes()
{
  this->m_Attributes.clear();
  this->m_OrderedAttributes.clear();
}

/** Get number of children. */
DOMNode::SizeType
DOMNode::GetNumberOfChildren() const
{
  return this->m_Children.size();
}

/** Return all children. */
void
DOMNode::GetAllChildren( ChildrenListType& output )
{
  for ( IdentifierType i = 0; i < static_cast<IdentifierType>(this->GetNumberOfChildren()); i++ )
    {
    DOMNode* node = this->GetChild( i );
    output.push_back( node );
    }
}

/** Return all children for read-only access. */
void
DOMNode::GetAllChildren( ConstChildrenListType& output ) const
{
  for ( IdentifierType i = 0; i < static_cast<IdentifierType>(this->GetNumberOfChildren()); i++ )
    {
    const DOMNode* node = this->GetChild( i );
    output.push_back( node );
    }
}

/** Return all children of a same tag name. */
void
DOMNode::GetChildren( const std::string& tag, ChildrenListType& output )
{
  for ( IdentifierType i = 0; i < static_cast<IdentifierType>(this->GetNumberOfChildren()); i++ )
    {
    DOMNode* node = this->GetChild( i );
    if ( tag == node->GetName() )
      {
      output.push_back( node );
      }
    }
}

/** Return all children of a same tag name for read-only access. */
void
DOMNode::GetChildren( const std::string& tag, ConstChildrenListType& output ) const
{
  for ( IdentifierType i = 0; i < static_cast<IdentifierType>(this->GetNumberOfChildren()); i++ )
    {
    const DOMNode* node = this->GetChild( i );
    if ( tag == node->GetName() )
      {
      output.push_back( node );
      }
    }
}

/** Remove all children. */
void
DOMNode::RemoveAllChildren()
{
  this->m_Children.clear();
}

/** Add a child in front of another child (throw exception if not able to add). */
void
DOMNode::AddChild( DOMNode* node, IdentifierType i )
{
  if ( this->m_Children.size() == 0 )
    {
    this->AddChildAtEnd( node );
    return;
    }

  if ( node == ITK_NULLPTR || this->ShareRoot(node) )
    {
    itkExceptionMacro( "not able to add child" );
    }

  if ( i < 0 || i >= static_cast<IdentifierType>(this->m_Children.size()) )
    {
    itkExceptionMacro( "not able to add child" );
    }

  node->m_Parent = this;
  this->m_Children.insert( this->m_Children.begin()+i, Pointer(node) );
}

/** Add a child in front of the children list (throw exception if not able to add). */
void
DOMNode::AddChildAtBegin( DOMNode* node )
{
  if ( node == ITK_NULLPTR || this->ShareRoot(node) )
    {
    itkExceptionMacro( "not able to add child" );
    }

  node->m_Parent = this;
  this->m_Children.insert( this->m_Children.begin(), Pointer(node) );
}

/** Add a child at the end of the children list (throw exception if not able to add). */
void
DOMNode::AddChildAtEnd( DOMNode* node )
{
  if ( node == ITK_NULLPTR || this->ShareRoot(node) )
    {
    itkExceptionMacro( "not able to add child" );
    }

  node->m_Parent = this;
  this->m_Children.push_back( Pointer(node) );
}

/** Replace a child (throw exception if not able to replace). */
void
DOMNode::SetChild( DOMNode* node, IdentifierType i )
{
  if ( node == ITK_NULLPTR || this->ShareRoot(node) )
    {
    itkExceptionMacro( "not able to add child" );
    }

  if ( i < 0 || i >= static_cast<IdentifierType>(this->m_Children.size()) )
    {
    itkExceptionMacro( "not able to add child" );
    }

  node->m_Parent = this;
  this->m_Children[i] = node;
}

/** Remove a child by index (throw exception if out of range). */
void
DOMNode::RemoveChild( IdentifierType i )
{
  if ( i < 0 || i >= static_cast<IdentifierType>(m_Children.size()) )
    {
    itkExceptionMacro( "not able to remove child" );
    }

  this->m_Children[i]->m_Parent = ITK_NULLPTR;
  this->m_Children.erase( this->m_Children.begin() + i );
}

/** Remove all attributes and children. */
void
DOMNode::RemoveAllAttributesAndChildren()
{
  this->RemoveAllAttributes();
  this->RemoveAllChildren();
}

/** Retrieve a child by index (return ITK_NULLPTR if out of range). */
DOMNode*
DOMNode::GetChild( IdentifierType i )
{
  if ( i < 0 || i >= static_cast<IdentifierType>(this->m_Children.size()) )
    {
    return ITK_NULLPTR;
    }
  else
    {
    return (DOMNode*)this->m_Children[i];
    }
}

/** Retrieve a child by index (return ITK_NULLPTR if out of range). */
const DOMNode*
DOMNode::GetChild( IdentifierType i ) const
{
  // just use the non-const version and return it as a const,
  // this is safe as the non-const version actually doesn't modify the internal state.
  return const_cast<Self*>(this)->GetChild( i );
}

/** Retrieve a child by tag name and an index (multiple children can have a same tag name, return ITK_NULLPTR if no such child). */
DOMNode*
DOMNode::GetChild( const std::string& tag, IdentifierType i )
{
  IdentifierType k = 0;
  for ( IdentifierType j = 0; j < static_cast<IdentifierType>(this->GetNumberOfChildren()); j++ )
    {
    DOMNode* node = this->GetChild( j );
    if ( tag == node->GetName() )
      {
      if ( k++ == i )
        {
        return node;
        }
      }
    }
  return ITK_NULLPTR;
}

/** Retrieve a child by tag name and an index (multiple children can have a same tag name, return ITK_NULLPTR if no such child). */
const DOMNode*
DOMNode::GetChild( const std::string& tag, IdentifierType i ) const
{
  // just use the non-const version and return it as a const,
  // this is safe as the non-const version actually doesn't modify the internal state.
  return const_cast<Self*>(this)->GetChild( tag, i );
}

/** Retrieve a child by its unique "id" attribute value (return ITK_NULLPTR if not found). */
DOMNode*
DOMNode::GetChildByID( const std::string& value )
{
  for ( IdentifierType j = 0; j < static_cast<IdentifierType>(this->GetNumberOfChildren()); j++ )
    {
    DOMNode* node = this->GetChild( j );
    if ( value == node->GetID() )
      {
      return node;
      }
    }
  return ITK_NULLPTR;
}

/** Retrieve a child by its unique "id" attribute value (return ITK_NULLPTR if not found). */
const DOMNode*
DOMNode::GetChildByID( const std::string& value ) const
{
  // just use the non-const version and return it as a const,
  // this is safe as the non-const version actually doesn't modify the internal state.
  return const_cast<Self*>(this)->GetChildByID( value );
}

/** Retrieve an older or younger sibling by distance (return ITK_NULLPTR if no such sibling). */
DOMNode*
DOMNode::GetSibling( OffsetType i )
{
  DOMNode* parent = this->GetParent();

  if ( parent == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  IdentifierType j;
  for ( j = 0; j < static_cast<IdentifierType>(parent->GetNumberOfChildren()); j++ )
    {
    if ( parent->GetChild(j) == this )
      {
      break;
      }
    }

  j += i;
  if ( j < 0 || j >= static_cast<IdentifierType>(parent->GetNumberOfChildren()) )
    {
    return ITK_NULLPTR;
    }
  else
    {
    return parent->GetChild(j);
    }
}

/** Retrieve an older or younger sibling by distance (return ITK_NULLPTR if no such sibling). */
const DOMNode*
DOMNode::GetSibling( OffsetType i ) const
{
  // just use the non-const version and return it as a const,
  // this is safe as the non-const version actually doesn't modify the internal state.
  return const_cast<Self*>(this)->GetSibling( i );
}

/** Return the root node. */
DOMNode*
DOMNode::GetRoot()
{
  DOMNode* node = this->GetParent();

  if ( node == ITK_NULLPTR )
    {
    return this;
    }
  else
    {
    return node->GetRoot();
    }
}

/** Return the root node. */
const DOMNode*
DOMNode::GetRoot() const
{
  // just use the non-const version and return it as a const,
  // this is safe as the non-const version actually doesn't modify the internal state.
  return const_cast<Self*>(this)->GetRoot();
}

/** Test whether the input node and this node share the same root. */
bool
DOMNode::ShareRoot( const DOMNode* node ) const
{
  return ( node != ITK_NULLPTR && node->GetRoot() == this->GetRoot() );
}

/**
 * The following function finds a child or sibling or relative using a query string or path.
 * A path or QueryString consists of multiple following items that are separated by '/':
 *     -[n]           : an older sibling by distance 1 (when omitted) or n;
 *     +[n]           : a younger sibling by distance 1 (when omitted) or n;
 *     n              : a child at index n;
 *     <tag>[:n]      : a child at index 0 (when omitted) or n after filtering children with a tag name;
 *     ![:n]          : a child at index 0 (when omitted) or n within all text children;
 *     :<id>          : a child by id;
 *     .              : current node;
 *     ..             : parent node;
 *     /<rpath>       : absolute path (denote apath), search from the root.
 *
 * The method returns ITK_NULLPTR if queried node does not exist.
 */
DOMNode*
DOMNode::Find( const std::string& path )
{
  std::string s;

  std::string rpath;
    {
    std::size_t pos = path.find_first_of( '/' );
    if ( pos == std::string::npos )
      {
      s = path;
      rpath = "";
      }
    else
      {
      s = path.substr( 0, pos );
      rpath = path.substr( pos+1 );
      }
    }

  DOMNode* node = ITK_NULLPTR;

  // /<rpath>
  if ( s == "" )
    {
    node = this->GetRoot();
    }

  // if it is an unsigned integer number
  else if ( s[0] >= '0' && s[0] <= '9' )
    {
    std::istringstream iss( s );
    unsigned int i = 0;
    iss >> i;
    if ( !iss.fail() )
      {
      node = this->GetChild( IdentifierType(i) );
      }
    }

  // +[n]
  else if ( s[0] == '+' )
    {
    if ( s.size() > 1 )
      {
      s = s.substr( 1 );
      std::istringstream iss( s );
      unsigned int i = 0;
      iss >> i;
      if ( !iss.fail() )
        {
        node = this->GetSibling( OffsetType(i) );
        }
      }
    else
      {
      node = this->GetSibling( 1 );
      }
    }

  // -[n]
  else if ( s[0] == '-' )
    {
    if ( s.size() > 1 )
      {
      s = s.substr( 1 );
      std::istringstream iss( s );
      unsigned int i = 0;
      iss >> i;
      if ( !iss.fail() )
        {
        node = this->GetSibling( -OffsetType(i) );
        }
      }
    else
      {
      node = this->GetSibling( -1 );
      }
    }

  // :<id>
  else if ( s[0] == ':' )
    {
    s = s.substr( 1 );
    node = this->GetChildByID( s );
    }

  // ..
  else if ( s == ".." )
    {
    node = this->GetParent();
    }

  // .
  else if ( s == "." )
    {
    node = this;
    }

  // <tag>[:n]
  else
    {
    std::size_t pos = s.find_first_of( ':' );
    if ( pos != std::string::npos )
      {
      std::string s2 = s.substr( pos+1 );
      s = s.substr( 0, pos );
      std::istringstream iss( s2 );
      IdentifierType i = 0;
      iss >> i;
      if ( !iss.fail() )
        {
        node = this->GetChild( s, i );
        }
      }
    else
      {
      node = this->GetChild( s );
      }
    }

  if ( rpath == "" || node == ITK_NULLPTR )
    {
    return node;
    }
  else
    {
    return node->Find( rpath );
    }
}

/**
 * The following function finds a child or sibling or relative using a query string or path.
 * The method returns ITK_NULLPTR if queried node does not exist.
 */
const DOMNode*
DOMNode::Find( const std::string& path ) const
{
  // just use the non-const version and return it as a const,
  // this is safe as the non-const version actually doesn't modify the internal state.
  return const_cast<Self*>(this)->Find( path );
}

/** Return the path of this node within its root, in the form of a query string that uses only indices. */
std::string
DOMNode::GetPath() const
{
  std::string path = "";

  const DOMNode* parent = this->GetParent();
  if ( parent == ITK_NULLPTR )
    {
    return path;
    }

  for ( IdentifierType i = 0; i < static_cast<IdentifierType>(parent->GetNumberOfChildren()); i++ )
    {
    if ( parent->GetChild(i) == this )
      {
      // set the global path
      path = parent->GetPath();
      path.append( 1, '/' );
      // add the local path
      std::string s;
      StringTools::FromData( s, i );
      path.append( s );
      // early exit as we've already found the path
      break;
      }
    }
  return path;
}

/** Get a child and cast it to a text node (return ITK_NULLPTR if out of range or not a text node). */
DOMTextNode*
DOMNode::GetTextChild( IdentifierType i )
{
  DOMNode* node = this->GetChild( i );
  return dynamic_cast<DOMTextNode*>(node);
}

/** Get a child and cast it to a text node (return ITK_NULLPTR if out of range or not a text node). */
const DOMTextNode*
DOMNode::GetTextChild( IdentifierType i ) const
{
  const DOMNode* node = this->GetChild( i );
  return dynamic_cast<const DOMTextNode*>(node);
}

/** Generate a text node from a string and add/insert it as a child (see AddChild(node,i)). */
void
DOMNode::AddTextChild( const std::string& text, IdentifierType i )
{
  DOMTextNode::Pointer tnode = DOMTextNode::New();
  tnode->SetText( text );
  this->AddChild( (DOMTextNode*)tnode, i );
}

/** Generate a text node from a string and add/insert in front of all children. */
void
DOMNode::AddTextChildAtBegin( const std::string& text )
{
  DOMTextNode::Pointer tnode = DOMTextNode::New();
  tnode->SetText( text );
  this->AddChildAtBegin( (DOMTextNode*)tnode );
}

/** Generate a text node from a string and add/insert at the end of all children. */
void
DOMNode::AddTextChildAtEnd( const std::string& text )
{
  DOMTextNode::Pointer tnode = DOMTextNode::New();
  tnode->SetText( text );
  this->AddChildAtEnd( (DOMTextNode*)tnode );
}

/** Generate a text node from a string and replace a child with it (see SetChild(node,i). */
void
DOMNode::SetTextChild( const std::string& text, IdentifierType i )
{
  DOMTextNode::Pointer tnode = DOMTextNode::New();
  tnode->SetText( text );
  this->SetChild( (DOMTextNode*)tnode, i );
}

} // namespace itk
