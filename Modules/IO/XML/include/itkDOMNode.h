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

#ifndef itkDOMNode_h
#define itkDOMNode_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "ITKIOXMLExport.h"

#include <string>
#include <vector>
#include <list>
#include <map>

namespace itk
{

class DOMTextNode; // forward declaration

/**
 * \class DOMNode
 * \brief Class to represent a node in a Document Object Model (DOM) tree structure.
 *
 * A DOM is a structured document representation, in our case parsed from an XML stream (i.e. a file or a string).
 * It is a tree structure in which each node has links to its children nodes and
 * to its parent node (if it is not the root). This class serves as a base node corresponding to an
 * XML tag that only contains attributes (no content for the tag itself).
 *
 * \note We reserve the use of the attribute "id" (all combinations of upper and lower
 *       case) for uniquely identifying an XML structure among its siblings. That is, we assume
 *       that this tag is unique among all siblings in an XML tree structure. If it is not unique,
 *       the user may not be able to correctly retrieve a node by function GetChildByID(), or to search for
 *       a node using Find() with a query string that is based on "id".
 *
 * \ingroup ITKIOXML
 */
class ITKIOXML_EXPORT DOMNode : public Object
{
public:
  /** Standard class typedefs. */
  typedef DOMNode                     Self;
  typedef Object                      Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DOMNode, Object);

  /** Container to return all or a subset of the children of a DOM node. */
  typedef std::vector<DOMNode*>         ChildrenListType;
  typedef std::vector<const DOMNode*>   ConstChildrenListType;

  /** An attribute is a pair of <key,value>, both key and value are strings. */
  typedef std::string AttributeKeyType;
  typedef std::string AttributeValueType;

  typedef std::pair<const AttributeKeyType,AttributeValueType> AttributeItemType;

  /** Container to return the attributes of a DOM node. */
  typedef std::list<AttributeItemType> AttributesListType;

  typedef std::size_t SizeType;
  typedef int         IdentifierType;
  typedef int         OffsetType;

  /** Retrieve the parent node. */
  virtual void SetParent( DOMNode* node );
  DOMNode* GetParent();
  const DOMNode* GetParent() const;

  /** Retrieve the tag name of this node. */
  itkSetMacro( Name, std::string& );
  itkGetConstReferenceMacro( Name, std::string );

  /** Retrieve the special attribute "id" of this node. */
  itkSetMacro( ID, std::string& );
  itkGetConstReferenceMacro( ID, std::string );

  /** Retrieve an attribute by key (return an empty string if not found). */
  virtual std::string GetAttribute( const std::string& key ) const;
  /** Check whether has an attribute. */
  virtual bool HasAttribute( const std::string& key ) const;
  /** Add or replace an attribute. */
  virtual void SetAttribute( const std::string& key, const std::string& value );
  /** Remove an attribute by key (throw exception if not found). */
  virtual void RemoveAttribute( const std::string& key );

  /**
   * Return all attributes, in the order of being parsed or added (default),
   * or alphabetic order of the attribute keys (keepOriginalOrder = false).
   */
  virtual void GetAllAttributes( AttributesListType& output, bool keepOriginalOrder = true ) const;
  /** Remove all attributes. */
  virtual void RemoveAllAttributes();

  /** Get number of children. */
  virtual SizeType GetNumberOfChildren() const;

  /** Return all children. */
  virtual void GetAllChildren( ChildrenListType& output );
  /** Return all children for read-only access. */
  virtual void GetAllChildren( ConstChildrenListType& output ) const;

  /** Return all children of a same tag name. */
  virtual void GetChildren( const std::string& tag, ChildrenListType& output );
  /** Return all children of a same tag name for read-only access. */
  virtual void GetChildren( const std::string& tag, ConstChildrenListType& output ) const;

  /** Remove all children. */
  virtual void RemoveAllChildren();

  /** Add a child in front of another child (throw exception if not able to add). */
  virtual void AddChild( DOMNode* node, IdentifierType i=0 );
  /** Add a child in front of the children list (throw exception if not able to add). */
  virtual void AddChildAtBegin( DOMNode* node );
  /** Add a child at the end of the children list (throw exception if not able to add). */
  virtual void AddChildAtEnd( DOMNode* node );

  /** Replace a child (throw exception if not able to replace). */
  virtual void SetChild( DOMNode* node, IdentifierType i=0 );

  /** Remove a child by index (throw exception if not able to remove). */
  virtual void RemoveChild( IdentifierType i=0 );

  /** Remove all attributes and children. */
  virtual void RemoveAllAttributesAndChildren();

  /** Retrieve a child by index (return ITK_NULLPTR if i is out of range). */
  virtual DOMNode* GetChild( IdentifierType i=0 );
  virtual const DOMNode* GetChild( IdentifierType i=0 ) const;

  /** Retrieve a child by tag name and an index (multiple children can have a same tag name, return ITK_NULLPTR if no such child). */
  virtual DOMNode* GetChild( const std::string& tag, IdentifierType i=0 );
  virtual const DOMNode* GetChild( const std::string& tag, IdentifierType i=0 ) const;

  /** Retrieve a child by its unique "id" attribute value (return ITK_NULLPTR if not found). */
  virtual DOMNode* GetChildByID( const std::string& value );
  virtual const DOMNode* GetChildByID( const std::string& value ) const;

  /** Retrieve an older or younger sibling by distance (return ITK_NULLPTR if no such sibling). */
  virtual DOMNode* GetSibling( OffsetType i );
  virtual const DOMNode* GetSibling( OffsetType i ) const;

  /** Return the root node. */
  virtual DOMNode* GetRoot();
  virtual const DOMNode* GetRoot() const;

  /** Test whether the input node and this node share the same root. */
  virtual bool ShareRoot( const DOMNode* node ) const;

  /**
   * The following function finds a child or sibling or relative using a query string or path.
   * A path or QueryString consists of multiple following items that are separated by '/':
   * \verbatim
   *     -[n]           : an older sibling by distance 1 (when omitted) or n;
   *     +[n]           : a younger sibling by distance 1 (when omitted) or n;
   *     n              : a child at index n;
   *     <tag>[:n]      : a child at index 0 (when omitted) or n after filtering children with a tag name;
   *     ![:n]          : a child at index 0 (when omitted) or n within all text children;
   *     :<id>          : a child by id;
   *     .              : current node;
   *     ..             : parent node;
   *     /<rpath>       : absolute path (denote apath), search from the root.
   * \endverbatim
   *
   * The method returns ITK_NULLPTR if queried node does not exist.
   */
  virtual DOMNode* Find( const std::string& path );
  virtual const DOMNode* Find( const std::string& path ) const;

  /** Return the path of this node within its root, in the form of a query string that uses only indices. */
  virtual std::string GetPath() const;

  /** Get a child and cast it to a text node (return ITK_NULLPTR if out of range or not a text node). */
  virtual DOMTextNode* GetTextChild( IdentifierType i=0 );
  virtual const DOMTextNode* GetTextChild( IdentifierType i=0 ) const;

  /** Generate a text node from a string and add/insert it as a child (see AddChild(node,i)). */
  virtual void AddTextChild( const std::string& text, IdentifierType i=0 );
  /** Generate a text node from a string and add/insert in front of all children. */
  virtual void AddTextChildAtBegin( const std::string& text );
  /** Generate a text node from a string and add/insert at the end of all children. */
  virtual void AddTextChildAtEnd( const std::string& text );

  /** Generate a text node from a string and replace a child with it (see SetChild(node,i). */
  virtual void SetTextChild( const std::string& text, IdentifierType i=0 );

protected:
  DOMNode();

private:
  /** The parent node that this node was placed into. */
  DOMNode* m_Parent;

  /** The XML tag of this node. */
  std::string m_Name;

  /** The special attribute "id" of this node. */
  std::string m_ID;

  /** Internally the children are stored in a vector. */
  typedef std::vector<Pointer> ChildrenContainer;
  ChildrenContainer m_Children;

  /** Internally the attributes are stored in a map. */
  typedef std::map<AttributeKeyType,AttributeValueType> AttributesContainer;
  AttributesContainer m_Attributes;

  /** Container to keep the inserting orders of the attributes. */
  typedef std::list<AttributeItemType*> OrderedAttributesContainer;
  OrderedAttributesContainer m_OrderedAttributes;

  ITK_DISALLOW_COPY_AND_ASSIGN(DOMNode);
};

} // namespace itk

#include "itkDOMTextNode.h"

#include "itkStringTools.h"
#include "itkFancyString.h"

#endif // itkDOMNode_h
