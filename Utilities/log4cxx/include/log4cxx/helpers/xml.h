/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
#ifndef _LOG4CXX_HELPERS_XML_H
#define _LOG4CXX_HELPERS_XML_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/object.h>
#include <log4cxx/helpers/exception.h>

namespace log4cxx
{
  namespace helpers
  {
    class XMLDOMNode;
    typedef helpers::ObjectPtrT<XMLDOMNode> XMLDOMNodePtr;

    class XMLDOMDocument;
    typedef helpers::ObjectPtrT<XMLDOMDocument> XMLDOMDocumentPtr;

    class XMLDOMElement;
    typedef helpers::ObjectPtrT<XMLDOMElement> XMLDOMElementPtr;

    class XMLDOMNodeList;
    typedef helpers::ObjectPtrT<XMLDOMNodeList> XMLDOMNodeListPtr;

    class LOG4CXX_EXPORT DOMException : public RuntimeException
    {
    };

    /**
    The XMLDOMNode interface is the primary datatype for the entire Document
    Object Model.
    */
    class LOG4CXX_EXPORT XMLDOMNode : virtual public Object
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(XMLDOMNode)
      enum XMLDOMNodeType
      {
        NOT_IMPLEMENTED_NODE = 0,
        ELEMENT_NODE = 1,
        DOCUMENT_NODE = 9
      };
      
      virtual XMLDOMNodeListPtr getChildNodes() = 0;
      virtual XMLDOMNodeType getNodeType() = 0;
      virtual XMLDOMDocumentPtr getOwnerDocument() = 0;
    };

    /**
    The XMLDOMDocument interface represents an entire XML document.
    
    Conceptually, it is the root of the document tree, and provides the
    primary access to the document's data.
    */
    class LOG4CXX_EXPORT XMLDOMDocument : virtual public XMLDOMNode
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(XMLDOMDocument)
      virtual void load(const String& fileName) = 0;
      virtual XMLDOMElementPtr getDocumentElement() = 0;
      virtual XMLDOMElementPtr getElementById(const String& tagName,
        const String& elementId) = 0;
    };

    /** 
    The XMLDOMElement interface represents an element in an XML document
    */
    class LOG4CXX_EXPORT XMLDOMElement : virtual public XMLDOMNode
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(XMLDOMElement)
      virtual String getTagName() = 0;
      virtual String getAttribute(const String& name) = 0;
    };

    /**
    The XMLDOMNodeList interface provides the abstraction of an ordered
    collection of nodes, without defining or constraining how this
    collection is implemented. 
    
    XMLDOMNodeList objects in the DOM are live.

    The items in the XMLDOMNodeList are accessible via an integral index,
    starting from 0. 
    */
    class LOG4CXX_EXPORT XMLDOMNodeList : virtual public Object
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(XMLDOMNodeList)
      virtual int getLength() = 0;
      virtual XMLDOMNodePtr item(int index) = 0;
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif // _LOG4CXX_HELPERS_XML_H

