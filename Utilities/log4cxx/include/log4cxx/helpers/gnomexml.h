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
 
#ifndef _LOG4CXX_HELPERS_GNOMEXML_H
#define _LOG4CXX_HELPERS_GNOMEXML_H

#include <log4cxx/config.h>

#ifdef HAVE_LIBXML2

#include <log4cxx/helpers/xml.h>
#include <log4cxx/helpers/objectimpl.h>

#include <libxml/tree.h>

namespace log4cxx
{
  namespace helpers
  {
    class GnomeXMLDOMNode : 
      virtual public XMLDOMNode,
      virtual public ObjectImpl
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(GnomeXMLDOMNode)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(XMLDOMNode)
      END_LOG4CXX_CAST_MAP()

      GnomeXMLDOMNode(xmlNodePtr node);

      virtual XMLDOMNodeListPtr getChildNodes();
      virtual XMLDOMNodeType getNodeType()
        { return NOT_IMPLEMENTED_NODE; }

      virtual XMLDOMDocumentPtr getOwnerDocument();

    protected:
      xmlNodePtr node;
    };

    class GnomeXMLDOMDocument : 
      virtual public XMLDOMDocument,
      virtual public ObjectImpl
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(GnomeXMLDOMDocument)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(XMLDOMDocument)
        LOG4CXX_CAST_ENTRY(XMLDOMNode)
      END_LOG4CXX_CAST_MAP()

      GnomeXMLDOMDocument();
      GnomeXMLDOMDocument(xmlDocPtr document);
      ~GnomeXMLDOMDocument();

      virtual XMLDOMNodeListPtr getChildNodes();
      virtual XMLDOMNodeType getNodeType()
        { return XMLDOMNode::DOCUMENT_NODE; }
      virtual XMLDOMDocumentPtr getOwnerDocument();
      virtual void load(const String& fileName);
      virtual XMLDOMElementPtr getDocumentElement();
      virtual XMLDOMElementPtr getElementById(
        const String& tagName, const String& elementId);

    protected:
      xmlDocPtr document;
      bool ownDocument;
    };

    class GnomeXMLDOMElement : 
      virtual public XMLDOMElement,
      virtual public ObjectImpl
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(GnomeXMLDOMElement)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(XMLDOMElement)
        LOG4CXX_CAST_ENTRY(XMLDOMNode)
      END_LOG4CXX_CAST_MAP()

      GnomeXMLDOMElement(xmlNodePtr element);

      virtual XMLDOMNodeListPtr getChildNodes();
      virtual XMLDOMNodeType getNodeType()
        { return XMLDOMNode::ELEMENT_NODE; }
      virtual XMLDOMDocumentPtr getOwnerDocument();
      virtual String getTagName();
      virtual String getAttribute(const String& name);

    protected:
      xmlNodePtr element;
    };

    class GnomeXMLDOMNodeList : 
      virtual public XMLDOMNodeList,
      virtual public ObjectImpl
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(GnomeXMLDOMNodeList)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(XMLDOMNodeList)
      END_LOG4CXX_CAST_MAP()

      GnomeXMLDOMNodeList(xmlNodePtr firstChild);

      virtual int getLength();
      virtual XMLDOMNodePtr item(int index);

    protected:
      xmlNodePtr firstChild;
      xmlNodePtr currentChild;
      int currentIndex;
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif // HAVE_LIBXML2
#endif // _LOG4CXX_HELPERS_MSXML_H
