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
 
#define _WIN32_DCOM
#include <log4cxx/config.h>

#ifdef HAVE_MS_XML

#include <windows.h>
#include <log4cxx/helpers/msxml.h>
#include <log4cxx/helpers/loglog.h>
#include <objbase.h>
#include <log4cxx/helpers/exception.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(MsXMLDOMDocument)
IMPLEMENT_LOG4CXX_OBJECT(MsXMLDOMNodeList)
IMPLEMENT_LOG4CXX_OBJECT(MsXMLDOMNode)
IMPLEMENT_LOG4CXX_OBJECT(MsXMLDOMElement)

#define EXEC(stmt) { HRESULT hr = stmt; if (FAILED(hr)) throw DOMException(); }

// MsXMLDOMNode

MsXMLDOMNode::MsXMLDOMNode(MSXML::IXMLDOMNodePtr node)
: node(node)
{
}

XMLDOMNodeListPtr MsXMLDOMNode::getChildNodes()
{
  MSXML::IXMLDOMNodeListPtr nodeList;
  EXEC(node->get_childNodes(&nodeList));
  return new MsXMLDOMNodeList(nodeList);
}

XMLDOMDocumentPtr MsXMLDOMNode::getOwnerDocument()
{
  MSXML::IXMLDOMDocumentPtr document;
  EXEC(node->get_ownerDocument(&document));
  return new MsXMLDOMDocument(document);
}

// MsXMLDOMDocument

MsXMLDOMDocument::MsXMLDOMDocument(MSXML::IXMLDOMDocumentPtr document)
: document(document), mustCallCoUninitialize(false)
{
}

MsXMLDOMDocument::MsXMLDOMDocument() : mustCallCoUninitialize(false)
{
  HRESULT hRes = ::CoInitializeEx(0, COINIT_MULTITHREADED);
  if (FAILED(hRes))
  {
    switch (hRes)
    {
    case RPC_E_CHANGED_MODE:
      break;

    default:
      throw RuntimeException(_T("Cannot Initialize COM"));
    }
  }
  else
  {
    mustCallCoUninitialize = true;
  }

  hRes = document.CreateInstance(L"Msxml2.DOMDocument.3.0");
  if (FAILED(hRes))
  {
    hRes = document.CreateInstance(L"Msxml2.DOMDocument.2.6");
    if (FAILED(hRes))
    {
      hRes = document.CreateInstance(L"Msxml2.DOMDocument");
      if (FAILED(hRes))
      {
        hRes = document.CreateInstance(L"Msxml.DOMDocument");
        if (FAILED(hRes))
        {
          throw DOMException();
        }
      }
    }
  }
}

MsXMLDOMDocument::~MsXMLDOMDocument()
{
  document.Release();

  if (mustCallCoUninitialize)
  {
    ::CoUninitialize();
  }
}

XMLDOMNodeListPtr MsXMLDOMDocument::getChildNodes()
{
  MSXML::IXMLDOMNodeListPtr nodeList;
  EXEC(document->get_childNodes(&nodeList));
  return new MsXMLDOMNodeList(nodeList);
}

XMLDOMDocumentPtr MsXMLDOMDocument::getOwnerDocument()
{
  return this;
}

void MsXMLDOMDocument::load(const String& fileName)
{
  try
  {
    VARIANT_BOOL bSuccess = document->load(fileName.c_str());

    if (!bSuccess)
    {
      MSXML::IXMLDOMParseErrorPtr parseError = document->parseError;

      // fetch errorcode
      long errorCode = parseError->errorCode;

      _bstr_t reason = parseError->reason;
      long line = parseError->line;
      long linepos = parseError->linepos;

      // remove \n or \r
      int len = reason.length();
      while(len > 0 && (((BSTR)reason)[len -1] == L'\n' ||
        ((BSTR)reason)[len -1] == L'\r'))
      {
        ((BSTR)reason)[len -1] = L'\0';
        len--;
      }

      USES_CONVERSION;
      LOGLOG_ERROR(_T("Could not open [") << fileName << _T("] : ") 
        << W2T((BSTR)reason) << _T("(line ") << line << _T(", column ")
        << linepos << _T(")"));
    }

  }
  catch(_com_error&)
  {
    LogLog::error(_T("Could not open [")+fileName+_T("]."));
    throw DOMException();
  }
}

XMLDOMElementPtr MsXMLDOMDocument::getDocumentElement()
{
  MSXML::IXMLDOMElementPtr element;
  EXEC(document->get_documentElement(&element));
  return new MsXMLDOMElement(element);
}

XMLDOMElementPtr MsXMLDOMDocument::getElementById(const String& tagName, const String& elementId)
{
  MSXML::IXMLDOMElementPtr element;

  try
  {
    MSXML::IXMLDOMNodeListPtr list = document->getElementsByTagName(tagName.c_str());
    for (int t=0; t < list->length; t++)
    {
      MSXML::IXMLDOMNodePtr node = list->item[t];
      MSXML::IXMLDOMNamedNodeMapPtr map= node->attributes;
      MSXML::IXMLDOMNodePtr attrNode = map->getNamedItem(L"name");
      _bstr_t nodeValue = attrNode->nodeValue;

      USES_CONVERSION;
      if (elementId == W2T((BSTR)nodeValue))
      {
        element = node;
        break;
      }
    }
  }
  catch(_com_error&)
  {
    throw DOMException();
  }

  return new MsXMLDOMElement(element);
}

// MsXMLDOMElement
MsXMLDOMElement::MsXMLDOMElement(MSXML::IXMLDOMElementPtr element)
: element(element)
{
}

XMLDOMNodeListPtr MsXMLDOMElement::getChildNodes()
{
  MSXML::IXMLDOMNodeListPtr nodeList;
  EXEC(element->get_childNodes(&nodeList));
  return new MsXMLDOMNodeList(nodeList);
}

XMLDOMDocumentPtr MsXMLDOMElement::getOwnerDocument()
{
  MSXML::IXMLDOMDocumentPtr document;
  EXEC(element->get_ownerDocument(&document));
  return new MsXMLDOMDocument(document);
}

String MsXMLDOMElement::getTagName()
{
  try
  {
    _bstr_t tagName = element->tagName;
    USES_CONVERSION;
    return W2T((BSTR)tagName);
  }
  catch(_com_error&)
  {
    throw DOMException();
  }
}

String MsXMLDOMElement::getAttribute(const String& name)
{
  try
  {
    _variant_t attribute = element->getAttribute(name.c_str());
    if (attribute.vt == VT_NULL)
    {
      return String();
    }
    else
    {
      return (const TCHAR *)_bstr_t(attribute);
    }
  }
  catch(_com_error&)
  {
    throw DOMException();
  }
}

// MsXMLDOMNodeList  
MsXMLDOMNodeList::MsXMLDOMNodeList(MSXML::IXMLDOMNodeListPtr nodeList)
: nodeList(nodeList)
{
}

int MsXMLDOMNodeList::getLength()
{
  long length;
  EXEC(nodeList->get_length(&length));

  return (int)length;
}

XMLDOMNodePtr MsXMLDOMNodeList::item(int index)
{
  try
  {
    MSXML::IXMLDOMNodePtr node = nodeList->item[index];

    if (node->nodeType == MSXML::NODE_ELEMENT)
    {
      return new MsXMLDOMElement(MSXML::IXMLDOMElementPtr(node));
    }
    else
    {
      return new MsXMLDOMNode(node);
    }
  }
  catch(_com_error&)
  {
    throw DOMException();
  }
}

#endif

