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
#ifndef itk_expat_mangle_h
#define itk_expat_mangle_h

/*

This header file mangles all symbols exported from the expat library.
It is included in all files while building the expat library.  Due to
namespace pollution, no expat headers should be included in .h files in
ITK.

The following command was used to obtain the symbol list:

nm libITKEXPAT-4.9.a | grep " [TRD] " |  awk '{ print $3 }' | sort | awk '{ print "#define "$1" itk_expat_"$1 }'

*/

#define GetVTKEXpatXMLRole_RCSId itk_expat_GetVTKEXpatXMLRole_RCSId
#define XML_DefaultCurrent itk_expat_XML_DefaultCurrent
#define XML_ErrorString itk_expat_XML_ErrorString
#define XML_ExpatVersion itk_expat_XML_ExpatVersion
#define XML_ExpatVersionInfo itk_expat_XML_ExpatVersionInfo
#define XML_ExternalEntityParserCreate itk_expat_XML_ExternalEntityParserCreate
#define XML_GetBase itk_expat_XML_GetBase
#define XML_GetBuffer itk_expat_XML_GetBuffer
#define XML_GetCurrentByteCount itk_expat_XML_GetCurrentByteCount
#define XML_GetCurrentByteIndex itk_expat_XML_GetCurrentByteIndex
#define XML_GetCurrentColumnNumber itk_expat_XML_GetCurrentColumnNumber
#define XML_GetCurrentLineNumber itk_expat_XML_GetCurrentLineNumber
#define XML_GetErrorCode itk_expat_XML_GetErrorCode
#define XML_GetIdAttributeIndex itk_expat_XML_GetIdAttributeIndex
#define XML_GetInputContext itk_expat_XML_GetInputContext
#define XML_GetSpecifiedAttributeCount itk_expat_XML_GetSpecifiedAttributeCount
#define XmlGetUtf16InternalEncoding itk_expat_XmlGetUtf16InternalEncoding
#define XmlGetUtf16InternalEncodingNS itk_expat_XmlGetUtf16InternalEncodingNS
#define XmlGetUtf8InternalEncoding itk_expat_XmlGetUtf8InternalEncoding
#define XmlGetUtf8InternalEncodingNS itk_expat_XmlGetUtf8InternalEncodingNS
#define XmlInitEncoding itk_expat_XmlInitEncoding
#define XmlInitEncodingNS itk_expat_XmlInitEncodingNS
#define XmlInitUnknownEncoding itk_expat_XmlInitUnknownEncoding
#define XmlInitUnknownEncodingNS itk_expat_XmlInitUnknownEncodingNS
#define XML_Parse itk_expat_XML_Parse
#define XML_ParseBuffer itk_expat_XML_ParseBuffer
#define XML_ParserCreate itk_expat_XML_ParserCreate
#define XML_ParserCreate_MM itk_expat_XML_ParserCreate_MM
#define XML_ParserCreateNS itk_expat_XML_ParserCreateNS
#define XML_ParserFree itk_expat_XML_ParserFree
#define XmlParseXmlDecl itk_expat_XmlParseXmlDecl
#define XmlParseXmlDeclNS itk_expat_XmlParseXmlDeclNS
#define XmlPrologStateInit itk_expat_XmlPrologStateInit
#define XmlPrologStateInitExternalEntity itk_expat_XmlPrologStateInitExternalEntity
#define XML_SetAttlistDeclHandler itk_expat_XML_SetAttlistDeclHandler
#define XML_SetBase itk_expat_XML_SetBase
#define XML_SetCdataSectionHandler itk_expat_XML_SetCdataSectionHandler
#define XML_SetCharacterDataHandler itk_expat_XML_SetCharacterDataHandler
#define XML_SetCommentHandler itk_expat_XML_SetCommentHandler
#define XML_SetDefaultHandler itk_expat_XML_SetDefaultHandler
#define XML_SetDefaultHandlerExpand itk_expat_XML_SetDefaultHandlerExpand
#define XML_SetDoctypeDeclHandler itk_expat_XML_SetDoctypeDeclHandler
#define XML_SetElementDeclHandler itk_expat_XML_SetElementDeclHandler
#define XML_SetElementHandler itk_expat_XML_SetElementHandler
#define XML_SetEncoding itk_expat_XML_SetEncoding
#define XML_SetEndCdataSectionHandler itk_expat_XML_SetEndCdataSectionHandler
#define XML_SetEndDoctypeDeclHandler itk_expat_XML_SetEndDoctypeDeclHandler
#define XML_SetEndElementHandler itk_expat_XML_SetEndElementHandler
#define XML_SetEndNamespaceDeclHandler itk_expat_XML_SetEndNamespaceDeclHandler
#define XML_SetEntityDeclHandler itk_expat_XML_SetEntityDeclHandler
#define XML_SetExternalEntityRefHandler itk_expat_XML_SetExternalEntityRefHandler
#define XML_SetExternalEntityRefHandlerArg itk_expat_XML_SetExternalEntityRefHandlerArg
#define XML_SetNamespaceDeclHandler itk_expat_XML_SetNamespaceDeclHandler
#define XML_SetNotationDeclHandler itk_expat_XML_SetNotationDeclHandler
#define XML_SetNotStandaloneHandler itk_expat_XML_SetNotStandaloneHandler
#define XML_SetParamEntityParsing itk_expat_XML_SetParamEntityParsing
#define XML_SetProcessingInstructionHandler itk_expat_XML_SetProcessingInstructionHandler
#define XML_SetReturnNSTriplet itk_expat_XML_SetReturnNSTriplet
#define XML_SetStartCdataSectionHandler itk_expat_XML_SetStartCdataSectionHandler
#define XML_SetStartDoctypeDeclHandler itk_expat_XML_SetStartDoctypeDeclHandler
#define XML_SetStartElementHandler itk_expat_XML_SetStartElementHandler
#define XML_SetStartNamespaceDeclHandler itk_expat_XML_SetStartNamespaceDeclHandler
#define XML_SetUnknownEncodingHandler itk_expat_XML_SetUnknownEncodingHandler
#define XML_SetUnparsedEntityDeclHandler itk_expat_XML_SetUnparsedEntityDeclHandler
#define XML_SetUserData itk_expat_XML_SetUserData
#define XML_SetXmlDeclHandler itk_expat_XML_SetXmlDeclHandler
#define XmlSizeOfUnknownEncoding itk_expat_XmlSizeOfUnknownEncoding
#define XML_UseParserAsHandlerArg itk_expat_XML_UseParserAsHandlerArg
#define XmlUtf16Encode itk_expat_XmlUtf16Encode
#define XmlUtf8Encode itk_expat_XmlUtf8Encode

#endif /* itk_expat_mangle_h */
