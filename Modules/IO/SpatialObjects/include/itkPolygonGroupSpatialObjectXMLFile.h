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
#ifndef itkPolygonGroupSpatialObjectXMLFile_h
#define itkPolygonGroupSpatialObjectXMLFile_h


#include "itkPolygonGroupSpatialObject.h"
#include "itkXMLFile.h"
namespace itk
{
/* 3D Polygon Groups only ones that make sense for this data type */
using PGroupSpatialObjectType = PolygonGroupSpatialObject< 3 >;

/** \class PolygonGroupSpatialObjectXMLFileReader
 *
 * Reads an XML-format file containing a list of polygons, and
 * creates a corresponding PolygonGroupSpatialObject
 * \ingroup ITKIOSpatialObjects
 */
class PolygonGroupSpatialObjectXMLFileReader:
  public XMLReader< PGroupSpatialObjectType >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolygonGroupSpatialObjectXMLFileReader);

  /** Standard type alias */
  using Self = PolygonGroupSpatialObjectXMLFileReader;
  using Superclass = XMLReader< PGroupSpatialObjectType >;
  using Pointer = SmartPointer< Self >;

  using PolygonGroupType = PGroupSpatialObjectType;
  using PolygonSpatialObjectType = PolygonSpatialObject< 3 >;
  using PointType = SpatialObjectPoint< 3 >;
  using PointListType = std::vector< PointType >;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolygonGroupSpatialObjectXMLFileReader, XMLReader);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

public:
  /** Determine if a file can be read */
  int CanReadFile(const char *name) override;

protected:
  PolygonGroupSpatialObjectXMLFileReader() {}
  ~PolygonGroupSpatialObjectXMLFileReader() override {}

  void StartElement(const char *name, const char **atts) override;

  void EndElement(const char *name) override;

  void CharacterDataHandler(const char *inData, int inLength) override;

private:
  PGroupSpatialObjectType::Pointer  m_PGroup;
  PolygonSpatialObjectType::Pointer m_CurPoly;
  PointListType                     m_CurPointList;
  std::string                       m_CurCharacterData;
};

/** \class PolygonGroupSpatialObjectXMLFileWriter
 *
 * Writes an XML-format file containing a list of polygons,
 * based on a PolygonGroupSpatialObject.
 * \ingroup ITKIOSpatialObjects
 */
class PolygonGroupSpatialObjectXMLFileWriter:
  public XMLWriterBase< PGroupSpatialObjectType >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolygonGroupSpatialObjectXMLFileWriter);

  /** standard type alias */
  using Superclass = XMLWriterBase< PGroupSpatialObjectType >;
  using Self = PolygonGroupSpatialObjectXMLFileWriter;
  using Pointer = SmartPointer< Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolygonGroupSpatialObjectXMLFileWriter,
               XMLWriterBase< PGroupSpatialObjectType > );
  using PolygonGroupType = PGroupSpatialObjectType;
  using PolygonSpatialObjectType = PolygonSpatialObject< 3 >;
  /** Test whether a file is writable. */
  int CanWriteFile(const char *name) override;

  /** Actually write out the file in question */
  int WriteFile() override;

protected:
  PolygonGroupSpatialObjectXMLFileWriter() {}
  ~PolygonGroupSpatialObjectXMLFileWriter() override {}
};
}
#endif
