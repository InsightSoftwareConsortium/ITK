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
typedef PolygonGroupSpatialObject< 3 > PGroupSpatialObjectType;

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
  /** Standard typedefs */
  typedef PolygonGroupSpatialObjectXMLFileReader Self;
  typedef XMLReader< PGroupSpatialObjectType >   Superclass;
  typedef SmartPointer< Self >                   Pointer;

  typedef PGroupSpatialObjectType   PolygonGroupType;
  typedef PolygonSpatialObject< 3 > PolygonSpatialObjectType;
  typedef SpatialObjectPoint< 3 >   PointType;
  typedef std::vector< PointType >  PointListType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolygonGroupSpatialObjectXMLFileReader, XMLReader);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

public:
  /** Determine if a file can be read */
  virtual int CanReadFile(const char *name) ITK_OVERRIDE;

protected:
  PolygonGroupSpatialObjectXMLFileReader() {}
  virtual ~PolygonGroupSpatialObjectXMLFileReader() ITK_OVERRIDE {}

  virtual void StartElement(const char *name, const char **atts) ITK_OVERRIDE;

  virtual void EndElement(const char *name) ITK_OVERRIDE;

  virtual void CharacterDataHandler(const char *inData, int inLength) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolygonGroupSpatialObjectXMLFileReader);

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
  /** standard typedefs */
  typedef XMLWriterBase< PGroupSpatialObjectType > Superclass;
  typedef PolygonGroupSpatialObjectXMLFileWriter   Self;
  typedef SmartPointer< Self >                     Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolygonGroupSpatialObjectXMLFileWriter,
               XMLWriterBase< PGroupSpatialObjectType > );
  typedef PGroupSpatialObjectType   PolygonGroupType;
  typedef PolygonSpatialObject< 3 > PolygonSpatialObjectType;
  /** Test whether a file is writable. */
  virtual int CanWriteFile(const char *name) ITK_OVERRIDE;

  /** Actually write out the file in question */
  virtual int WriteFile() ITK_OVERRIDE;

protected:
  PolygonGroupSpatialObjectXMLFileWriter() {}
  virtual ~PolygonGroupSpatialObjectXMLFileWriter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolygonGroupSpatialObjectXMLFileWriter);
};
}
#endif
