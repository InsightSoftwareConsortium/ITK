/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolygonGroupSpatialObjectXMLFile.h
  Language:  C++
  Date:      $Date$
  Version:   $1.0$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __tkPolygonGroupSpatialObjectXMLFile_h
#define __itkPolygonGroupSpatialObjectXMLFile_h

#include "itkXMLFile.h"
#include "itkPolygonGroupSpatialObject.h"
namespace itk
{

/* 3D Polygon Groups only ones that make sense for this data type */
typedef PolygonGroupSpatialObject<3> PGroupSpatialObjectType;

/** \class PolygonGroupSpatialObjectXMLFileReader
 * 
 * Reads an XML-format file containing a list of polygons, and
 * creates a corresponding PolygonGroupSpatialObject
 */
class PolygonGroupSpatialObjectXMLFileReader :
public XMLReader<PGroupSpatialObjectType>
{
 public:
  /** Standard typedefs */ 
  typedef PolygonGroupSpatialObjectXMLFileReader Self;
  typedef XMLReader<PGroupSpatialObjectType> Superclass;
  typedef SmartPointer<Self> Pointer;

  typedef PGroupSpatialObjectType PolygonGroupType;
  typedef PolygonSpatialObject<3> PolygonSpatialObjectType;
  typedef SpatialObjectPoint<3> PointType;
  typedef std::list<PointType> PointListType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Self,Superclass);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
public:
  /** Determine if a file can be read */
  virtual int CanReadFile(const char* name);
protected:
  virtual void StartElement(const char * name,const char **atts);
  virtual void EndElement(const char *name);
  virtual void CharacterDataHandler(const char *inData, int inLength);
private:
  PGroupSpatialObjectType::Pointer m_PGroup;
  PolygonSpatialObjectType::Pointer m_CurPoly;
  PointListType m_CurPointList;
  std::string m_CurCharacterData;
};

/** \class PolygonGroupSpatialObjectXMLFileWriter
 * 
 * Writes an XML-format file containing a list of polygons,
 * based on a PolygonGroupSpatialObject.
 */
class PolygonGroupSpatialObjectXMLFileWriter :
    public XMLWriterBase<PGroupSpatialObjectType>
{
public:
  /** standard typedefs */
  typedef XMLWriterBase<PGroupSpatialObjectType> Superclass;
  typedef PolygonGroupSpatialObjectXMLFileWriter Self;
  typedef SmartPointer<Self> Pointer;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
    /** Run-time type information (and related methods). */
  itkTypeMacro(PolygonGroupSpatialObjectXMLFileWriter,
               XMLWriterBase<PGroupSpatialObjectType>);
  typedef PGroupSpatialObjectType PolygonGroupType;
  typedef PolygonSpatialObject<3> PolygonSpatialObjectType;
  /** Test whether a file is writable. */
  virtual int CanWriteFile(const char* name);
  /** Actually write out the file in question */
  virtual int WriteFile();
};

}
#endif
