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
#ifndef _itkPolygonGroupSpatialObjectXMLFile_hxx
#define _itkPolygonGroupSpatialObjectXMLFile_hxx

#include "itkPolygonGroupSpatialObjectXMLFile.h"
#include "itksys/SystemTools.hxx"
#include "itkMetaDataObject.h"
#include "itkIOCommon.h"
#define RAISE_EXCEPTION(s)                                     \
              { ExceptionObject exception(__FILE__, __LINE__); \
              exception.SetDescription(s);                     \
              throw exception; }

namespace itk
{
int
PolygonGroupSpatialObjectXMLFileReader::CanReadFile(const char *name)
{
  if ( !itksys::SystemTools::FileExists(name)
       || itksys::SystemTools::FileIsDirectory(name)
       || itksys::SystemTools::FileLength(name) == 0 )
    {
    return 0;
    }
  return 1;
}

void
PolygonGroupSpatialObjectXMLFileReader::StartElement( const char *name, const char **itkNotUsed(atts) )
{
  if ( itksys::SystemTools::Strucmp(name, "POLYGONGROUP") == 0 )
    {
    m_PGroup = PGroupSpatialObjectType::New();
    }
  else if ( itksys::SystemTools::Strucmp(name, "POLYGON") == 0 )
    {
    m_CurPoly = PolygonSpatialObjectType::New();
    m_PGroup->AddSpatialObject(m_CurPoly);
    m_CurPointList.clear();
    }
}

void
PolygonGroupSpatialObjectXMLFileReader::EndElement(const char *name)
{
  itk::MetaDataDictionary & thisDic = m_PGroup->GetMetaDataDictionary();
  if ( itksys::SystemTools::Strucmp(name, "POLYGONGROUP") == 0 )
    {
    m_OutputObject = &( *m_PGroup );
    }
  if ( itksys::SystemTools::Strucmp(name, "PATIENT-ID") == 0 )
    {
    itk::EncapsulateMetaData< std::string >(thisDic, ITK_PatientID,
                                            m_CurCharacterData);
    }
  else if ( itksys::SystemTools::Strucmp(name, "SCAN-ID") == 0 )
    {
    itk::EncapsulateMetaData< std::string >(thisDic, ROI_SCAN_ID,
                                            m_CurCharacterData);
    }
  else if ( itksys::SystemTools::Strucmp(name, "DATE") == 0 )
    {
    itk::EncapsulateMetaData< std::string >(thisDic, ITK_OriginationDate,
                                            m_CurCharacterData);
    }
  else if ( itksys::SystemTools::Strucmp(name, "X-SIZE") == 0 )
    {
    int size = atoi( m_CurCharacterData.c_str() );
    itk::EncapsulateMetaData< int >(thisDic, ROI_X_SIZE, size);
    }
  else if ( itksys::SystemTools::Strucmp(name, "Y-SIZE") == 0 )
    {
    int size = atoi( m_CurCharacterData.c_str() );
    itk::EncapsulateMetaData< int >(thisDic, ROI_Y_SIZE, size);
    }
  else if ( itksys::SystemTools::Strucmp(name, "Z-SIZE") == 0 )
    {
    int size = atoi( m_CurCharacterData.c_str() );
    itk::EncapsulateMetaData< int >(thisDic, ROI_Z_SIZE, size);
    }
  else if ( itksys::SystemTools::Strucmp(name, "X-RESOLUTION") == 0 )
    {
    float res = atof( m_CurCharacterData.c_str() );
    itk::EncapsulateMetaData< float >(thisDic, ROI_X_RESOLUTION, res);
    }
  else if ( itksys::SystemTools::Strucmp(name, "Y-RESOLUTION") == 0 )
    {
    float res = atof( m_CurCharacterData.c_str() );
    itk::EncapsulateMetaData< float >(thisDic, ROI_Y_RESOLUTION, res);
    }
  else if ( itksys::SystemTools::Strucmp(name, "Z-RESOLUTION") == 0 )
    {
    float res = atof( m_CurCharacterData.c_str() );
    itk::EncapsulateMetaData< float >(thisDic, ROI_Z_RESOLUTION, res);
    }
  else if ( itksys::SystemTools::Strucmp(name, "NUM-SEGMENTS") == 0 )
    {
    int size = atoi( m_CurCharacterData.c_str() );
    itk::EncapsulateMetaData< int >(thisDic, ROI_NUM_SEGMENTS, size);
    }
  else if ( itksys::SystemTools::Strucmp(name, "POINT") == 0 )
    {
    double      pval[3];
    const char *s = m_CurCharacterData.c_str();
    char *      endptr;
    for ( int i = 0; i < 3; i++ )
      {
      pval[i] = strtod(s, &endptr);
      if ( s == endptr )
        {
        RAISE_EXCEPTION("Can't convert number");
        }
      else
        {
        s = endptr;
        }
      }
    PointType p;
    p.SetPosition(pval);
    m_CurPointList.push_back(p);
    }
  else if ( itksys::SystemTools::Strucmp(name, "POLYGON") == 0 )
    {
    m_CurPoly->SetPoints(m_CurPointList);
    }
}

void
PolygonGroupSpatialObjectXMLFileReader::CharacterDataHandler(const char *inData, int inLength)
{
  m_CurCharacterData = "";
  for ( int i = 0; i < inLength; i++ )
    {
    m_CurCharacterData = m_CurCharacterData + inData[i];
    }
}

int
PolygonGroupSpatialObjectXMLFileWriter::CanWriteFile( const char *itkNotUsed(name) )
{
  return true;                  // not sure what else to say
}

// Define ZeroValue() for std::string so that the value can be
// initialized before calling ExposeMetaData. Since ExposeMetaData can
// return without setting a value, some compilers warn about using an
// initialized variable.
template< >
class NumericTraits< std::string >
{
public:
  static const std::string ZeroValue() { return std::string(""); }
};

template< typename T >
void
WriteMetaDataAttribute(PolygonGroupSpatialObjectXMLFileWriter *This,
                       itk::MetaDataDictionary & thisDic,
                       const char *const MetaName,
                       const char *const attName,
                       std::ofstream & output)
{
  T value = NumericTraits<T>::ZeroValue();

  if ( ExposeMetaData< T >(thisDic, MetaName, value) )
    {
    This->WriteStartElement(attName, output);
    output << value;
    This->WriteEndElement(attName, output);
    output << std::endl;
    }
}

int
PolygonGroupSpatialObjectXMLFileWriter::WriteFile()
{
  //
  // sanity checks
  if ( m_InputObject == ITK_NULLPTR )
    {
    std::string errmsg("No PolygonGroup to Write");
    RAISE_EXCEPTION(errmsg);
    }
  if ( m_Filename.length() == 0 )
    {
    std::string errmsg("No filename given");
    RAISE_EXCEPTION(errmsg);
    }
  std::ofstream output( m_Filename.c_str() );
  if ( output.fail() )
    {
    std::string errmsg("Can't Open ");
    errmsg += m_Filename;
    RAISE_EXCEPTION(errmsg);
    }

  WriteStartElement("?xml version=\"1.0\"?", output);
  output << std::endl;
  WriteStartElement("!DOCTYPE POLYGONGROUP", output);
  output << std::endl;
  //
  // Write out metadata

  WriteStartElement("POLYGONGROUP", output);
  output << std::endl;

  itk::MetaDataDictionary & thisDic = m_InputObject->GetMetaDataDictionary();
  WriteMetaDataAttribute< std::string >(this, thisDic,
                                        ITK_PatientID, "PATIENT-ID", output);

  WriteMetaDataAttribute< std::string >(this, thisDic, ROI_SCAN_ID,
                                        "SCAN-ID", output);
  WriteMetaDataAttribute< std::string >(this, thisDic, ITK_OriginationDate,
                                        "DATE", output);
  WriteMetaDataAttribute< int >(this, thisDic, ROI_X_SIZE,
                                "X-SIZE", output);
  WriteMetaDataAttribute< int >(this, thisDic, ROI_Y_SIZE,
                                "Y-SIZE", output);
  WriteMetaDataAttribute< int >(this, thisDic, ROI_Z_SIZE,
                                "Z-SIZE", output);
  WriteMetaDataAttribute< float >(this, thisDic, ROI_X_RESOLUTION,
                                  "X-RESOLUTION", output);

  WriteMetaDataAttribute< float >(this, thisDic, ROI_Y_RESOLUTION,
                                  "Y-RESOLUTION", output);
  WriteMetaDataAttribute< float >(this, thisDic, ROI_Z_RESOLUTION,
                                  "Z-RESOLUTION", output);
  WriteMetaDataAttribute< int >(this, thisDic, ROI_NUM_SEGMENTS,
                                "NUM-SEGMENTS", output);

  //
  // Write out polygondata
  PolygonGroupType::ChildrenListType *children =
    m_InputObject->GetChildren(0, ITK_NULLPTR);
  PolygonGroupType::ChildrenListType::iterator it = children->begin();
  PolygonGroupType::ChildrenListType::iterator end = children->end();
  while ( it != end )
    {
    WriteStartElement("POLYGON", output);
    output << std::endl;
    PolygonSpatialObjectType *curstrand =
      dynamic_cast< PolygonSpatialObjectType * >( ( *it ).GetPointer() );
    PolygonSpatialObjectType::PointListType &         points = curstrand->GetPoints();
    PolygonSpatialObjectType::PointListType::iterator pointIt = points.begin();
    PolygonSpatialObjectType::PointListType::iterator pointItEnd = points.end();
    while ( pointIt != pointItEnd )
      {
      PolygonSpatialObjectType::PointType curpoint = ( *pointIt ).GetPosition();
      WriteStartElement("POINT", output);
      output << curpoint[0] << " " << curpoint[1] << " "  << curpoint[2];
      WriteEndElement("POINT", output);
      output << std::endl;
      ++pointIt;
      }
    WriteEndElement("POLYGON", output);
    output << std::endl;
    ++it;
    }
  WriteEndElement("POLYGONGROUP", output);
  output << std::endl;
  output.close();

  delete children;
  return 0;
}
}

#endif
