#ifndef METAOBJECT_H
#define METAOBJECT_H

#include <iostream>
#include <fstream>

#include <metaUtils.h>

extern int META_DEBUG;


class MetaObject
  {
  ////
  //
  // PROTECTED
  //
  ////
  protected:

      std::ifstream* m_ReadStream;
      std::ofstream* m_WriteStream;

      typedef std::vector<MET_FieldRecordType *> FieldsContainerType;

      FieldsContainerType m_Fields;

      char  m_FileName[255];

      char  m_Comment[255];         // "Comment = "          ""

      char  m_ObjectTypeName[255];    // "ObjectType = "     defined by suffix
      char  m_ObjectSubTypeName[255]; // "ObjectSubType = "  defined by suffix

      int   m_NDims;                // "NDims = "            required

      float m_Offset[10];         // "Offset = "         0,0,0

      float m_Rotation[100];     // "Rotation = "      1,0,0, 0,1,0, 0,0,1

      MET_OrientationEnumType m_AnatomicalOrientation[10];

      float m_ElementSpacing[10];   // "ElementSpacing = "   0,0,0

      float m_Color[4];             // "Color = "            1.0, 0.0, 0.0, 1.0
 
      int   m_ID;          // "ID = "               0

      int   m_ParentID;             // "ParentID = "         -1

      char  m_Name[255];            // "Name = "             ""

      bool  m_BinaryData;           // "BinaryData = "      False

      bool  m_BinaryDataByteOrderMSB;
      
      virtual void M_Destroy(void);

      virtual void M_SetupReadFields(void);

      virtual void M_SetupWriteFields(void);

      virtual bool M_Read(void);

      virtual bool M_Write(void);

      //MET_FieldRecordType * M_GetFieldRecord(const char * _fieldName);
      //int   M_GetFieldRecordNumber(const char * _fieldName);

  /////
  //
  // PUBLIC
  //
  ////
  public:

      ////
      // Constructors & Destructor
      ////
      MetaObject(void);
      MetaObject(const char * _fileName);
      MetaObject(unsigned int dim);

      virtual ~MetaObject(void);

      void  FileName(const char *_fileName);
      const char  * FileName(void) const;

      void  CopyInfo(const MetaObject * _object);

      bool  Read(const char * _fileName=NULL);

      bool  ReadStream(int _nDims, std::ifstream * _stream);

      bool  Write(const char * _fileName=NULL);

      virtual bool Append(const char *_headName=NULL);

      ////
      //
      // Common fields
      //
      ////

      //    PrintMetaInfo()
      //       Writes image parameters to stdout
      virtual void  PrintInfo(void) const;

      //    Comment(...)
      //       Optional Field
      //       Arbitrary string
      const char  * Comment(void) const;
      void    Comment(const char * _comment);

      const char  * ObjectTypeName(void) const;
      void    ObjectTypeName(const char * _objectTypeName);
      const char  * ObjectSubTypeName(void) const;
      void    ObjectSubTypeName(const char * _objectSubTypeName);

      //    NDims()
      //       REQUIRED Field
      //       Number of dimensions to the image
      int   NDims(void) const;

      //    Offset(...)
      //       Optional Field
      //       Physical location (in millimeters and wrt machine coordinate
      //         system or the patient) of the first element in the image
      const float * Offset(void) const;
      float Offset(int _i) const;
      void  Offset(const float * _position);
      void  Offset(int _i, float _value);
      const float * Position(void) const;
      float Position(int _i) const;
      void  Position(const float * _position);
      void  Position(int _i, float _value);

      //    Rotation(...)
      //       Optional Field
      //       Physical orientation of the object as an NDims x NDims matrix
      const float * Rotation(void) const;
      float Rotation(int _i, int _j) const;
      void  Rotation(const float * _orientation);
      void  Rotation(int _i, int _j, float _value);
      const float * Orientation(void) const;
      float Orientation(int _i, int _j) const;
      void  Orientation(const float * _orientation);
      void  Orientation(int _i, int _j, float _value);

      const char * AnatomicalOrientationAcronym(void) const;
      const MET_OrientationEnumType * AnatomicalOrientation(void) const;
      MET_OrientationEnumType AnatomicalOrientation(int _dim) const;
      void AnatomicalOrientation(const char *_ao);
      void AnatomicalOrientation(const MET_OrientationEnumType *_ao);
      void AnatomicalOrientation(int _dim, MET_OrientationEnumType _ao);
      void AnatomicalOrientation(int _dim, char ao);

      
      //    ElementSpacing(...)
      //       Optional Field
      //       Physical Spacing (in same units as position)
      const float * ElementSpacing(void) const;
      float ElementSpacing(int _i) const;
      void  ElementSpacing(const float * _elementSpacing);
      void  ElementSpacing(int _i, float _value);

      //    Name(...)
      //       Optional Field
      //       Name of the current metaObject
      void  Name(const char *_Name);
      const char  * Name(void) const;

      //    Color(...)
      //       Optional Field
      //       Color of the current metaObject   
      const float * Color(void) const;
      void  Color(float _r, float _g, float _b, float _a);
      void  Color(const float * _color);
 
      //    ID(...)
      //       Optional Field
      //       ID number of the current metaObject
      void ID(int _id);
      int  ID(void) const;

      //    ParentID(...)
      //       Optional Field
      //       ID number of the parent  metaObject
      void  ParentID(int _parentId);
      int   ParentID(void) const;

      //    BinaryData(...)
      //       Optional Field
      //       Data is binary or not
      void  BinaryData(bool _binaryData);
      bool  BinaryData(void) const;

      void  BinaryDataByteOrderMSB(bool _binaryDataByteOrderMSB);
      bool  BinaryDataByteOrderMSB(void) const;

      virtual void Clear(void);

      void ClearFields(void);

      bool InitializeEssential(int m_NDims);

  };


#endif
