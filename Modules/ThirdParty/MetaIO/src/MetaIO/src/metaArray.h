/*============================================================================
  MetaIO
  Copyright 2000-2010 Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#include "metaTypes.h"

#ifndef ITKMetaIO_METAVECTOR_H
#define ITKMetaIO_METAVECTOR_H

#include "metaUtils.h"
#include "metaForm.h"

/*!    MetaArray (.h and .cpp)
 *
 * Description:
 *    Reads and Writes MetaArray Files.
 *    MetaArray Files can be in one of two possible formats:
 *       a combined header/data format, typically designated .mva files
 *       or as separate header and data files, typically designated
 *       .mvh and .mvd files
 *
 * Features:
 *    Header information is in ascii format - for easy creation, editing,
 *      and review.
 *    Has required and optional header data (provides rapid formation
 *      or extensive documentation).
 *    Handles byte ordering (MSB/LSB)
 *    REQUIRED: NDims, ByteOrderMSB, ElementDataType, DataFileName
 *
 * \author Stephen R. Aylward
 *
 * \date August 29, 1999
 *
 * Depends on:
 *    MetaUtils.h
 *    MetaForm.h
 */

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE {
#endif

class METAIO_EXPORT MetaArray : public MetaForm
{
  /////
  //
  // PUBLIC
  //
  ////
  public:

    ////
    //
    // Constructors & Destructor
    //
    ////
    MetaArray(void);

    MetaArray(const char *_headerName);

    MetaArray(MetaArray *_vector,
              bool _allocateElementData=false,
              bool _autoFreeElementData=false);

    MetaArray(int _length,
              MET_ValueEnumType _elementType,
              int _elementNumberOfChannels=1,
              void *_elementData=NULL,
              bool _allocateElementData=false,
              bool _autoFreeElementData=false);

    ~MetaArray(void) override;

    void  PrintInfo(void) const override;

    void  CopyInfo(const MetaForm * _form) override;

    void  Clear(void) override;

    bool  InitializeEssential(int _nDims,
                              MET_ValueEnumType _elementType,
                              int _elementNumberOfChannels=1,
                              void *_elementData=NULL,
                              bool _allocateElementData=false,
                              bool _autoFreeElementData=true);

    bool  AllocateElementData(bool _autoFreeElementData=true);

    int   Length(void) const;
    void  Length(int _length);

    int   NDims(void) const;
    void  NDims(int _length);

    MET_ValueEnumType ElementType(void) const;
    void              ElementType(MET_ValueEnumType _elementType);

    int   ElementNumberOfChannels(void) const;
    void  ElementNumberOfChannels(int _elementNumberOfChannels);

    //
    //
    //
    void  ElementByteOrderSwap(void);
    bool  ElementByteOrderFix(void);

    //
    //    ConverTo(...)
    //       Converts to a new data type
    bool  ConvertElementDataTo(MET_ValueEnumType _toElementType,
                               double _fromMin=0,
                               double _fromMax=0,
                               double _toMin=0,
                               double _toMax=0);

    bool  ImportBufferToElementData(const void *_fromBuffer,
                                    MET_ValueEnumType _fromBufferType,
                                    double _fromMin=0,
                                    double _fromMax=0,
                                    double _toMin=0,
                                    double _toMax=0);

    //
    //
    //
    bool         AutoFreeElementData(void) const;
    void         AutoFreeElementData(bool _freeData);

    const char * ElementDataFileName(void) const;
    void         ElementDataFileName(const char * _dataFileName);

    void *       ElementData(void);
    double       ElementData(int _i) const;
    void         ElementData(void * _data, bool _autoFreeElementData=false);
    bool         ElementData(int _i, double _v);

    //
    //
    //
    virtual bool CanRead(const char *_headerName=NULL) const;

    virtual bool Read(const char *_headerName=NULL,
                      bool _readElements=true,
                      void * _elementDataBuffer=NULL,
                      bool _autoFreeElementData=false);

    virtual bool CanReadStream(std::ifstream * _stream) const;

    virtual bool ReadStream(std::ifstream * _stream,
                            bool _readElements=true,
                            void * _elementDataBuffer=NULL,
                            bool _autoFreeElementData=false);

    virtual bool Write(const char *_headName=NULL,
                       const char *_dataName=NULL,
                       bool _writeElements=true,
                       const void * _constElementData=NULL);

    virtual bool WriteStream(std::ofstream * _stream,
                             bool _writeElements=true,
                             const void * _constElementData=NULL);

  ////
  //
  // PROTECTED
  //
  ////
  protected:

    int                m_Length;

    MET_ValueEnumType  m_ElementType;

    int                m_ElementNumberOfChannels;

    bool               m_AutoFreeElementData;

    std::streamoff m_CompressedElementDataSize;

    std::string        m_ElementDataFileName;

    void *             m_ElementData;

    void  M_Destroy(void) override;

    void  M_SetupReadFields(void) override;

    void  M_SetupWriteFields(void) override;

    bool  M_Read(void) override;

    bool  M_ReadElements(std::ifstream * _fstream,
                         void * _data,
                         int _dataQuantity);

    bool  M_WriteElements(std::ofstream * _fstream,
                          const void * _data,
                          std::streamoff _dataQuantity);

    };

#if (METAIO_USE_NAMESPACE)
};
#endif

#endif
