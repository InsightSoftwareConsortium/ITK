#ifndef METAIMAGE_H
#define METAIMAGE_H

#include "metaTypes.h"
#include "metaUtils.h"
#include "metaObject.h"

#include "metaImageTypes.h"
#include "metaImageUtils.h"

/*!    MetaImage (.h and .cpp)
 *
 * Description:
 *    Reads and Writes MetaImageFiles.
 *    MetaImageFiles can be in one of two possible formats:
 *       a combined header/data format, typically designated .mha files
 *       or as separate header and data files, typically designated
 *       .mhd and .mda files
 *
 * Features:
 *    Header information is in ascii format - for easy creation, editing,
 *    and review. If separate files are used, a specified number of
 *    header-bytes in the datafile can be skipped
 *       - in this manner, different file formats (e.g., bmp, img,
 *          and /usr/Image) can be converted to metaImageFile format by
 *          merely specifying a separate text header (.mhd) file and in that
 *          file specifying how many data-header-bytes should be skipped.
 *          Thereby the data files can serve a dual role (as bitmap files
 *          and as metaImage data files) 
 *    Supports all pixel formats
 *    Handles byte ordering (MSB/LSB)
 *    Provides the ability to handle associated medical image
 *          information (element size, element spacing, etc).
 *    Has required and optional header data (provides rapid formation
 *          or extensive documentation).
 *    REQUIRED: NDims, DimSize, ByteOrderMSB, ElementDataType, DataFileName
 *
 * \author Stephen R. Aylward
 * 
 * \date August 29, 1999
 * 
 * Depends on:
 *    MetaUtils.h
 *    MetaFileLib.h
 */
class MetaImage : public MetaObject
  {
  ////
  //
  // PROTECTED
  //
  ////
  protected:

    char  m_ElementDataFileName[255];

    int   m_DimSize[10];
    int   m_Quantity;
    int   m_SubQuantity[10];

    MET_ImageModalityEnumType m_Modality;

    int    m_HeaderSize;

    float m_SequenceID[4];

    bool  m_ElementSizeValid;
    float m_ElementSize[10];

    MET_ValueEnumType m_ElementType;
    int    m_ElementNumberOfChannels;

    bool   m_ElementMinMaxValid;
    double m_ElementMin;
    double m_ElementMax;

    double m_ElementToIntensityFunctionSlope;
    double m_ElementToIntensityFunctionOffset;

    bool  m_AutoFreeElementData;
    void  * m_ElementData;

    void  M_Destroy(void);

    void  M_SetupReadFields(void);

    void  M_SetupWriteFields(void);

    bool  M_ReadElements(std::ifstream * _fstream, void * _data,
                         int _dataQuantity);

    bool  M_Read(void);

    unsigned char * m_CompressedElementData;
    unsigned char * PerformCompression(unsigned char *source,int quantity);

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
    MetaImage(void);

    MetaImage(const char *_headerName);   

    MetaImage(MetaImage *_im);    // share memory

    MetaImage(int _nDims, 
              const int * _dimSize,
              const float *_elementSpacing,
              MET_ValueEnumType _elementType,
              int _elementNumberOfChannels=1,
              void *_elementData=NULL);

    MetaImage(int _x, int _y, 
              float _elementSpacingX, 
              float _elementSpacingY,
              MET_ValueEnumType _elementType, 
              int _elementNumberOfChannels=1,
              void *_elementData=NULL);

    MetaImage(int _x, int _y, int _z, 
              float _elementSpacingX,
              float _elementSpacingY,
              float _elementSpacingZ, 
              MET_ValueEnumType _elementType,
              int _elementNumberOfChannels=1,
              void *_elementData=NULL);

    ~MetaImage(void);

    void PrintInfo(void) const;

    void CopyInfo(const MetaImage * _im);

    int  HeaderSize(void) const;
    void HeaderSize(int _headerSize);

    ////
    //
    // Utility Code
    //
    ////

    MET_ImageModalityEnumType  Modality(void) const;
    void Modality(MET_ImageModalityEnumType _modality);

    //    DimSize(...)
    //       REQUIRED Field
    //       Number of elements along each dimension
    const int * DimSize(void) const;
    int   DimSize(int _i) const;      
    //void  DimSize(const int * _dimSize);
    //void  DimSize(int _i, int _value);

    //    Quantity()
    //       Not a field in file
    //       Total number of elements in image (Prod(dimSize[i]))
    int   Quantity(void) const;

    //    SubQuantity(...)
    //       Not a field in file
    //       Number of elements in image spanning sub-dimensions
    //       E.g., elements per line, 2D sub-image, 3D sub-volume,
    const int * SubQuantity(void) const;      
    int   SubQuantity(int _i) const;  

    //    SequenceID(...)
    //       Optional Field
    //       DICOM designation of this image relative to other images
    //         acquired at the same time
    const float * SequenceID(void) const;
    float SequenceID(int _i) const;
    void  SequenceID(const float * _sequenceID);
    void  SequenceID(int _i, float _value);

    //    ElemSize(...)
    //       Optional Field
    //       Physical size (in MM) of each element in the image
    //       (0 = xSize, 1 = ySize, 2 = zSize)
    void ElementSizeValid(bool _elementSizeValid);
    bool ElementSizeValid(void) const;
    const float * ElementSize(void) const;
    float ElementSize(int i) const;
    void  ElementSize(const float * _pointSize);
    void  ElementSize(int _i, float _value);

    MET_ValueEnumType ElementType(void) const;
    void  ElementType(MET_ValueEnumType _elementType);

    int   ElementNumberOfChannels(void) const;
    void  ElementNumberOfChannels(int _elementNumberOfChannels);

    //    ElemMakeByteOrderMSB(), ElemMakeByteOrderLSB(),
    //    ElemByteOrderSwap(), ElemByteOrderFix()
    //       The following functions are available only after
    //       ReadImageData() or if _read_and_close=TRUE when read
    void  ElementByteOrderSwap(void);
    bool  ElementByteOrderFix(void);

    //    Min(...) Max(...)
    //       The default max returned is the largest allowed by
    //         ElemNBytes (12 bit uint16_t will give 4096 max).
    //       This may not represent the true max.   Use _reCalc=true
    //         to force a calcuation of the actual max element value.
    bool  ElementMinMaxValid(void) const;
    void  ElementMinMaxValid(bool _elementMinMaxValid);
    void  ElementMinMaxRecalc(void);
    double ElementMin(void) const;    
    void  ElementMin(double _elementMin);
    double ElementMax(void) const;
    void  ElementMax(double _elementMax);

    double ElementToIntensityFunctionSlope(void) const;
    void   ElementToIntensityFunctionSlope(double _slope);
    double ElementToIntensityFunctionOffset(void) const;
    void   ElementToIntensityFunctionOffset(double _offset);

    //    ConverTo(...)
    //       Converts to a new data type
    //       Rescales using Min and Max (see above)
    bool  ConvertElementDataTo(MET_ValueEnumType _elementType=MET_USHORT,
                               double _toMin=0, double _toMax=0);
    bool  ConvertElementDataToIntensityData(
                               MET_ValueEnumType _intensityType=MET_SHORT);
    bool  ConvertIntensityDataToElementData(
                               MET_ValueEnumType _elementType=MET_USHORT);

    //
    //
    //
    void * ElementData(void);
    double ElementData(int _i) const;
    void  ElementData(void * _data);
    bool  ElementData(int _i, double _v);

    bool  AutoFreeElementData(void) const;
    void  AutoFreeElementData(bool _freeData);

    //
    //
    //
    const char * ElementDataFileName(void) const;
    void ElementDataFileName(const char * _dataFileName);

    //
    //
    //
    virtual bool Read(const char *_headerName=NULL, bool _readElements=true,
                      void * _buffer=NULL);

    virtual bool Write(const char *_headName=NULL, const char *_dataName=NULL,
                       bool _writeElements=true);

    virtual bool Append(const char *_headName=NULL);

    bool ReadStream(int _nDims, std::ifstream * _stream);

    void Clear(void);

    bool InitializeEssential(int _nDims, 
                                     const int * _dimSize,
                                     const float * _elementSpacing,
                                     MET_ValueEnumType _elementType,
                                     const int _elementNumberOfChannels=1,
                                     void *_elementData=NULL,
                                     bool _allocElementMemory=true);

  };


#endif
