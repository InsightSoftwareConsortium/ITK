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

#include "itkNrrdImageIO.h"
#include "NrrdIO.h"

#include "itkMetaDataObject.h"
#include "itkIOCommon.h"
#include "itkFloatingPointExceptions.h"

namespace itk
{
#define KEY_PREFIX "NRRD_"

NrrdImageIO::NrrdImageIO()
{
  this->SetNumberOfDimensions(3);
  this->AddSupportedWriteExtension(".nrrd");
  this->AddSupportedReadExtension(".nrrd");
  this->AddSupportedWriteExtension(".nhdr");
  this->AddSupportedReadExtension(".nhdr");
}

NrrdImageIO::~NrrdImageIO()
{
}

bool NrrdImageIO::SupportsDimension(unsigned long dim)
{
  if ( 1 == this->GetNumberOfComponents() )
    {
    return dim <= NRRD_DIM_MAX;
    }
  else
    {
    return dim <= NRRD_DIM_MAX - 1;
    }
}

void NrrdImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

ImageIOBase::IOComponentType
NrrdImageIO::NrrdToITKComponentType(const int nrrdComponentType) const
{
  switch ( nrrdComponentType )
    {
    case nrrdTypeUnknown:
    case nrrdTypeBlock:
      return UNKNOWNCOMPONENTTYPE;

    case nrrdTypeChar:
      return CHAR;

    case nrrdTypeUChar:
      return UCHAR;

    case nrrdTypeShort:
      return SHORT;

    case nrrdTypeUShort:
      return USHORT;

    case nrrdTypeInt:
      return INT;

    case nrrdTypeUInt:
      return UINT;

    case nrrdTypeLLong:
      return LONGLONG;

    case nrrdTypeULLong:
      return ULONGLONG;

    case nrrdTypeFloat:
      return FLOAT;

    case nrrdTypeDouble:
      return DOUBLE;
    }
  // Strictly to avoid compiler warning regarding "control may reach end of
  // non-void function":
  return UNKNOWNCOMPONENTTYPE;
}

int
NrrdImageIO::ITKToNrrdComponentType(const ImageIOBase::IOComponentType itkComponentType) const
{
  switch ( itkComponentType )
    {
    case UNKNOWNCOMPONENTTYPE:
      return nrrdTypeUnknown;

    case CHAR:
      return nrrdTypeChar;

    case UCHAR:
      return nrrdTypeUChar;

    case SHORT:
      return nrrdTypeShort;

    case USHORT:
      return nrrdTypeUShort;

    // "long" is a silly type because it basically guaranteed not to be
    // cross-platform across 32-vs-64 bit machines, but we can figure out
    // a cross-platform way of storing the information.
    case LONG:
      return (4 == sizeof(long) ) ? nrrdTypeInt : nrrdTypeLLong;

    case ULONG:
      return (4 == sizeof(long) ) ? nrrdTypeUInt : nrrdTypeULLong;

    case INT:
      return nrrdTypeInt;

    case UINT:
      return nrrdTypeUInt;

    case LONGLONG:
      return nrrdTypeLLong;

    case ULONGLONG:
      return nrrdTypeULLong;

    case FLOAT:
      return nrrdTypeFloat;

    case DOUBLE:
      return nrrdTypeDouble;
    }
  // Strictly to avoid compiler warning regarding "control may reach end of
  // non-void function":
  return nrrdTypeUnknown;
}

bool NrrdImageIO::CanReadFile(const char *filename)
{
  // Check the extension first to avoid opening files that do not
  // look like nrrds.  The file must have an appropriate extension to be
  // recognized.
  std::string fname = filename;

  bool                   extensionFound = false;
  std::string::size_type nrrdPos = fname.rfind(".nrrd");
  if ( ( nrrdPos != std::string::npos )
       && ( nrrdPos == fname.length() - 5 ) )
    {
    extensionFound = true;
    }

  std::string::size_type nhdrPos = fname.rfind(".nhdr");
  if ( ( nhdrPos != std::string::npos )
       && ( nhdrPos == fname.length() - 5 ) )
    {
    extensionFound = true;
    }

  if ( !extensionFound )
    {
    itkDebugMacro(<< "The filename extension is not recognized");
    return false;
    }

  // We have the correct extension, so now check for the Nrrd magic "NRRD",
  // while ignoring the format version (the next four characters)
  std::ifstream inputStream;
  try
    {
    this->OpenFileForReading( inputStream, fname );
    }
  catch( ExceptionObject & )
    {
    return false;
    }

  char magic[5] = { '\0', '\0', '\0', '\0', '\0' };
  inputStream.read( magic, 4 * sizeof( char ) );

  if ( inputStream.eof() )
    {
    inputStream.close();
    return false;
    }

  if ( strcmp(magic, "NRRD") == 0 )
    {
    inputStream.close();
    return true;
    }

  inputStream.close();
  return false;
}

void NrrdImageIO::ReadImageInformation()
{
  // This method determines the following and sets the appropriate value in
  // the parent IO class:
  //
  // binary/ascii file type
  // endianness
  // pixel type
  // pixel component type
  // number of pixel components
  // number of image dimensions
  // image spacing
  // image origin
  // meta data dictionary information

  Nrrd *       nrrd = nrrdNew();
  NrrdIoState *nio = nrrdIoStateNew();

  try
    {
#if !defined(__MINGW32__) && (defined(ITK_HAS_FEENABLEEXCEPT) || defined(_MSC_VER))
    // nrrd causes exceptions on purpose, so mask them
    bool saveFPEState(FloatingPointExceptions::GetExceptionAction() );
    FloatingPointExceptions::Disable();
#endif

    // this is the mechanism by which we tell nrrdLoad to read
    // just the header, and none of the data
    nrrdIoStateSet(nio, nrrdIoStateSkipData, 1);
    if ( nrrdLoad(nrrd, this->GetFileName(), nio) != 0 )
      {
      char *err = biffGetDone(NRRD);

      // don't use macro so that we can free err
      std::ostringstream message;
      message << "itk::ERROR: " << this->GetNameOfClass()
              << "(" << this << "): " << "ReadImageInformation: Error reading "  << this->GetFileName() << ":\n" << err;
      ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(), ITK_LOCATION);
      free( err );
      throw e_;
      }

#if !defined(__MINGW32__) && (defined(ITK_HAS_FEENABLEEXCEPT) || defined(_MSC_VER))
    // restore state
    FloatingPointExceptions::SetEnabled(saveFPEState);
#endif


    if ( nrrdTypeBlock == nrrd->type )
      {
      itkExceptionMacro("ReadImageInformation: Cannot currently "
                        "handle nrrdTypeBlock");
      }
    if ( nio->endian == airEndianLittle )
      {
      this->SetByteOrderToLittleEndian();
      }
    else if ( nio->endian == airEndianBig )
      {
      this->SetByteOrderToBigEndian();
      }
    else
      {
      this->SetByteOrder(ImageIOBase::OrderNotApplicable);
      }

    if ( nio->encoding == nrrdEncodingAscii )
      {
      this->SetFileTypeToASCII();
      }
    else
      {
      this->SetFileTypeToBinary();
      }
    // set type of pixel components; this is orthogonal to pixel type

    ImageIOBase::IOComponentType
      cmpType = this->NrrdToITKComponentType(nrrd->type);
    if ( UNKNOWNCOMPONENTTYPE == cmpType )
      {
      itkExceptionMacro("Nrrd type " << airEnumStr(nrrdType, nrrd->type)
                                     << " could not be mapped to an ITK component type");
      }
    this->SetComponentType(cmpType);

    // Set the number of image dimensions and bail if needed
    unsigned int domainAxisNum, domainAxisIdx[NRRD_DIM_MAX],
      rangeAxisNum, rangeAxisIdx[NRRD_DIM_MAX];
    domainAxisNum = nrrdDomainAxesGet(nrrd, domainAxisIdx);
    rangeAxisNum = nrrdRangeAxesGet(nrrd, rangeAxisIdx);
    if ( nrrd->spaceDim && nrrd->spaceDim != domainAxisNum )
      {
      itkExceptionMacro("ReadImageInformation: nrrd's #independent axes ("
                        << domainAxisNum << ") doesn't match dimension of space"
                        " in which orientation is defined ("
                        << nrrd->spaceDim << "); not currently handled");
      }
    // else nrrd->spaceDim == domainAxisNum when nrrd has orientation

    if ( 0 == rangeAxisNum )
      {
      // we don't have any non-scalar data
      this->SetNumberOfDimensions(nrrd->dim);
      this->SetPixelType(ImageIOBase::SCALAR);
      this->SetNumberOfComponents(1);
      }
    else if ( 1 == rangeAxisNum )
      {
      this->SetNumberOfDimensions(nrrd->dim - 1);
      int kind = nrrd->axis[rangeAxisIdx[0]].kind;
      size_t size = nrrd->axis[rangeAxisIdx[0]].size;
      // NOTE: it is the NRRD readers responsibility to make sure that
      // the size (#of components) associated with a specific kind is
      // matches the actual size of the axis.
      switch ( kind )
        {
        case nrrdKindDomain:
        case nrrdKindSpace:
        case nrrdKindTime:
          itkExceptionMacro("ReadImageInformation: range axis kind ("
                            << airEnumStr(nrrdKind, kind) << ") seems more "
                            "like a domain axis than a range axis");

        case nrrdKindStub:
        case nrrdKindScalar:
          this->SetPixelType(ImageIOBase::SCALAR);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKind3Color:
        case nrrdKindRGBColor:
          this->SetPixelType(ImageIOBase::RGB);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKind4Color:
        case nrrdKindRGBAColor:
          this->SetPixelType(ImageIOBase::RGBA);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKindVector:
        case nrrdKind2Vector:
        case nrrdKind3Vector:
        case nrrdKind4Vector:
        case nrrdKindList:
          this->SetPixelType(ImageIOBase::VECTOR);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKindPoint:
          this->SetPixelType(ImageIOBase::POINT);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKindCovariantVector:
        case nrrdKind3Gradient:
        case nrrdKindNormal:
        case nrrdKind3Normal:
          this->SetPixelType(ImageIOBase::COVARIANTVECTOR);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKind3DSymMatrix:
          // ImageIOBase::DIFFUSIONTENSOR3D is a subclass
          this->SetPixelType(ImageIOBase::SYMMETRICSECONDRANKTENSOR);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKind3DMaskedSymMatrix:
          this->SetPixelType(ImageIOBase::SYMMETRICSECONDRANKTENSOR);
          // NOTE: we will crop out the mask in Read() below; this is the
          // one case where NumberOfComponents != size
          this->SetNumberOfComponents(size - 1);
          break;
        case nrrdKindComplex:
          this->SetPixelType(ImageIOBase::COMPLEX);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKindHSVColor:
        case nrrdKindXYZColor:
        case nrrdKindQuaternion:
        case nrrdKind2DSymMatrix:
        case nrrdKind2DMaskedSymMatrix:
        case nrrdKind2DMatrix:
        case nrrdKind2DMaskedMatrix:
        case nrrdKind3DMatrix:
          // for all other Nrrd kinds, we punt and call it a vector
          this->SetPixelType(ImageIOBase::VECTOR);
          this->SetNumberOfComponents(size);
          break;
        default:
          itkExceptionMacro("ReadImageInformation: nrrdKind " << kind
                                                              << " not known!");
        }
      }
    else
      {
      itkExceptionMacro("ReadImageInformation: nrrd has "
                        << rangeAxisNum
                        << " dependent axis (not 1); not currently handled");
      }

    double                spacing;
    double                spaceDir[NRRD_SPACE_DIM_MAX];
    std::vector< double > spaceDirStd(domainAxisNum);
    int                   spacingStatus;

    int iFlipFactors[3];  // used to flip the measurement frame later on
    for ( unsigned int iI = 0; iI < 3; iI++ )
      {
      iFlipFactors[iI] = 1;
      }

    for ( unsigned int axii = 0; axii < domainAxisNum; axii++ )
      {
      unsigned int naxi = domainAxisIdx[axii];
      this->SetDimensions(axii, static_cast<unsigned int>( nrrd->axis[naxi].size ) );
      spacingStatus = nrrdSpacingCalculate(nrrd, naxi, &spacing, spaceDir);

      switch ( spacingStatus )
        {
        case nrrdSpacingStatusNone:
          // Let ITK's defaults stay
          // this->SetSpacing(axii, 1.0);
          break;
        case nrrdSpacingStatusScalarNoSpace:
          this->SetSpacing(axii, spacing);
          break;
        case nrrdSpacingStatusDirection:
          if ( AIR_EXISTS(spacing) )
            {
            // only set info if we have something to set
            switch ( nrrd->space )
              {
              // on read, convert non-LPS coords into LPS coords, when we can
              case nrrdSpaceRightAnteriorSuperior:
                spaceDir[0] *= -1;   // R -> L
                spaceDir[1] *= -1;   // A -> P
                iFlipFactors[0] = -1;
                iFlipFactors[1] = -1;
                break;
              case nrrdSpaceLeftAnteriorSuperior:
                spaceDir[0] *= -1;   // R -> L
                iFlipFactors[0] = -1;
                break;
              case nrrdSpaceLeftPosteriorSuperior:
                // no change needed
                break;
              default:
                // we're not coming from a space for which the conversion
                // to LPS is well-defined
                break;
              }
            this->SetSpacing(axii, spacing);

            for ( unsigned int saxi = 0; saxi < nrrd->spaceDim; saxi++ )
              {
              spaceDirStd[saxi] = spaceDir[saxi];
              }
            this->SetDirection(axii, spaceDirStd);
            }
          break;
        default:
        case nrrdSpacingStatusUnknown:
          itkExceptionMacro("ReadImageInformation: Error interpreting "
                            "nrrd spacing (nrrdSpacingStatusUnknown)");
        case nrrdSpacingStatusScalarWithSpace:
          itkExceptionMacro("ReadImageInformation: Error interpreting "
                            "nrrd spacing (nrrdSpacingStatusScalarWithSpace)");
        }
      }

    // Figure out origin
    if ( nrrd->spaceDim )
      {
      if ( AIR_EXISTS(nrrd->spaceOrigin[0]) )
        {
        // only set info if we have something to set
        double spaceOrigin[NRRD_SPACE_DIM_MAX];
        for ( unsigned int saxi = 0; saxi < nrrd->spaceDim; saxi++ )
          {
          spaceOrigin[saxi] = nrrd->spaceOrigin[saxi];
          }
        switch ( nrrd->space )
          {
          // convert non-LPS coords into LPS coords, when we can
          case nrrdSpaceRightAnteriorSuperior:
            spaceOrigin[0] *= -1;   // R -> L
            spaceOrigin[1] *= -1;   // A -> P
            break;
          case nrrdSpaceLeftAnteriorSuperior:
            spaceOrigin[0] *= -1;   // R -> L
            break;
          case nrrdSpaceLeftPosteriorSuperior:
            // no change needed
            break;
          default:
            // we're not coming from a space for which the conversion
            // to LPS is well-defined
            break;
          }
        for ( unsigned int saxi = 0; saxi < nrrd->spaceDim; saxi++ )
          {
          this->SetOrigin(saxi, spaceOrigin[saxi]);
          }
        }
      }
    else
      {
      double spaceOrigin[NRRD_DIM_MAX];
      int    originStatus = nrrdOriginCalculate(nrrd, domainAxisIdx, domainAxisNum,
                                                nrrdCenterCell, spaceOrigin);
      for ( unsigned int saxi = 0; saxi < domainAxisNum; saxi++ )
        {
        switch ( originStatus )
          {
          case nrrdOriginStatusNoMin:
          case nrrdOriginStatusNoMaxOrSpacing:
            // only set info if we have something to set
            // this->SetOrigin(saxi, 0.0);
            break;
          case nrrdOriginStatusOkay:
            this->SetOrigin(saxi, spaceOrigin[saxi]);
            break;
          default:
          case nrrdOriginStatusUnknown:
          case nrrdOriginStatusDirection:
            itkExceptionMacro("ReadImageInformation: Error interpreting "
                              "nrrd origin status");
          }
        }
      }

    // Store key/value pairs in MetaDataDictionary
    char                 key[AIR_STRLEN_SMALL];
    const char *         val;
    char *               keyPtr = ITK_NULLPTR;
    char *               valPtr = ITK_NULLPTR;
    MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
    // Necessary to clear dict if ImageIO object is re-used
    thisDic.Clear();
    std::string          classname( this->GetNameOfClass() );
    EncapsulateMetaData< std::string >(thisDic, ITK_InputFilterName, classname);
    for ( unsigned int kvpi = 0; kvpi < nrrdKeyValueSize(nrrd); kvpi++ )
      {
      nrrdKeyValueIndex(nrrd, &keyPtr, &valPtr, kvpi);
      EncapsulateMetaData< std::string >( thisDic, std::string(keyPtr),
                                          std::string(valPtr) );
      keyPtr = (char *)airFree(keyPtr);
      valPtr = (char *)airFree(valPtr);
      }

    // save in MetaDataDictionary those important nrrd fields that
    // (currently) have no ITK equivalent. NOTE that for the per-axis
    // information, we use the same axis index (axii) as in ITK, NOT
    // the original axis index in nrrd (axi).  This is because in the
    // Read() method, non-scalar data is permuted to the fastest axis,
    // on the on the Write() side, its always written to the fastest axis,
    // so we might was well go with consistent and idiomatic indexing.
    NrrdAxisInfo *naxis;
    for ( unsigned int axii = 0; axii < domainAxisNum; axii++ )
      {
      unsigned int axi = domainAxisIdx[axii];
      naxis = nrrd->axis + axi;
      if ( AIR_EXISTS(naxis->thickness) )
        {
        sprintf(key, "%s%s[%u]", KEY_PREFIX,
                airEnumStr(nrrdField, nrrdField_thicknesses), axii);
        EncapsulateMetaData< double >(thisDic, std::string(key),
                                      naxis->thickness);
        }
      if ( naxis->center )
        {
        sprintf(key, "%s%s[%u]", KEY_PREFIX,
                airEnumStr(nrrdField, nrrdField_centers), axii);
        val = airEnumStr(nrrdCenter, naxis->center);
        EncapsulateMetaData< std::string >( thisDic, std::string(key),
                                            std::string(val) );
        }
      if ( naxis->kind )
        {
        sprintf(key, "%s%s[%u]", KEY_PREFIX,
                airEnumStr(nrrdField, nrrdField_kinds), axii);
        val = airEnumStr(nrrdKind, naxis->kind);
        EncapsulateMetaData< std::string >( thisDic, std::string(key),
                                            std::string(val) );
        }
      if ( airStrlen(naxis->label) )
        {
        sprintf(key, "%s%s[%u]", KEY_PREFIX,
                airEnumStr(nrrdField, nrrdField_labels), axii);
        EncapsulateMetaData< std::string >( thisDic, std::string(key),
                                            std::string(naxis->label) );
        }
      }
    if ( airStrlen(nrrd->content) )
      {
      sprintf( key, "%s%s", KEY_PREFIX,
               airEnumStr(nrrdField, nrrdField_content) );
      EncapsulateMetaData< std::string >( thisDic, std::string(key),
                                          std::string(nrrd->content) );
      }
    if ( AIR_EXISTS(nrrd->oldMin) )
      {
      sprintf( key, "%s%s", KEY_PREFIX,
               airEnumStr(nrrdField, nrrdField_old_min) );
      EncapsulateMetaData< double >(thisDic, std::string(key), nrrd->oldMin);
      }
    if ( AIR_EXISTS(nrrd->oldMax) )
      {
      sprintf( key, "%s%s", KEY_PREFIX,
               airEnumStr(nrrdField, nrrdField_old_max) );
      EncapsulateMetaData< double >(thisDic, std::string(key), nrrd->oldMax);
      }
    if ( nrrd->space )
      {
      sprintf( key, "%s%s", KEY_PREFIX,
               airEnumStr(nrrdField, nrrdField_space) );
      val = airEnumStr(nrrdSpace, nrrd->space);

      // keep everything consistent: so enter it as LPS in the meta data
      // dictionary in case it could get converted, otherwise leave it
      // as is

      switch ( nrrd->space )
        {
        case nrrdSpaceRightAnteriorSuperior:
        case nrrdSpaceLeftAnteriorSuperior:
        case nrrdSpaceLeftPosteriorSuperior:
          // in all these cases we could convert
          EncapsulateMetaData< std::string >( thisDic, std::string(key),
                                              std::string( airEnumStr(nrrdSpace, nrrdSpaceLeftPosteriorSuperior) ) );
          break;
        default:
          // we're not coming from a space for which the conversion
          // to LPS is well-defined
          EncapsulateMetaData< std::string >( thisDic, std::string(key),
                                              std::string(val) );
          break;
        }
      }

    if ( AIR_EXISTS(nrrd->measurementFrame[0][0]) )
      {
      sprintf( key, "%s%s", KEY_PREFIX,
               airEnumStr(nrrdField, nrrdField_measurement_frame) );
      std::vector< std::vector< double > > msrFrame(domainAxisNum);

      // flip the measurement frame here if we have to
      // so that everything is consistent with the ITK LPS space directions
      // but only do this if we have a three dimensional space or smaller

      for ( unsigned int saxi = 0; saxi < domainAxisNum; saxi++ )
        {
        msrFrame[saxi].resize(domainAxisNum);
        for ( unsigned int saxj = 0; saxj < domainAxisNum; saxj++ )
          {
          if ( domainAxisNum <= 3 )
            {
            msrFrame[saxi][saxj] = iFlipFactors[saxj] * nrrd->measurementFrame[saxi][saxj];
            }
          else
            {
            msrFrame[saxi][saxj] = nrrd->measurementFrame[saxi][saxj];
            }
          }
        }
      EncapsulateMetaData< std::vector< std::vector< double > > >(thisDic,
                                                                  std::string(key),
                                                                  msrFrame);
      }

    nrrd = nrrdNix(nrrd);
    nio = nrrdIoStateNix(nio);
    }
  catch (...)
    {
    // clean up from an exception
    nrrd = nrrdNix(nrrd);
    nio = nrrdIoStateNix(nio);

    // rethrow exception
    throw;
    }
}

void NrrdImageIO::Read(void *buffer)
{
  Nrrd *       nrrd = nrrdNew();
  bool         nrrdAllocated;

  // NOTE the main reason the logic becomes complicated here is that
  // ITK has to be the one to allocate the data segment ("buffer")

  if ( ImageIOBase::SYMMETRICSECONDRANKTENSOR == this->GetPixelType() )
    {
    // It may be that this is coming from a nrrdKind3DMaskedSymMatrix,
    // in which case ITK's buffer has not been allocated for the
    // actual size of the data.  The data will be allocated by nrrdLoad.
    nrrdAllocated = true;
    }
  else
    {
    // The data buffer has already been allocated for the correct size.
    // Hand the buffer off to the nrrd, setting just enough info so that
    // the nrrd knows the allocated data size (the axes may actually be out
    // of order in the case of non-scalar data.  Internal to nrrdLoad(), the
    // given buffer will be re-used, instead of allocating new data.
    unsigned int baseDim;
    nrrdAllocated = false;
    nrrd->data = buffer;
    nrrd->type = this->ITKToNrrdComponentType(this->m_ComponentType);
    if ( ImageIOBase::SCALAR == this->m_PixelType )
      {
      baseDim = 0;
      }
    else
      {
      baseDim = 1;
      nrrd->axis[0].size = this->GetNumberOfComponents();
      }
    nrrd->dim = baseDim + this->GetNumberOfDimensions();
    for ( unsigned int axi = 0; axi < this->GetNumberOfDimensions(); axi++ )
      {
      nrrd->axis[axi + baseDim].size = this->GetDimensions(axi);
      }
    }

#if !defined(__MINGW32__) && (defined(ITK_HAS_FEENABLEEXCEPT) || defined(_MSC_VER))
  // nrrd causes exceptions on purpose, so mask them
  bool saveFPEState(FloatingPointExceptions::GetExceptionAction() );
  FloatingPointExceptions::Disable();
#endif

  // Read in the nrrd.  Yes, this means that the header is being read
  // twice: once by NrrdImageIO::ReadImageInformation, and once here
  if ( nrrdLoad(nrrd, this->GetFileName(), ITK_NULLPTR) != 0 )
    {
    char *err =  biffGetDone(NRRD); // would be nice to free(err)
    itkExceptionMacro("Read: Error reading "
                      << this->GetFileName() << ":\n" << err);
    }

#if !defined(__MINGW32__) && (defined(ITK_HAS_FEENABLEEXCEPT) || defined(_MSC_VER))
  // restore state
  FloatingPointExceptions::SetEnabled(saveFPEState);
#endif

  unsigned int rangeAxisNum, rangeAxisIdx[NRRD_DIM_MAX];
  rangeAxisNum = nrrdRangeAxesGet(nrrd, rangeAxisIdx);

  if ( rangeAxisNum > 1 )
    {
    itkExceptionMacro("Read: handling more than one non-scalar axis "
                      "not currently handled");
    }
  if ( 1 == rangeAxisNum && 0 != rangeAxisIdx[0] )
    {
    // the range (dependent variable) is not on the fastest axis,
    // so we have to permute axes to put it there, since that is
    // how we set things up in ReadImageInformation() above
    Nrrd *       ntmp = nrrdNew();
    unsigned int axmap[NRRD_DIM_MAX];
    axmap[0] = rangeAxisIdx[0];
    for ( unsigned int axi = 1; axi < nrrd->dim; axi++ )
      {
      axmap[axi] = axi - ( axi <= rangeAxisIdx[0] );
      }
    // The memory size of the input and output of nrrdAxesPermute is
    // the same; the existing nrrd->data is re-used.
    if ( nrrdCopy(ntmp, nrrd)
         || nrrdAxesPermute(nrrd, ntmp, axmap) )
      {
      char *err =  biffGetDone(NRRD); // would be nice to free(err)
      itkExceptionMacro("Read: Error permuting independent axis in "
                        << this->GetFileName() << ":\n" << err);
      }
    nrrdNuke(ntmp);
    }

  if ( nrrdAllocated )
    {
    // Now we have to get the data back into the given ITK buffer
    // In any case, the logic here has the luxury of assuming that the
    // *single* non-scalar axis is the *first* (fastest) axis.
    if ( nrrdKind3DMaskedSymMatrix == nrrd->axis[0].kind
         && ImageIOBase::SYMMETRICSECONDRANKTENSOR == this->GetPixelType() )
      {
      // we crop out the mask and put the output in ITK-allocated "buffer"
      size_t size[NRRD_DIM_MAX], minIdx[NRRD_DIM_MAX], maxIdx[NRRD_DIM_MAX];
      for ( unsigned int axi = 0; axi < nrrd->dim; axi++ )
        {
        minIdx[axi] = ( 0 == axi ) ? 1 : 0;
        maxIdx[axi] = nrrd->axis[axi].size - 1;
        size[axi] = maxIdx[axi] - minIdx[axi] + 1;
        }
      Nrrd *ntmp = nrrdNew();
      if ( nrrdCopy(ntmp, nrrd) )
        {
        char *err = biffGetDone(NRRD); // would be nice to free(err)
        itkExceptionMacro("Read: Error copying:\n" << err);
        }
      nrrdEmpty(nrrd);
      if ( nrrdWrap_nva(nrrd, buffer, ntmp->type, ntmp->dim, size)
           || nrrdCrop(nrrd, ntmp, minIdx, maxIdx) )
        {
        char *err = biffGetDone(NRRD); // would be nice to free(err)
        itkExceptionMacro("Read: Error wrapping or cropping:\n" << err);
        }
      nrrdNuke(ntmp);
      nrrdNix(nrrd);
      }
    else
      {
      // false alarm; we didn't need to allocate the data ourselves
      memcpy( buffer, nrrd->data,
              nrrdElementSize(nrrd) * nrrdElementNumber(nrrd) );
      nrrdNuke(nrrd);
      }
    }
  else //
    {
    // "buffer" == nrrd->data was ITK-allocated; lose the nrrd struct
    nrrdNix(nrrd);
    }
}

bool NrrdImageIO::CanWriteFile(const char *name)
{
  std::string filename = name;

  if (  filename == "" )
    {
    return false;
    }

  std::string::size_type nrrdPos = filename.rfind(".nrrd");
  if ( ( nrrdPos != std::string::npos )
       && ( nrrdPos == filename.length() - 5 ) )
    {
    return true;
    }

  std::string::size_type nhdrPos = filename.rfind(".nhdr");
  if ( ( nhdrPos != std::string::npos )
       && ( nhdrPos == filename.length() - 5 ) )
    {
    return true;
    }

  return false;
}

void NrrdImageIO::WriteImageInformation(void)
{
  // Nothing needs doing here.
}

void NrrdImageIO::Write(const void *buffer)
{
  Nrrd *       nrrd = nrrdNew();
  NrrdIoState *nio = nrrdIoStateNew();
  int          kind[NRRD_DIM_MAX];
  size_t       size[NRRD_DIM_MAX];
  unsigned int nrrdDim, baseDim, spaceDim;
  double       spaceDir[NRRD_DIM_MAX][NRRD_SPACE_DIM_MAX];
  double       origin[NRRD_DIM_MAX];

  spaceDim = this->GetNumberOfDimensions();
  if ( this->GetNumberOfComponents() > 1 )
    {
    size[0] = this->GetNumberOfComponents();
    switch ( this->GetPixelType() )
      {
      case ImageIOBase::RGB:
        kind[0] = nrrdKindRGBColor;
        break;
      case ImageIOBase::RGBA:
        kind[0] = nrrdKindRGBAColor;
        break;
      case ImageIOBase::POINT:
        kind[0] = nrrdKindPoint;
        break;
      case ImageIOBase::COVARIANTVECTOR:
        kind[0] = nrrdKindCovariantVector;
        break;
      case ImageIOBase::SYMMETRICSECONDRANKTENSOR:
      case ImageIOBase::DIFFUSIONTENSOR3D:
        kind[0] = nrrdKind3DSymMatrix;
        break;
      case ImageIOBase::COMPLEX:
        kind[0] = nrrdKindComplex;
        break;
      case ImageIOBase::VECTOR:
      case ImageIOBase::OFFSET:      // HEY is this right?
      case ImageIOBase::FIXEDARRAY:  // HEY is this right?
      default:
        kind[0] = nrrdKindVector;
        break;
      }
    // the range axis has no space direction
    for ( unsigned int saxi = 0; saxi < spaceDim; saxi++ )
      {
      spaceDir[0][saxi] = AIR_NAN;
      }
    baseDim = 1;
    }
  else
    {
    baseDim = 0;
    }
  nrrdDim = baseDim + spaceDim;
  std::vector< double > spaceDirStd(spaceDim);
  unsigned int          axi;
  for ( axi = 0; axi < spaceDim; axi++ )
    {
    size[axi + baseDim] = this->GetDimensions(axi);
    kind[axi + baseDim] = nrrdKindDomain;
    origin[axi] = this->GetOrigin(axi);
    double spacing = this->GetSpacing(axi);
    spaceDirStd = this->GetDirection(axi);
    for ( unsigned int saxi = 0; saxi < spaceDim; saxi++ )
      {
      spaceDir[axi + baseDim][saxi] = spacing * spaceDirStd[saxi];
      }
    }
  if ( nrrdWrap_nva(nrrd, const_cast< void * >( buffer ),
                    this->ITKToNrrdComponentType(m_ComponentType),
                    nrrdDim, size) || ( 3 == spaceDim
                                        // special case: ITK is LPS in 3-D
                                        ? nrrdSpaceSet(nrrd, nrrdSpaceLeftPosteriorSuperior)
                                        : nrrdSpaceDimensionSet(nrrd, spaceDim) )
       || nrrdSpaceOriginSet(nrrd, origin) )
    {
    char *err = biffGetDone(NRRD); // would be nice to free(err)
    itkExceptionMacro("Write: Error wrapping nrrd for "
                      << this->GetFileName() << ":\n" << err);
    }
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoKind, kind);
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoSpaceDirection, spaceDir);

  // Go through MetaDataDictionary and set either specific nrrd field
  // or a key/value pair
  MetaDataDictionary &                       thisDic = this->GetMetaDataDictionary();
  std::vector< std::string >                 keys = thisDic.GetKeys();
  std::vector< std::string >::const_iterator keyIt;
  const char *                               keyField, *field;
  for ( keyIt = keys.begin(); keyIt != keys.end(); ++keyIt )
    {
    if ( !strncmp( KEY_PREFIX, ( *keyIt ).c_str(), strlen(KEY_PREFIX) ) )
      {
      keyField = ( *keyIt ).c_str() + strlen(KEY_PREFIX);
      // only of one of these can succeed
      field = airEnumStr(nrrdField, nrrdField_thicknesses);
      if ( !strncmp( keyField, field, strlen(field) ) )
        {
        if ( 1 == sscanf(keyField + strlen(field), "[%u]", &axi)
             && axi + baseDim < nrrd->dim )
          {
          double thickness = 0.0;
          ExposeMetaData< double >(thisDic, *keyIt, thickness);
          nrrd->axis[axi + baseDim].thickness = thickness;
          }
        }
      field = airEnumStr(nrrdField, nrrdField_centers);
      if ( !strncmp( keyField, field, strlen(field) ) )
        {
        if ( 1 == sscanf(keyField + strlen(field), "[%u]", &axi)
             && axi + baseDim < nrrd->dim )
          {
          std::string value;
          ExposeMetaData< std::string >(thisDic, *keyIt, value);
          nrrd->axis[axi + baseDim].center = airEnumVal( nrrdCenter,
                                                         value.c_str() );
          }
        }
      field = airEnumStr(nrrdField, nrrdField_kinds);
      if ( !strncmp( keyField, field, strlen(field) ) )
        {
        if ( 1 == sscanf(keyField + strlen(field), "[%u]", &axi)
             && axi + baseDim < nrrd->dim )
          {
          std::string value;
          ExposeMetaData< std::string >(thisDic, *keyIt, value);
          nrrd->axis[axi + baseDim].kind = airEnumVal( nrrdKind,
                                                       value.c_str() );
          }
        }
      field = airEnumStr(nrrdField, nrrdField_labels);
      if ( !strncmp( keyField, field, strlen(field) ) )
        {
        if ( 1 == sscanf(keyField + strlen(field), "[%u]", &axi)
             && axi + baseDim < nrrd->dim )
          {
          std::string value;
          ExposeMetaData< std::string >(thisDic, *keyIt, value);
          nrrd->axis[axi + baseDim].label = airStrdup( value.c_str() );
          }
        }
      field = airEnumStr(nrrdField, nrrdField_old_min);
      if ( !strncmp( keyField, field, strlen(field) ) )
        {
        ExposeMetaData< double >(thisDic, *keyIt, nrrd->oldMin);
        }
      field = airEnumStr(nrrdField, nrrdField_old_max);
      if ( !strncmp( keyField, field, strlen(field) ) )
        {
        ExposeMetaData< double >(thisDic, *keyIt, nrrd->oldMax);
        }

      field = airEnumStr(nrrdField, nrrdField_space);
      if ( !strncmp( keyField, field, strlen(field) ) )
        {
        int         space;
        std::string value;
        ExposeMetaData< std::string >(thisDic, *keyIt, value);
        space = airEnumVal( nrrdSpace, value.c_str() );
        if ( nrrdSpaceDimension(space) == nrrd->spaceDim )
          {
          // sanity check
          nrrd->space = space;
          }
        }

      field = airEnumStr(nrrdField, nrrdField_content);
      if ( !strncmp( keyField, field, strlen(field) ) )
        {
        std::string value;
        ExposeMetaData< std::string >(thisDic, *keyIt, value);
        nrrd->content = airStrdup( value.c_str() );
        }
      field = airEnumStr(nrrdField, nrrdField_measurement_frame);
      if ( !strncmp( keyField, field, strlen(field) ) )
        {
        std::vector< std::vector< double > > msrFrame;
        ExposeMetaData< std::vector< std::vector< double > > >(thisDic,
                                                               *keyIt, msrFrame);
        for ( unsigned int saxi = 0; saxi < nrrd->spaceDim; saxi++ )
          {
          for ( unsigned int saxj = 0; saxj < nrrd->spaceDim; saxj++ )
            {
            if ( saxi < msrFrame.size()
                 && saxj < msrFrame[saxi].size() )
              {
              nrrd->measurementFrame[saxi][saxj] = msrFrame[saxi][saxj];
              }
            else
              {
              // there is a difference between the dimension of the
              // recorded measurement frame, and the actual dimension of
              // the ITK image, which (for now) determines nrrd->spaceDim.
              // We can't set this to AIR_NAN, because the coefficients of
              // the measurement frame have to all be equally existent.
              // If we used 0, it might not a flag that something is wrong.
              // So, we have to get creative.
              nrrd->measurementFrame[saxi][saxj] = 666666;
              }
            }
          }
        }
      }
    else
      {
      // not a NRRD field packed into meta data; just a regular key/value
      std::string value;
      ExposeMetaData< std::string >(thisDic, *keyIt, value);
      nrrdKeyValueAdd( nrrd, ( *keyIt ).c_str(), value.c_str() );
      }
    }

  // set encoding for data: compressed (raw), (uncompressed) raw, or ascii
  if ( this->GetUseCompression() == true
       && nrrdEncodingGzip->available() )
    {
    // this is necessarily gzip-compressed *raw* data
    nio->encoding = nrrdEncodingGzip;
    }
  else
    {
    Superclass::FileType fileType = this->GetFileType();
    switch ( fileType )
      {
      default:
      case TypeNotApplicable:
      case Binary:
        nio->encoding = nrrdEncodingRaw;
        break;
      case ASCII:
        nio->encoding = nrrdEncodingAscii;
        break;
      }
    }

  // set desired endianness of output
  Superclass::ByteOrder byteOrder = this->GetByteOrder();
  switch ( byteOrder )
    {
    default:
    case OrderNotApplicable:
      nio->endian = airEndianUnknown;
      break;
    case BigEndian:
      nio->endian = airEndianBig;
      break;
    case LittleEndian:
      nio->endian = airEndianLittle;
      break;
    }

  // Write the nrrd to file.
  if ( nrrdSave(this->GetFileName(), nrrd, nio) )
    {
    char *err = biffGetDone(NRRD); // would be nice to free(err)
    itkExceptionMacro("Write: Error writing "
                      << this->GetFileName() << ":\n" << err);
    }

  // Free the nrrd struct but don't touch nrrd->data
  nrrdNix(nrrd);
  nrrdIoStateNix(nio);
}

} // end namespace itk
