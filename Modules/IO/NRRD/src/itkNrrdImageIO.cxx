/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include <sstream>

namespace
{
// This function determines which NRRD axis should be used for pixel component in the ITK image,
// and which NRRD axes should be used for the ITK image axes.
// The pixel component axis is the first NRRD range axis (preferably a non-list axis).
// The image axes are the NRRD domain axes followed by the range axes (except the one that was already used as
// component).
void
GetAxisOrderForFileReading(Nrrd *                             nrrd,
                           std::vector<unsigned int> &        imageAxes_nrrd,
                           int &                              pixelAxisIndex,
                           unsigned int &                     numberOfDomainAxes,
                           bool &                             needPermutation,
                           itk::NrrdImageIOEnums::AxesReorder axesReorder)
{
  imageAxes_nrrd.clear();

  // domainAxis: space dimensions.
  // rangeAxis: additional axes that are not space dimensions, such as components or time points.
  // Examples:
  //   domain domain domain list -> numberOfDomainAxes = 3, rangeAxisNum_nrrd = 1
  //   list domain domain domain -> numberOfDomainAxes = 3, rangeAxisNum_nrrd = 1
  //   RGBA-color domain domain domain list -> numberOfDomainAxes = 3, rangeAxisNum_nrrd = 2
  //   covariant-vector domain domain domain list -> numberOfDomainAxes = 3, rangeAxisNum_nrrd = 2

  unsigned int domainAxisIndices_nrrd[NRRD_DIM_MAX]{};
  numberOfDomainAxes = nrrdDomainAxesGet(nrrd, domainAxisIndices_nrrd);
  unsigned int rangeAxisIndices_nrrd[NRRD_DIM_MAX]{};
  unsigned int rangeAxisNum_nrrd = nrrdRangeAxesGet(nrrd, rangeAxisIndices_nrrd);

  pixelAxisIndex = -1; // No pixel component axis

  if (rangeAxisNum_nrrd > 0 && (axesReorder != itk::NrrdImageIOEnums::AxesReorder::UseScalarPixel))
  {
    // A range axes may be used as pixel component.

    // If there is a non-list-type range axis then use the first one as pixel component.
    for (unsigned int axInd = 0; axInd < rangeAxisNum_nrrd; ++axInd)
    {
      if (nrrd->axis[rangeAxisIndices_nrrd[axInd]].kind != nrrdKindList)
      {
        pixelAxisIndex = rangeAxisIndices_nrrd[axInd];
        break;
      }
    }
    if (pixelAxisIndex < 0 && axesReorder == itk::NrrdImageIOEnums::AxesReorder::UseAnyRangeAxisAsPixel)
    {
      // The image only has list-type range axes. UseAnyRangeAxisAsPixel strategy allows using that as pixel component.
      pixelAxisIndex = rangeAxisIndices_nrrd[0];
    }
  }

  // There are multiple range axes. For pixel component, prefer the first axis that is not "list" kind.
  // For example, if there are RGBColor and a list axes then prefer using the non-list RGBColor as component axis.
  // List axes will be additional image dimensions.

  // Add all domain axes
  for (unsigned int domainAxisIndex = 0; domainAxisIndex < numberOfDomainAxes; ++domainAxisIndex)
  {
    unsigned int domainAxis_nrrd = domainAxisIndices_nrrd[domainAxisIndex];
    imageAxes_nrrd.push_back(domainAxis_nrrd);
  }
  // Add all range axes that are not already added as component axis
  for (unsigned int rangeAxisIndex = 0; rangeAxisIndex < rangeAxisNum_nrrd; ++rangeAxisIndex)
  {
    unsigned int rangeAxis_nrrd = rangeAxisIndices_nrrd[rangeAxisIndex];
    if (static_cast<int>(rangeAxis_nrrd) != pixelAxisIndex)
    {
      imageAxes_nrrd.push_back(rangeAxis_nrrd);
    }
  }

  needPermutation = false;
  if (pixelAxisIndex > 0)
  {
    // pixel axis has to be moved to be the fastest axis
    needPermutation = true;
  }
  else
  {
    // No need to reorder
    // - if pixelAxisIndex == 0 (pixel components are mapped to NRRD axis 0) and image axes match NRRD axes of 1, 2, ...
    // - if pixelAxisIndex < 0 (pixel type is not mapped to a NRRD axis) and image axes match NRRD axes of 0, 1, ...
    unsigned int offset = (pixelAxisIndex == 0 ? 1 : 0);
    for (unsigned int i = 0; i < imageAxes_nrrd.size(); ++i)
    {
      // Check if the order of axes in the NRRD file is different from the order of axes in the ITK image.
      if (imageAxes_nrrd[i] != i + offset)
      {
        needPermutation = true;
        break;
      }
    }
  }
}

const std::string NRRD_KEY_PREFIX{ "NRRD_" };

} // namespace

namespace itk
{

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const NrrdImageIOEnums::AxesReorder value)
{
  return out << [value] {
    switch (value)
    {
      case NrrdImageIOEnums::AxesReorder::UseAnyRangeAxisAsPixel:
        return "itk::NrrdImageIOEnums::AxesReorder::UseAnyRangeAxisAsPixel";
      case NrrdImageIOEnums::AxesReorder::UseNonListRangeAxisAsPixel:
        return "itk::NrrdImageIOEnums::AxesReorder::UseNonListRangeAxisAsPixel";
      case NrrdImageIOEnums::AxesReorder::UseScalarPixel:
        return "itk::NrrdImageIOEnums::AxesReorder::UseScalarPixel";
      default:
        return "INVALID VALUE FOR itk::NrrdImageIOEnums::AxesReorder";
    }
  }();
}

NrrdImageIO::NrrdImageIO()
{
  this->SetNumberOfDimensions(3);

  const char * extensions[] = { ".nrrd", ".nhdr" };

  for (auto ext : extensions)
  {
    this->AddSupportedWriteExtension(ext);
    this->AddSupportedReadExtension(ext);
  }

  this->Self::SetCompressor("");
  this->Self::SetMaximumCompressionLevel(9);
  this->Self::SetCompressionLevel(2);
}

NrrdImageIO::~NrrdImageIO() = default;

bool
NrrdImageIO::SupportsDimension(unsigned long dim)
{
  if (1 == this->GetNumberOfComponents())
  {
    return dim <= NRRD_DIM_MAX;
  }

  return dim <= NRRD_DIM_MAX - 1;
}

void
NrrdImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NrrdCompressionEncoding: " << m_NrrdCompressionEncoding << std::endl;
}

void
NrrdImageIO::InternalSetCompressor(const std::string & _compressor)
{
  this->m_NrrdCompressionEncoding = nullptr;

  // set default to gzip
  if (_compressor.empty())
  {
    if (nrrdEncodingGzip->available())
    {
      this->m_NrrdCompressionEncoding = nrrdEncodingGzip;
    }
    return;
  }

  const NrrdEncoding * nrrdCompressionEncodings[] = { nrrdEncodingGzip, nrrdEncodingBzip2 };

  for (auto & nrrdEncoding : nrrdCompressionEncodings)
  {
    if (!nrrdEncoding->available())
    {
      continue;
    }

    std::string name = nrrdEncoding->name;
    std::transform(name.begin(), name.end(), name.begin(), ::toupper);

    if (_compressor == name)
    {
      this->m_NrrdCompressionEncoding = nrrdEncoding;
      return;
    }
  }
  this->Superclass::InternalSetCompressor(_compressor);
}

IOComponentEnum
NrrdImageIO::NrrdToITKComponentType(const int nrrdComponentType) const
{
  switch (nrrdComponentType)
  {
    case nrrdTypeUnknown:
    case nrrdTypeBlock:
      return IOComponentEnum::UNKNOWNCOMPONENTTYPE;

    case nrrdTypeChar:
      return IOComponentEnum::CHAR;

    case nrrdTypeUChar:
      return IOComponentEnum::UCHAR;

    case nrrdTypeShort:
      return IOComponentEnum::SHORT;

    case nrrdTypeUShort:
      return IOComponentEnum::USHORT;

    case nrrdTypeInt:
      return IOComponentEnum::INT;

    case nrrdTypeUInt:
      return IOComponentEnum::UINT;

    case nrrdTypeLLong:
      return IOComponentEnum::LONGLONG;

    case nrrdTypeULLong:
      return IOComponentEnum::ULONGLONG;

    case nrrdTypeFloat:
      return IOComponentEnum::FLOAT;

    case nrrdTypeDouble:
      return IOComponentEnum::DOUBLE;
  }
  // Strictly to avoid compiler warning regarding "control may reach end of
  // non-void function":
  return IOComponentEnum::UNKNOWNCOMPONENTTYPE;
}

int
NrrdImageIO::ITKToNrrdComponentType(const IOComponentEnum itkComponentType) const
{
  switch (itkComponentType)
  {
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      return nrrdTypeUnknown;

    case IOComponentEnum::SCHAR:
      return nrrdTypeChar;

    case IOComponentEnum::UCHAR:
      return nrrdTypeUChar;

    case IOComponentEnum::SHORT:
      return nrrdTypeShort;

    case IOComponentEnum::USHORT:
      return nrrdTypeUShort;

    // "long" is a silly type because it basically guaranteed not to be
    // cross-platform across 32-vs-64 bit machines, but we can figure out
    // a cross-platform way of storing the information.
    case IOComponentEnum::LONG:
      return (4 == sizeof(long)) ? nrrdTypeInt : nrrdTypeLLong;

    case IOComponentEnum::ULONG:
      return (4 == sizeof(long)) ? nrrdTypeUInt : nrrdTypeULLong;

    case IOComponentEnum::INT:
      return nrrdTypeInt;

    case IOComponentEnum::UINT:
      return nrrdTypeUInt;

    case IOComponentEnum::LONGLONG:
      return nrrdTypeLLong;

    case IOComponentEnum::ULONGLONG:
      return nrrdTypeULLong;

    case IOComponentEnum::FLOAT:
      return nrrdTypeFloat;

    case IOComponentEnum::DOUBLE:
      return nrrdTypeDouble;
    case IOComponentEnum::LDOUBLE:
      return nrrdTypeUnknown; // Long double not supported by nrrd
  }
  // Strictly to avoid compiler warning regarding "control may reach end of
  // non-void function":
  return nrrdTypeUnknown;
}

bool
NrrdImageIO::CanReadFile(const char * filename)
{
  // Check the extension first to avoid opening a file that does not
  // look like NRRD.  The file must have an appropriate extension to be
  // recognized.
  const std::string fname = filename;

  const bool extensionFound = this->HasSupportedReadExtension(filename);

  if (!extensionFound)
  {
    itkDebugMacro("The filename extension is not recognized");
    return false;
  }

  // We have the correct extension, so now check for the Nrrd magic "NRRD",
  // while ignoring the format version (the next four characters)
  std::ifstream inputStream;
  try
  {
    this->OpenFileForReading(inputStream, fname);
  }
  catch (const ExceptionObject &)
  {
    return false;
  }

  char magic[5] = { '\0', '\0', '\0', '\0', '\0' };
  inputStream.read(magic, 4 * sizeof(char));

  if (inputStream.eof())
  {
    inputStream.close();
    return false;
  }

  if (strcmp(magic, "NRRD") == 0)
  {
    inputStream.close();
    return true;
  }

  inputStream.close();
  return false;
}

void
NrrdImageIO::ReadImageInformation()
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

  Nrrd *        nrrd = nrrdNew();
  NrrdIoState * nio = nrrdIoStateNew();

  try
  {
    // nrrd causes exceptions on purpose, so mask them
    bool saveFPEState(false);
    if (FloatingPointExceptions::HasFloatingPointExceptionsSupport())
    {
      saveFPEState = FloatingPointExceptions::GetEnabled();
      FloatingPointExceptions::Disable();
    }

    // this is the mechanism by which we tell nrrdLoad to read
    // just the header, and none of the data
    nrrdIoStateSet(nio, nrrdIoStateSkipData, 1);
    if (nrrdLoad(nrrd, this->GetFileName(), nio) != 0)
    {
      char * err = biffGetDone(NRRD);

      // don't use macro so that we can free err
      std::ostringstream message;
      message << "itk::ERROR: " << this->GetNameOfClass() << '(' << this << "): "
              << "ReadImageInformation: Error reading " << this->GetFileName() << ":\n"
              << err;
      ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(), ITK_LOCATION);
      free(err);
      throw e_;
    }

    // restore state
    if (FloatingPointExceptions::HasFloatingPointExceptionsSupport())
    {
      FloatingPointExceptions::SetEnabled(saveFPEState);
    }


    if (nrrdTypeBlock == nrrd->type)
    {
      itkExceptionMacro("ReadImageInformation: Cannot currently "
                        "handle nrrdTypeBlock");
    }
    if (nio->endian == airEndianLittle)
    {
      this->SetByteOrderToLittleEndian();
    }
    else if (nio->endian == airEndianBig)
    {
      this->SetByteOrderToBigEndian();
    }
    else
    {
      this->SetByteOrder(IOByteOrderEnum::OrderNotApplicable);
    }

    if (nio->encoding == nrrdEncodingAscii)
    {
      this->SetFileTypeToASCII();
    }
    else
    {
      this->SetFileTypeToBinary();
    }
    // set type of pixel components; this is orthogonal to pixel type

    const IOComponentEnum cmpType = this->NrrdToITKComponentType(nrrd->type);
    if (IOComponentEnum::UNKNOWNCOMPONENTTYPE == cmpType)
    {
      itkExceptionMacro("Nrrd type " << airEnumStr(nrrdType, nrrd->type)
                                     << " could not be mapped to an ITK component type");
    }
    this->SetComponentType(cmpType);

    int                       pixelAxisIndex{ -1 };
    std::vector<unsigned int> imageAxes_nrrd;
    bool                      needPermutation{ false };
    unsigned int              numberOfDomainAxes{ 0 };
    GetAxisOrderForFileReading(
      nrrd, imageAxes_nrrd, pixelAxisIndex, numberOfDomainAxes, needPermutation, this->GetAxesReorder());

    if (nrrd->spaceDim > 0)
    {
      // "space directions" is specified in the NRRD file
      if (nrrd->spaceDim != numberOfDomainAxes)
      {
        itkExceptionMacro("ReadImageInformation: number of domain axes in the NRRD file ("
                          << numberOfDomainAxes
                          << ") doesn't match dimension of space in which orientation is defined (" << nrrd->spaceDim
                          << "). This is not supported.");
      }
    }

    if (pixelAxisIndex < 0)
    {
      // simple scalar image
      this->SetNumberOfDimensions(nrrd->dim);
      this->SetPixelType(IOPixelEnum::SCALAR);
      this->SetNumberOfComponents(1);
    }
    else
    {
      this->SetNumberOfDimensions(imageAxes_nrrd.size());
      int    kind = nrrd->axis[pixelAxisIndex].kind;
      size_t size = nrrd->axis[pixelAxisIndex].size;
      // NOTE: it is the NRRD readers responsibility to make sure that
      // the size (#of components) associated with a specific kind is
      // matches the actual size of the axis.
      switch (kind)
      {
        case nrrdKindDomain:
        case nrrdKindSpace:
        case nrrdKindTime:
          itkExceptionMacro("ReadImageInformation: range axis kind (" << airEnumStr(nrrdKind, kind)
                                                                      << ") seems more "
                                                                         "like a domain axis than a range axis");

        case nrrdKindStub:
        case nrrdKindScalar:
          this->SetPixelType(IOPixelEnum::SCALAR);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKind3Color:
        case nrrdKindRGBColor:
          this->SetPixelType(IOPixelEnum::RGB);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKind4Color:
        case nrrdKindRGBAColor:
          this->SetPixelType(IOPixelEnum::RGBA);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKindVector:
        case nrrdKind2Vector:
        case nrrdKind3Vector:
        case nrrdKind4Vector:
        case nrrdKindList:
          this->SetPixelType(IOPixelEnum::VECTOR);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKindPoint:
          this->SetPixelType(IOPixelEnum::POINT);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKindCovariantVector:
        case nrrdKind3Gradient:
        case nrrdKindNormal:
        case nrrdKind3Normal:
          this->SetPixelType(IOPixelEnum::COVARIANTVECTOR);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKind3DSymMatrix:
          // IOPixelEnum::DIFFFUSIONTENSOR3D is a subclass
          this->SetPixelType(IOPixelEnum::SYMMETRICSECONDRANKTENSOR);
          this->SetNumberOfComponents(size);
          break;
        case nrrdKind3DMaskedSymMatrix:
          this->SetPixelType(IOPixelEnum::SYMMETRICSECONDRANKTENSOR);
          // NOTE: we will crop out the mask in Read() below; this is the
          // one case where NumberOfComponents != size
          this->SetNumberOfComponents(size - 1);
          break;
        case nrrdKindComplex:
          this->SetPixelType(IOPixelEnum::COMPLEX);
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
          this->SetPixelType(IOPixelEnum::VECTOR);
          this->SetNumberOfComponents(size);
          break;
        default:
          itkExceptionMacro("ReadImageInformation: nrrdKind " << kind << " not known!");
      }
    }

    unsigned int imageDimensions = imageAxes_nrrd.size();

    // used to flip the measurement frame later on
    std::vector<double> axisDirectionFactors(numberOfDomainAxes, 1.0);
    bool                normalizeAxisDirectionsToLPS = false;
    switch (nrrd->space)
    {
      // on read, convert non-LPS coords into LPS coords, when we can
      case nrrdSpaceRightAnteriorSuperior:
        axisDirectionFactors[0] = -1.0; // R -> L
        axisDirectionFactors[1] = -1.0; // A -> P
        normalizeAxisDirectionsToLPS = true;
        break;
      case nrrdSpaceLeftAnteriorSuperior:
        axisDirectionFactors[1] = -1; // A -> P
        normalizeAxisDirectionsToLPS = true;
        break;
      case nrrdSpaceLeftPosteriorSuperior:
        normalizeAxisDirectionsToLPS = true;
        // no change needed
        break;
      default:
        // we're not coming from a space for which the conversion
        // to LPS is well-defined
        break;
    }


    for (unsigned int domainAxis_itk = 0; domainAxis_itk < numberOfDomainAxes; ++domainAxis_itk)
    {
      unsigned int domainAxis_nrrd = imageAxes_nrrd[domainAxis_itk];
      this->SetDimensions(domainAxis_itk, static_cast<unsigned int>(nrrd->axis[domainAxis_nrrd].size));
      double spacing{ 1.0 };
      double spaceDir[NRRD_SPACE_DIM_MAX]{};
      int    spacingStatus = nrrdSpacingCalculate(nrrd, domainAxis_nrrd, &spacing, spaceDir);

      switch (spacingStatus)
      {
        case nrrdSpacingStatusNone:
          // Let ITK's defaults stay
          // this->SetSpacing(domainAxis_itk, 1.0);
          break;
        case nrrdSpacingStatusScalarNoSpace:
          this->SetSpacing(domainAxis_itk, spacing);
          break;
        case nrrdSpacingStatusDirection:
          if (AIR_EXISTS(spacing))
          {
            // only set info if we have something to set
            this->SetSpacing(domainAxis_itk, spacing);
            std::vector<double> spaceDirStd(imageDimensions);
            for (unsigned int axis = 0; axis < numberOfDomainAxes; ++axis)
            {
              spaceDirStd[axis] = axisDirectionFactors[axis] * spaceDir[axis];
            }
            this->SetDirection(domainAxis_itk, spaceDirStd);
          }
          break;
        default:
        case nrrdSpacingStatusUnknown:
          itkExceptionMacro("ReadImageInformation: Error interpreting "
                            "nrrd spacing (nrrdSpacingStatusUnknown)");
        case nrrdSpacingStatusScalarWithSpace:
          // Both "spacings" and "space origin" fields are specified. This is an unusual combination, no need to support
          // it. Either use space fields ("space directions" and "space origin") or use older convention of just
          // "spacings".
          itkExceptionMacro("ReadImageInformation: Error interpreting "
                            "nrrd spacing (nrrdSpacingStatusScalarWithSpace)");
      }
    }

    // If there are additional range axis in the NRRD file then add those as extra domain axes to the ITK image.
    for (unsigned int axis_itk = numberOfDomainAxes; axis_itk < imageAxes_nrrd.size(); ++axis_itk)
    {
      unsigned int axis_nrrd = imageAxes_nrrd[axis_itk];
      // NRRD file range axis is added to the ITK image as domain axis
      this->SetDimensions(axis_itk, static_cast<unsigned int>(nrrd->axis[axis_nrrd].size));

      // Cannot calculate spacing for this axis using nrrdSpacingCalculate,
      // because in NRRD it is a range axis, not domain kind. Set default values for axis.
      this->SetSpacing(axis_itk, 1.0);

      std::vector<double> spaceDirStd(imageDimensions);
      for (unsigned int axis = 0; axis < imageDimensions; ++axis)
      {
        spaceDirStd[axis] = (axis == axis_itk ? 1.0 : 0.0);
      }
      this->SetDirection(axis_itk, spaceDirStd);
    }

    // Set origin for NRRD domain axes
    if (nrrd->spaceDim > 0)
    {
      // "space directions" is specified in the NRRD file, set origin from "space origin" field
      if (AIR_EXISTS(nrrd->spaceOrigin[0]))
      {
        // only set info if we have something to set
        double spaceOrigin[NRRD_SPACE_DIM_MAX];
        for (unsigned int axis = 0; axis < nrrd->spaceDim; ++axis)
        {
          spaceOrigin[axis] = nrrd->spaceOrigin[axis];
        }
        switch (nrrd->space)
        {
          // convert non-LPS coords into LPS coords, when we can
          case nrrdSpaceRightAnteriorSuperior:
            spaceOrigin[0] *= -1; // R -> L
            spaceOrigin[1] *= -1; // A -> P
            break;
          case nrrdSpaceLeftAnteriorSuperior:
            spaceOrigin[1] *= -1; // A -> P
            break;
          case nrrdSpaceLeftPosteriorSuperior:
            // no change needed
            break;
          default:
            // we're not coming from a space for which the conversion
            // to LPS is well-defined
            break;
        }
        for (unsigned int axis = 0; axis < nrrd->spaceDim; ++axis)
        {
          this->SetOrigin(axis, spaceOrigin[axis]);
        }
      }
    }
    else
    {
      // "space directions" is not specified in the NRRD file, origin may still be possible to calculate
      double       spaceOrigin[NRRD_DIM_MAX]{};
      unsigned int domainAxisIndices_nrrd[NRRD_DIM_MAX]{};

      unsigned int numberOfDomainAxesForOrigin = nrrdDomainAxesGet(nrrd, domainAxisIndices_nrrd);
      if (numberOfDomainAxesForOrigin != numberOfDomainAxes)
      {
        // this should never happen, as numberOfDomainAxes is obtained by calling a same method earlier
        // but it is safer to check than just assume they match
        itkExceptionMacro("ReadImageInformation: internal error, mismatch in number of domain axes.");
      }

      int originStatus =
        nrrdOriginCalculate(nrrd, domainAxisIndices_nrrd, numberOfDomainAxes, nrrdCenterCell, spaceOrigin);
      for (unsigned int saxi = 0; saxi < numberOfDomainAxes; ++saxi)
      {
        switch (originStatus)
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
    MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
    // Necessary to clear dict if ImageIO object is re-used
    thisDic.Clear();
    const std::string classname(this->GetNameOfClass());
    EncapsulateMetaData<std::string>(thisDic, ITK_InputFilterName, classname);
    for (unsigned int kvpi = 0; kvpi < nrrdKeyValueSize(nrrd); ++kvpi)
    {
      char * keyPtr = nullptr;
      char * valPtr = nullptr;
      nrrdKeyValueIndex(nrrd, &keyPtr, &valPtr, kvpi);
      EncapsulateMetaData<std::string>(thisDic, std::string(keyPtr), std::string(valPtr));
      keyPtr = static_cast<char *>(airFree(keyPtr));
      valPtr = static_cast<char *>(airFree(valPtr));
    }

    // save in MetaDataDictionary those important nrrd fields that
    // (currently) have no ITK equivalent. NOTE that for the per-axis
    // information, we use the same axis index (domainAxisIndex) as in ITK, NOT
    // the original axis index in nrrd (axis_nrrd).  This is because in the
    // Read() method, non-scalar data is permuted to the fastest axis,
    // on the on the Write() side, its always written to the fastest axis,
    // so we might as well go with consistent and idiomatic indexing.
    // for (unsigned int domainAxisIndex = 0; domainAxisIndex < numberOfDomainAxes; ++domainAxisIndex)
    for (unsigned int axis_itk = 0; axis_itk < imageDimensions; ++axis_itk)
    {
      unsigned int   axis_nrrd = imageAxes_nrrd[axis_itk];
      NrrdAxisInfo * axisInfo = nrrd->axis + axis_nrrd;
      if (AIR_EXISTS(axisInfo->thickness))
      {
        std::string key =
          NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_thicknesses) + "[" + std::to_string(axis_itk) + "]";
        EncapsulateMetaData<double>(thisDic, key, axisInfo->thickness);
      }
      if (axisInfo->center)
      {
        std::string key =
          NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_centers) + "[" + std::to_string(axis_itk) + "]";
        EncapsulateMetaData<std::string>(thisDic, key, airEnumStr(nrrdCenter, axisInfo->center));
      }
      if (axisInfo->kind)
      {
        std::string key =
          NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_kinds) + "[" + std::to_string(axis_itk) + "]";
        EncapsulateMetaData<std::string>(thisDic, key, airEnumStr(nrrdKind, axisInfo->kind));
      }
      if (airStrlen(axisInfo->label))
      {
        std::string key =
          NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_labels) + "[" + std::to_string(axis_itk) + "]";
        EncapsulateMetaData<std::string>(thisDic, key, axisInfo->label);
      }
      if (airStrlen(axisInfo->units))
      {
        std::string key =
          NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_units) + "[" + std::to_string(axis_itk) + "]";
        EncapsulateMetaData<std::string>(thisDic, key, axisInfo->units);
      }
    }
    if (pixelAxisIndex >= 0)
    {
      // The pixel component has no assigned axis number in the ITK image. Therefore use
      // NRRD_<field>[pixel] metadata key for saving label and unit information.
      NrrdAxisInfo * axisInfo = nrrd->axis + pixelAxisIndex;
      if (axisInfo->kind)
      {
        std::string key = NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_kinds) + "[pixel]";
        EncapsulateMetaData<std::string>(thisDic, key, airEnumStr(nrrdKind, axisInfo->kind));
      }
      if (airStrlen(axisInfo->label))
      {
        std::string key = NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_labels) + "[pixel]";
        EncapsulateMetaData<std::string>(thisDic, key, axisInfo->label);
      }
      if (airStrlen(axisInfo->units))
      {
        std::string key = NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_units) + "[pixel]";
        EncapsulateMetaData<std::string>(thisDic, key, axisInfo->units);
      }
      // Save the original component index, just incase there are some custom metadata fields that
      // can only be interpreted if this information is known
      std::string key = NRRD_KEY_PREFIX + "pixel_original_axis";
      EncapsulateMetaData<int>(thisDic, key, pixelAxisIndex);
    }

    // Parse generic (not per-axis) metadata
    if (airStrlen(nrrd->content))
    {
      EncapsulateMetaData<std::string>(
        thisDic, NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_content), nrrd->content);
    }
    if (AIR_EXISTS(nrrd->oldMin))
    {
      EncapsulateMetaData<double>(thisDic, NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_old_min), nrrd->oldMin);
    }
    if (AIR_EXISTS(nrrd->oldMax))
    {
      EncapsulateMetaData<double>(thisDic, NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_old_max), nrrd->oldMax);
    }
    if (nrrd->space != nrrdSpaceUnknown)
    {
      // If the input directions were normalized to LPS then use that as space.
      // Otherwise leave the space unchanged.
      int space = (normalizeAxisDirectionsToLPS ? nrrdSpaceLeftPosteriorSuperior : nrrd->space);
      EncapsulateMetaData<std::string>(
        thisDic, NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_space), airEnumStr(nrrdSpace, space));
    }

    if (AIR_EXISTS(nrrd->measurementFrame[0][0]))
    {
      std::vector<std::vector<double>> msrFrame(numberOfDomainAxes);

      // flip the measurement frame here if we have to
      // so that everything is consistent with the ITK LPS space directions
      // but only do this if we have a three dimensional space or smaller
      for (unsigned int saxi = 0; saxi < numberOfDomainAxes; ++saxi)
      {
        msrFrame[saxi].resize(numberOfDomainAxes);
        for (unsigned int saxj = 0; saxj < numberOfDomainAxes; ++saxj)
        {
          if (numberOfDomainAxes <= 3)
          {
            msrFrame[saxi][saxj] = axisDirectionFactors[saxj] * nrrd->measurementFrame[saxi][saxj];
          }
          else
          {
            msrFrame[saxi][saxj] = nrrd->measurementFrame[saxi][saxj];
          }
        }
      }
      std::string key = NRRD_KEY_PREFIX + airEnumStr(nrrdField, nrrdField_measurement_frame);
      EncapsulateMetaData<std::vector<std::vector<double>>>(thisDic, key, msrFrame);
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

void
NrrdImageIO::Read(void * buffer)
{
  Nrrd * nrrd = nrrdNew();
  bool   nrrdAllocated;

  // NOTE the main reason the logic becomes complicated here is that
  // ITK has to be the one to allocate the data segment ("buffer")

  if (this->GetPixelType() == IOPixelEnum::SYMMETRICSECONDRANKTENSOR)
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
    nrrdAllocated = false;
    nrrd->data = buffer;
    nrrd->type = this->ITKToNrrdComponentType(this->m_ComponentType);
    if (IOPixelEnum::SCALAR == this->m_PixelType)
    {
      nrrd->dim = this->GetNumberOfDimensions();
      for (unsigned int axis = 0; axis < this->GetNumberOfDimensions(); ++axis)
      {
        nrrd->axis[axis].size = this->GetDimensions(axis);
      }
    }
    else
    {
      nrrd->axis[0].size = this->GetNumberOfComponents();
      nrrd->dim = this->GetNumberOfDimensions() + 1;
      for (unsigned int axis = 0; axis < this->GetNumberOfDimensions(); ++axis)
      {
        nrrd->axis[axis + 1].size = this->GetDimensions(axis);
      }
    }
  }

#if !defined(__MINGW32__) && (defined(ITK_HAS_FEENABLEEXCEPT) || defined(_MSC_VER))
  // nrrd causes exceptions on purpose, so mask them
  bool saveFPEState{ FloatingPointExceptions::GetExceptionAction() ==
                     itk::FloatingPointExceptions::ExceptionActionEnum::EXIT };
  FloatingPointExceptions::Disable();
#endif

  // Read in the nrrd.  Yes, this means that the header is being read
  // twice: once by NrrdImageIO::ReadImageInformation, and once here
  if (nrrdLoad(nrrd, this->GetFileName(), nullptr) != 0)
  {
    char * err = biffGetDone(NRRD); // would be nice to free(err)
    itkExceptionMacro("Read: Error reading " << this->GetFileName() << ":\n" << err);
  }

#if !defined(__MINGW32__) && (defined(ITK_HAS_FEENABLEEXCEPT) || defined(_MSC_VER))
  // restore state
  FloatingPointExceptions::SetEnabled(saveFPEState);
#endif

  int                       pixelAxisIndex{ -1 };
  std::vector<unsigned int> imageAxes_nrrd;
  unsigned int              numberOfDomainAxes{ 0 };
  bool                      needPermutation{ false };
  GetAxisOrderForFileReading(
    nrrd, imageAxes_nrrd, pixelAxisIndex, numberOfDomainAxes, needPermutation, this->GetAxesReorder());

  if (needPermutation)
  {
    // the pixel component is not on the fastest axis,
    // so we have to permute axes to put it there, since that is
    // how we set things up in ReadImageInformation() above
    Nrrd *       ntmp = nrrdNew();
    unsigned int axmap[NRRD_DIM_MAX];
    if (pixelAxisIndex >= 0)
    {
      imageAxes_nrrd.insert(imageAxes_nrrd.begin(), pixelAxisIndex);
    }
    for (unsigned int axis = 0; axis < imageAxes_nrrd.size(); ++axis)
    {
      axmap[axis] = imageAxes_nrrd[axis];
    }
    // The memory size of the input and output of nrrdAxesPermute is
    // the same; the existing nrrd->data is re-used.
    if (nrrdCopy(ntmp, nrrd) || nrrdAxesPermute(nrrd, ntmp, axmap))
    {
      char * err = biffGetDone(NRRD); // would be nice to free(err)
      itkExceptionMacro("Read: Error permuting independent axis in " << this->GetFileName() << ":\n" << err);
    }
    nrrdNuke(ntmp);
  }

  if (nrrdAllocated)
  {
    // Now we have to get the data back into the given ITK buffer
    // In any case, the logic here has the luxury of assuming that the
    // *single* non-scalar axis is the *first* (fastest) axis.
    if (nrrdKind3DMaskedSymMatrix == nrrd->axis[0].kind &&
        IOPixelEnum::SYMMETRICSECONDRANKTENSOR == this->GetPixelType())
    {
      // we crop out the mask and put the output in ITK-allocated "buffer"
      size_t size[NRRD_DIM_MAX], minIdx[NRRD_DIM_MAX], maxIdx[NRRD_DIM_MAX];
      for (unsigned int axis = 0; axis < nrrd->dim; ++axis)
      {
        minIdx[axis] = (0 == axis) ? 1 : 0;
        maxIdx[axis] = nrrd->axis[axis].size - 1;
        size[axis] = maxIdx[axis] - minIdx[axis] + 1;
      }
      Nrrd * ntmp = nrrdNew();
      if (nrrdCopy(ntmp, nrrd))
      {
        char * err = biffGetDone(NRRD); // would be nice to free(err)
        itkExceptionMacro("Read: Error copying:\n" << err);
      }
      nrrdEmpty(nrrd);
      if (nrrdWrap_nva(nrrd, buffer, ntmp->type, ntmp->dim, size) || nrrdCrop(nrrd, ntmp, minIdx, maxIdx))
      {
        char * err = biffGetDone(NRRD); // would be nice to free(err)
        itkExceptionMacro("Read: Error wrapping or cropping:\n" << err);
      }
      nrrdNuke(ntmp);
      nrrdNix(nrrd);
    }
    else
    {
      // false alarm; we didn't need to allocate the data ourselves
      memcpy(buffer, nrrd->data, nrrdElementSize(nrrd) * nrrdElementNumber(nrrd));
      nrrdNuke(nrrd);
    }
  }
  else //
  {
    // "buffer" == nrrd->data was ITK-allocated; lose the nrrd struct
    nrrdNix(nrrd);
  }
}

bool
NrrdImageIO::CanWriteFile(const char * name)
{
  const std::string filename = name;

  if (filename.empty())
  {
    return false;
  }

  return this->HasSupportedWriteExtension(name);
}

void
NrrdImageIO::WriteImageInformation()
{
  // Nothing needs doing here.
}


// helper function
template <typename T>
bool
_dump_metadata_to_stream(MetaDataDictionary & thisDic, const std::string & key, std::ostringstream & buffer)
{
  T value;
  if (ExposeMetaData<T>(thisDic, key, value))
  {
    buffer << value;
    return true;
  }
  return false;
}

void
NrrdImageIO::Write(const void * buffer)
{
  Nrrd *        nrrd = nrrdNew();
  NrrdIoState * nio = nrrdIoStateNew();
  int           kind[NRRD_DIM_MAX]{};
  size_t        size[NRRD_DIM_MAX]{};

  double spaceDir[NRRD_DIM_MAX][NRRD_SPACE_DIM_MAX];
  std::fill(&spaceDir[0][0], &spaceDir[0][0] + NRRD_DIM_MAX * NRRD_SPACE_DIM_MAX, AIR_NAN);

  int          pixelAxisIndex_nrrd{ -1 };
  unsigned int numberOfPixelAxis_nrrd{ 0 };
  if (this->GetNumberOfComponents() > 1)
  {
    pixelAxisIndex_nrrd = 0;
    numberOfPixelAxis_nrrd = 1;

    size[pixelAxisIndex_nrrd] = this->GetNumberOfComponents();

    switch (this->GetPixelType())
    {
      case IOPixelEnum::RGB:
        kind[pixelAxisIndex_nrrd] = nrrdKindRGBColor;
        break;
      case IOPixelEnum::RGBA:
        kind[pixelAxisIndex_nrrd] = nrrdKindRGBAColor;
        break;
      case IOPixelEnum::POINT:
        kind[pixelAxisIndex_nrrd] = nrrdKindPoint;
        break;
      case IOPixelEnum::COVARIANTVECTOR:
        kind[pixelAxisIndex_nrrd] = nrrdKindCovariantVector;
        break;
      case IOPixelEnum::SYMMETRICSECONDRANKTENSOR:
      case IOPixelEnum::DIFFUSIONTENSOR3D:
        kind[pixelAxisIndex_nrrd] = nrrdKind3DSymMatrix;
        break;
      case IOPixelEnum::COMPLEX:
        kind[pixelAxisIndex_nrrd] = nrrdKindComplex;
        break;
      case IOPixelEnum::VECTOR:
      case IOPixelEnum::OFFSET:     // HEY is this right?
      case IOPixelEnum::FIXEDARRAY: // HEY is this right?
      default:
        kind[pixelAxisIndex_nrrd] = nrrdKindVector;
        break;
    }
  }

  // Get list dimensions
  unsigned int         numberOfListAxes = 0;
  MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  for (unsigned int axi = 0; axi < this->GetNumberOfDimensions(); ++axi)
  {
    unsigned int axis_itk = axi;
    unsigned int axis_nrrd = numberOfPixelAxis_nrrd + axi;
    std::string  key =
      std::string(NRRD_KEY_PREFIX) + airEnumStr(nrrdField, nrrdField_kinds) + "[" + std::to_string(axis_itk) + "]";
    std::string kindValue;
    ExposeMetaData<std::string>(thisDic, key, kindValue);
    if (!kindValue.compare("list"))
    {
      kind[axis_nrrd] = nrrdKindList;
      ++numberOfListAxes;
    }
  }

  unsigned int spaceDim = this->GetNumberOfDimensions() - numberOfListAxes;

  // Origin is only specified for domain axes
  std::vector<double> originStd;
  for (unsigned int axi = 0; axi < this->GetNumberOfDimensions(); ++axi)
  {
    unsigned int axis_itk = axi;
    unsigned int axis_nrrd = numberOfPixelAxis_nrrd + axi;
    size[axis_nrrd] = this->GetDimensions(axis_itk);
    if (kind[axis_nrrd] == nrrdKindList)
    {
      for (unsigned int saxi = 0; saxi < spaceDim; ++saxi)
      {
        spaceDir[axis_nrrd][saxi] = AIR_NAN;
      }
      continue;
    }
    kind[axis_nrrd] = nrrdKindDomain;
    originStd.push_back(this->GetOrigin(axis_itk));
    double              spacing = this->GetSpacing(axis_itk);
    std::vector<double> spaceDirStd = this->GetDirection(axis_itk);
    for (unsigned int saxi = 0; saxi < spaceDim; ++saxi)
    {
      spaceDir[axis_nrrd][saxi] = spacing * spaceDirStd[saxi];
    }
  }

  unsigned int nrrdDimensions = this->GetNumberOfDimensions() + (pixelAxisIndex_nrrd < 0 ? 0 : 1);
  if (nrrdWrap_nva(
        nrrd, const_cast<void *>(buffer), this->ITKToNrrdComponentType(m_ComponentType), nrrdDimensions, size) ||
      (spaceDim == 3
         // special case: ITK is LPS in 3-D
         ? nrrdSpaceSet(nrrd, nrrdSpaceLeftPosteriorSuperior)
         : nrrdSpaceDimensionSet(nrrd, spaceDim)) ||
      nrrdSpaceOriginSet(nrrd, originStd.data()))
  {
    char * err = biffGetDone(NRRD); // would be nice to free(err)
    itkExceptionMacro("Write: Error wrapping nrrd for " << this->GetFileName() << ":\n" << err);
  }
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoKind, kind);
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoSpaceDirection, spaceDir);

  // Go through MetaDataDictionary and set either specific nrrd field
  // or a key/value pair
  std::vector<std::string>                 keys = thisDic.GetKeys();
  std::vector<std::string>::const_iterator keyIt;
  const char *                             keyField, *field;
  for (keyIt = keys.begin(); keyIt != keys.end(); ++keyIt)
  {
    if (!strncmp(NRRD_KEY_PREFIX.c_str(), keyIt->c_str(), NRRD_KEY_PREFIX.size()))
    {
      keyField = keyIt->c_str() + NRRD_KEY_PREFIX.size();
      // only of one of these can succeed
      field = airEnumStr(nrrdField, nrrdField_thicknesses);
      unsigned int axi{ 0 };
      if (!strncmp(keyField, field, strlen(field)))
      {
        if (1 == sscanf(keyField + strlen(field), "[%u]", &axi) && axi + numberOfPixelAxis_nrrd < nrrd->dim)
        {
          double thickness = 0.0;
          ExposeMetaData<double>(thisDic, *keyIt, thickness);
          nrrd->axis[axi + numberOfPixelAxis_nrrd].thickness = thickness;
        }
      }
      field = airEnumStr(nrrdField, nrrdField_centers);
      if (!strncmp(keyField, field, strlen(field)))
      {
        if (1 == sscanf(keyField + strlen(field), "[%u]", &axi) && axi + numberOfPixelAxis_nrrd < nrrd->dim)
        {
          std::string value;
          ExposeMetaData<std::string>(thisDic, *keyIt, value);
          nrrd->axis[axi + numberOfPixelAxis_nrrd].center = airEnumVal(nrrdCenter, value.c_str());
        }
      }
      field = airEnumStr(nrrdField, nrrdField_kinds);
      if (!strncmp(keyField, field, strlen(field)))
      {
        if (1 == sscanf(keyField + strlen(field), "[%u]", &axi) && axi + numberOfPixelAxis_nrrd < nrrd->dim)
        {
          std::string value;
          ExposeMetaData<std::string>(thisDic, *keyIt, value);
          nrrd->axis[axi + numberOfPixelAxis_nrrd].kind = airEnumVal(nrrdKind, value.c_str());
        }
      }
      field = airEnumStr(nrrdField, nrrdField_labels);
      if (!strncmp(keyField, field, strlen(field)))
      {
        if (1 == sscanf(keyField + strlen(field), "[%u]", &axi) && axi + numberOfPixelAxis_nrrd < nrrd->dim)
        {
          std::string value;
          ExposeMetaData<std::string>(thisDic, *keyIt, value);
          nrrd->axis[axi + numberOfPixelAxis_nrrd].label = airStrdup(value.c_str());
        }
      }
      field = airEnumStr(nrrdField, nrrdField_units);
      if (!strncmp(keyField, field, strlen(field)))
      {
        if (1 == sscanf(keyField + strlen(field), "[%u]", &axi) && axi + numberOfPixelAxis_nrrd < nrrd->dim)
        {
          std::string value;
          ExposeMetaData<std::string>(thisDic, *keyIt, value);
          nrrd->axis[axi + numberOfPixelAxis_nrrd].units = airStrdup(value.c_str());
        }
      }
      field = airEnumStr(nrrdField, nrrdField_old_min);
      if (!strncmp(keyField, field, strlen(field)))
      {
        ExposeMetaData<double>(thisDic, *keyIt, nrrd->oldMin);
      }
      field = airEnumStr(nrrdField, nrrdField_old_max);
      if (!strncmp(keyField, field, strlen(field)))
      {
        ExposeMetaData<double>(thisDic, *keyIt, nrrd->oldMax);
      }

      field = airEnumStr(nrrdField, nrrdField_space);
      if (!strncmp(keyField, field, strlen(field)))
      {
        int         space;
        std::string value;
        ExposeMetaData<std::string>(thisDic, *keyIt, value);
        space = airEnumVal(nrrdSpace, value.c_str());
        if (nrrdSpaceDimension(space) == nrrd->spaceDim)
        {
          // sanity check
          nrrd->space = space;
        }
      }

      field = airEnumStr(nrrdField, nrrdField_content);
      if (!strncmp(keyField, field, strlen(field)))
      {
        std::string value;
        ExposeMetaData<std::string>(thisDic, *keyIt, value);
        nrrd->content = airStrdup(value.c_str());
      }
      field = airEnumStr(nrrdField, nrrdField_measurement_frame);
      if (!strncmp(keyField, field, strlen(field)))
      {
        std::vector<std::vector<double>> msrFrame;
        ExposeMetaData<std::vector<std::vector<double>>>(thisDic, *keyIt, msrFrame);
        for (unsigned int saxi = 0; saxi < nrrd->spaceDim; ++saxi)
        {
          for (unsigned int saxj = 0; saxj < nrrd->spaceDim; ++saxj)
          {
            if (saxi < msrFrame.size() && saxj < msrFrame[saxi].size())
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
      // convert to string and dump to the file
      // const char *tname = thisDic.Get(*keyIt)->GetNameOfClass();
      std::ostringstream dump;
      if (_dump_metadata_to_stream<std::string>(thisDic, *keyIt, dump) ||
          _dump_metadata_to_stream<double>(thisDic, *keyIt, dump) ||
          _dump_metadata_to_stream<float>(thisDic, *keyIt, dump) ||
          _dump_metadata_to_stream<int>(thisDic, *keyIt, dump) ||
          _dump_metadata_to_stream<unsigned int>(thisDic, *keyIt, dump) ||
          _dump_metadata_to_stream<Array<float>>(thisDic, *keyIt, dump) ||
          _dump_metadata_to_stream<Array<double>>(thisDic, *keyIt, dump) ||
          _dump_metadata_to_stream<Array<int>>(thisDic, *keyIt, dump) ||
          _dump_metadata_to_stream<Array<unsigned int>>(thisDic, *keyIt, dump))
        nrrdKeyValueAdd(nrrd, keyIt->c_str(), dump.str().c_str());
    }
  }

  // set encoding for data: compressed (raw), (uncompressed) raw, or ascii
  if (this->GetUseCompression() && this->m_NrrdCompressionEncoding != nullptr &&
      this->m_NrrdCompressionEncoding->available())
  {
    nio->encoding = this->m_NrrdCompressionEncoding;
    nio->zlibLevel = this->GetCompressionLevel();
    // nio->zlibStrategy = default
  }
  else
  {
    const typename Superclass::IOFileEnum fileType = this->GetFileType();
    switch (fileType)
    {
      default:
      case IOFileEnum::TypeNotApplicable:
      case IOFileEnum::Binary:
        nio->encoding = nrrdEncodingRaw;
        break;
      case IOFileEnum::ASCII:
        nio->encoding = nrrdEncodingAscii;
        break;
    }
  }

  // set desired endianness of output
  const typename Superclass::IOByteOrderEnum byteOrder = this->GetByteOrder();
  switch (byteOrder)
  {
    default:
    case IOByteOrderEnum::OrderNotApplicable:
      nio->endian = airEndianUnknown;
      break;
    case IOByteOrderEnum::BigEndian:
      nio->endian = airEndianBig;
      break;
    case IOByteOrderEnum::LittleEndian:
      nio->endian = airEndianLittle;
      break;
  }

  // Write the nrrd to file.
  if (nrrdSave(this->GetFileName(), nrrd, nio))
  {
    char * err = biffGetDone(NRRD); // would be nice to free(err)
    itkExceptionMacro("Write: Error writing " << this->GetFileName() << ":\n" << err);
  }

  // Free the nrrd struct but don't touch nrrd->data
  nrrdNix(nrrd);
  nrrdIoStateNix(nio);
}

} // end namespace itk
