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

#include "itkMetaImageIO.h"
#include "itkSpatialOrientationAdapter.h"
#include "itkMetaDataObject.h"
#include "itkIOCommon.h"
#include "itksys/SystemTools.hxx"
#include "itkMath.h"

namespace itk
{
// Explicitly set std::numeric_limits<double>::max_digits10 this will provide
// better accuracy when writing out floating point number in MetaImage header.
unsigned int MetaImageIO::m_DefaultDoublePrecision = 17;

MetaImageIO::MetaImageIO()
{
  m_FileType = Binary;
  m_SubSamplingFactor = 1;
  if ( MET_SystemByteOrderMSB() )
    {
    m_ByteOrder = BigEndian;
    }
  else
    {
    m_ByteOrder = LittleEndian;
    }

  this->AddSupportedWriteExtension(".mha");
  this->AddSupportedWriteExtension(".mhd");

  this->AddSupportedReadExtension(".mha");
  this->AddSupportedReadExtension(".mhd");
  // set behavior of MetaImageIO independently of the default value in MetaImage
  this->SetDoublePrecision(GetDefaultDoublePrecision());
}

MetaImageIO::~MetaImageIO()
{}

void MetaImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  m_MetaImage.PrintInfo();
  os << indent << "SubSamplingFactor: " << m_SubSamplingFactor << "\n";
}

void MetaImageIO::SetDataFileName(const char *filename)
{
  m_MetaImage.ElementDataFileName(filename);
}

// This method will only test if the header looks like a
// MetaImage.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool MetaImageIO::CanReadFile(const char *filename)
{
  // First check the extension
  std::string fname = filename;

  if (  fname == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    return false;
    }

  return m_MetaImage.CanRead(filename);
}

void MetaImageIO::ReadImageInformation()
{
  if ( !m_MetaImage.Read(m_FileName.c_str(), false) )
    {
    itkExceptionMacro( "File cannot be read: "
                       << this->GetFileName() << " for reading."
                       << std::endl
                       << "Reason: "
                       << itksys::SystemTools::GetLastSystemError() );
    }

  if ( m_MetaImage.BinaryData() )
    {
    this->SetFileType(Binary);
    }
  else
    {
    this->SetFileType(ASCII);
    }

  this->SetNumberOfComponents( m_MetaImage.ElementNumberOfChannels() );

  // Set default value
  this->SetComponentType(UNKNOWNCOMPONENTTYPE);
  itk::MetaDataDictionary & thisMetaDict = this->GetMetaDataDictionary();
  switch ( m_MetaImage.ElementType() )
    {
    default:
    case MET_OTHER:
    case MET_NONE:
      this->SetPixelType(UNKNOWNPIXELTYPE);
      this->SetComponentType(UNKNOWNCOMPONENTTYPE);
      break;
    case MET_CHAR:
    case MET_ASCII_CHAR:
      this->SetPixelType(SCALAR);
      this->SetComponentType(CHAR);
      break;
    case MET_CHAR_ARRAY:
    case MET_STRING:
      this->SetPixelType(VECTOR);
      this->SetComponentType(CHAR);
      break;
    case MET_UCHAR:
      this->SetPixelType(SCALAR);
      this->SetComponentType(UCHAR);
      break;
    case MET_UCHAR_ARRAY:
      this->SetPixelType(VECTOR);
      this->SetComponentType(UCHAR);
      break;
    case MET_SHORT:
      this->SetPixelType(SCALAR);
      this->SetComponentType(SHORT);
      break;
    case MET_SHORT_ARRAY:
      this->SetPixelType(VECTOR);
      this->SetComponentType(SHORT);
      break;
    case MET_USHORT:
      this->SetPixelType(SCALAR);
      this->SetComponentType(USHORT);
      break;
    case MET_USHORT_ARRAY:
      this->SetPixelType(VECTOR);
      this->SetComponentType(USHORT);
      break;
    case MET_INT:
      this->SetPixelType(SCALAR);
      if ( sizeof( int ) == MET_ValueTypeSize[MET_INT] )
        {
        this->SetComponentType(INT);
        }
      else if ( sizeof( long ) == MET_ValueTypeSize[MET_INT] )
        {
        this->SetComponentType(LONG);
        }
      break;
    case MET_INT_ARRAY:
      this->SetPixelType(VECTOR);
      if ( sizeof( int ) == MET_ValueTypeSize[MET_INT] )
        {
        this->SetComponentType(INT);
        }
      else if ( sizeof( long ) == MET_ValueTypeSize[MET_INT] )
        {
        this->SetComponentType(LONG);
        }
      break;
    case MET_UINT:
      this->SetPixelType(SCALAR);
      if ( sizeof( unsigned int ) == MET_ValueTypeSize[MET_UINT] )
        {
        this->SetComponentType(UINT);
        }
      else if ( sizeof( unsigned long ) == MET_ValueTypeSize[MET_UINT] )
        {
        this->SetComponentType(ULONG);
        }
      break;
    case MET_UINT_ARRAY:
      this->SetPixelType(VECTOR);
      if ( sizeof( int ) == MET_ValueTypeSize[MET_INT] )
        {
        this->SetComponentType(UINT);
        }
      else if ( sizeof( long ) == MET_ValueTypeSize[MET_INT] )
        {
        this->SetComponentType(ULONG);
        }
      break;
    case MET_LONG:
      this->SetPixelType(SCALAR);
      if ( sizeof( long ) == MET_ValueTypeSize[MET_LONG] )
        {
        this->SetComponentType(LONG);
        }
      else if ( sizeof( int ) == MET_ValueTypeSize[MET_LONG] )
        {
        this->SetComponentType(INT);
        }
      break;
    case MET_LONG_ARRAY:
      this->SetPixelType(VECTOR);
      if ( sizeof( long ) == MET_ValueTypeSize[MET_LONG] )
        {
        this->SetComponentType(LONG);
        }
      else if ( sizeof( int ) == MET_ValueTypeSize[MET_LONG] )
        {
        this->SetComponentType(INT);
        }
      break;
    case MET_ULONG:
      this->SetPixelType(SCALAR);
      if ( sizeof( unsigned long ) == MET_ValueTypeSize[MET_ULONG] )
        {
        this->SetComponentType(ULONG);
        }
      else if ( sizeof( unsigned int ) == MET_ValueTypeSize[MET_ULONG] )
        {
        this->SetComponentType(UINT);
        }
      break;
    case MET_ULONG_ARRAY:
      this->SetPixelType(VECTOR);
      if ( sizeof( unsigned long ) == MET_ValueTypeSize[MET_ULONG] )
        {
        this->SetComponentType(ULONG);
        }
      else if ( sizeof( unsigned int ) == MET_ValueTypeSize[MET_ULONG] )
        {
        this->SetComponentType(UINT);
        }
      break;
    case MET_LONG_LONG:
      this->SetPixelType(SCALAR);
      if ( sizeof( long long ) == MET_ValueTypeSize[MET_LONG_LONG] )
        {
        this->SetComponentType(LONGLONG);
        }
      else if ( sizeof( int ) == MET_ValueTypeSize[MET_LONG_LONG] )
        {
        this->SetComponentType(INT);
        }
      else if ( sizeof( long ) == MET_ValueTypeSize[MET_LONG_LONG] )
        {
        this->SetComponentType(LONG);
        }
      break;
    case MET_LONG_LONG_ARRAY:
      this->SetPixelType(VECTOR);
      if ( sizeof( long long ) == MET_ValueTypeSize[MET_LONG_LONG] )
        {
        this->SetComponentType(LONGLONG);
        }
      else if ( sizeof( int ) == MET_ValueTypeSize[MET_LONG_LONG] )
        {
        this->SetComponentType(INT);
        }
      else if ( sizeof( long ) == MET_ValueTypeSize[MET_LONG_LONG] )
        {
        this->SetComponentType(LONG);
        }
      break;
    case MET_ULONG_LONG:
      this->SetPixelType(SCALAR);
      if ( sizeof( unsigned long long ) == MET_ValueTypeSize[MET_ULONG_LONG] )
        {
        this->SetComponentType(ULONGLONG);
        }
      else if ( sizeof( unsigned int ) == MET_ValueTypeSize[MET_ULONG_LONG] )
        {
        this->SetComponentType(UINT);
        }
      else if ( sizeof( unsigned long ) == MET_ValueTypeSize[MET_ULONG_LONG] )
        {
        this->SetComponentType(ULONG);
        }
      break;
    case MET_ULONG_LONG_ARRAY:
      this->SetPixelType(VECTOR);
      if ( sizeof( unsigned long long ) == MET_ValueTypeSize[MET_ULONG_LONG] )
        {
        this->SetComponentType(ULONGLONG);
        }
      else if ( sizeof( unsigned int ) == MET_ValueTypeSize[MET_ULONG_LONG] )
        {
        this->SetComponentType(UINT);
        }
      else if ( sizeof( unsigned long ) == MET_ValueTypeSize[MET_ULONG_LONG] )
        {
        this->SetComponentType(ULONG);
        }
      break;
    case MET_FLOAT:
      this->SetPixelType(SCALAR);
      if ( sizeof( float ) == MET_ValueTypeSize[MET_FLOAT] )
        {
        this->SetComponentType(FLOAT);
        }
      else if ( sizeof( double ) == MET_ValueTypeSize[MET_FLOAT] )
        {
        this->SetComponentType(DOUBLE);
        }
      break;
    case MET_FLOAT_ARRAY:
      this->SetPixelType(VECTOR);
      if ( sizeof( float ) == MET_ValueTypeSize[MET_FLOAT] )
        {
        this->SetComponentType(FLOAT);
        }
      else if ( sizeof( double ) == MET_ValueTypeSize[MET_FLOAT] )
        {
        this->SetComponentType(DOUBLE);
        }
      break;
    case MET_DOUBLE:
      this->SetPixelType(SCALAR);
      this->SetComponentType(DOUBLE);
      if ( sizeof( double ) == MET_ValueTypeSize[MET_DOUBLE] )
        {
        this->SetComponentType(DOUBLE);
        }
      else if ( sizeof( float ) == MET_ValueTypeSize[MET_DOUBLE] )
        {
        this->SetComponentType(FLOAT);
        }
      break;
    case MET_DOUBLE_ARRAY:
      this->SetPixelType(VECTOR);
      if ( sizeof( double ) == MET_ValueTypeSize[MET_DOUBLE] )
        {
        this->SetComponentType(DOUBLE);
        }
      else if ( sizeof( float ) == MET_ValueTypeSize[MET_DOUBLE] )
        {
        this->SetComponentType(FLOAT);
        }
      break;
    case MET_FLOAT_MATRIX:
      this->SetPixelType(VECTOR);
      if ( sizeof( float ) == MET_ValueTypeSize[MET_FLOAT] )
        {
        this->SetComponentType(FLOAT);
        }
      else if ( sizeof( double ) == MET_ValueTypeSize[MET_FLOAT] )
        {
        this->SetComponentType(DOUBLE);
        }
      this->SetNumberOfComponents(m_NumberOfComponents * m_NumberOfComponents);
      break;
    }

  // BUG: 8732
  // The above use to MET_*_ARRAY may not be correct, as this MetaIO
  // ElementType was not designed to indicate vectors, but something
  // else
  //
  // if the file has multiple components then we default to a vector
  // pixel type, support could be added to MetaIO format to define
  // different pixel types
  if ( m_MetaImage.ElementNumberOfChannels() > 1 )
    {
    this->SetPixelType(VECTOR);
    }

  this->SetNumberOfDimensions( m_MetaImage.NDims() );

  unsigned int i;
  for ( i = 0; i < m_NumberOfDimensions; i++ )
    {
    this->SetDimensions(i, m_MetaImage.DimSize(i) / m_SubSamplingFactor);
    this->SetSpacing(i, m_MetaImage.ElementSpacing(i) * m_SubSamplingFactor);
    this->SetOrigin( i, m_MetaImage.Position(i) );
    }

  //
  // Read direction cosines
  //
  const double *       transformMatrix = m_MetaImage.TransformMatrix();
  vnl_vector< double > directionAxis( this->GetNumberOfDimensions() );
  for ( unsigned int ii = 0; ii < this->GetNumberOfDimensions(); ii++ )
    {
    for ( unsigned int jj = 0; jj < this->GetNumberOfDimensions(); jj++ )
      {
      directionAxis[jj] = transformMatrix[ii * this->GetNumberOfDimensions() + jj];
      }
    this->SetDirection(ii, directionAxis);
    }

  std::string classname( this->GetNameOfClass() );
  EncapsulateMetaData< std::string >(thisMetaDict, ITK_InputFilterName,
                                     classname);
  //
  // save the metadatadictionary in the MetaImage header.
  // NOTE: The MetaIO library only supports typeless strings as metadata
  int dictFields = m_MetaImage.GetNumberOfAdditionalReadFields();
  for ( int f = 0; f < dictFields; f++ )
    {
    std::string key( m_MetaImage.GetAdditionalReadFieldName(f) );
    std::string value ( m_MetaImage.GetAdditionalReadFieldValue(f) );
    EncapsulateMetaData< std::string >( thisMetaDict,key,value );
    }

  //
  // Read some metadata
  //
  MetaDataDictionary & metaDict = this->GetMetaDataDictionary();

  // Look at default metaio fields
  if ( m_MetaImage.DistanceUnits() != MET_DISTANCE_UNITS_UNKNOWN )
    {
    EncapsulateMetaData< std::string >(
      metaDict, ITK_VoxelUnits, std::string( m_MetaImage.DistanceUnitsName() ) );
    }

  if ( strlen( m_MetaImage.AcquisitionDate() ) > 0 )
    {
    EncapsulateMetaData< std::string >(
      metaDict, ITK_ExperimentDate, std::string( m_MetaImage.AcquisitionDate() ) );
    }
}

void MetaImageIO::Read(void *buffer)
{
  const unsigned int nDims = this->GetNumberOfDimensions();

  // this will check to see if we are actually streaming
  // we initialize with the dimensions of the file, since if
  // largestRegion and ioRegion don't match, we'll use the streaming
  // path since the comparison will fail
  ImageIORegion largestRegion(nDims);

  for ( unsigned int i = 0; i < nDims; i++ )
    {
    largestRegion.SetIndex(i, 0);
    largestRegion.SetSize( i, this->GetDimensions(i) );
    }

  if ( largestRegion != m_IORegion )
    {
    int *indexMin = new int[nDims];
    int *indexMax = new int[nDims];
    for ( unsigned int i = 0; i < nDims; i++ )
      {
      if ( i < m_IORegion.GetImageDimension() )
        {
        indexMin[i] = m_IORegion.GetIndex()[i];
        indexMax[i] = indexMin[i] + m_IORegion.GetSize()[i] - 1;
        }
      else
        {
        indexMin[i] = 0;
        // this is zero since this is a (size - 1)
        indexMax[i] = 0;
        }
      }

    if ( !m_MetaImage.ReadROI(indexMin, indexMax,
                              m_FileName.c_str(), true, buffer,
                              m_SubSamplingFactor) )
      {
      delete[] indexMin;
      delete[] indexMax;
      itkExceptionMacro( "File cannot be read: "
                         << this->GetFileName() << " for reading."
                         << std::endl
                         << "Reason: "
                         << itksys::SystemTools::GetLastSystemError() );
      }

    delete[] indexMin;
    delete[] indexMax;

    m_MetaImage.ElementByteOrderFix( m_IORegion.GetNumberOfPixels() );
    }
  else
    {
    if ( !m_MetaImage.Read(m_FileName.c_str(), true, buffer) )
      {
      itkExceptionMacro( "File cannot be read: "
                         << this->GetFileName() << " for reading."
                         << std::endl
                         << "Reason: "
                         << itksys::SystemTools::GetLastSystemError() );
      }

    // since we are not streaming m_IORegion may not be set, so
    m_MetaImage.ElementByteOrderFix( this->GetImageSizeInPixels() );
    }
}

MetaImage * MetaImageIO::GetMetaImagePointer(void)
{
  return &m_MetaImage;
}

bool MetaImageIO::CanWriteFile(const char *name)
{
  std::string filename = name;

  if (  filename == "" )
    {
    return false;
    }

  std::string::size_type mhaPos = filename.rfind(".mha");
  if ( ( mhaPos != std::string::npos )
       && ( mhaPos == filename.length() - 4 ) )
    {
    return true;
    }

  std::string::size_type mhdPos = filename.rfind(".mhd");
  if ( ( mhdPos != std::string::npos )
       && ( mhdPos == filename.length() - 4 ) )
    {
    return true;
    }

  return false;
}

void
MetaImageIO
::WriteImageInformation(void)
{

  MetaDataDictionary & metaDict = this->GetMetaDataDictionary();
  std::string          metaDataStr;

  // Look at default metaio fields
  if ( ExposeMetaData< std::string >(metaDict, ITK_VoxelUnits, metaDataStr) )
    {
    // Handle analyze style unit string
    if ( metaDataStr == "um. " )
      {
      m_MetaImage.DistanceUnits(MET_DISTANCE_UNITS_UM);
      }
    else if ( metaDataStr == "mm. " )
      {
      m_MetaImage.DistanceUnits(MET_DISTANCE_UNITS_MM);
      }
    else if ( metaDataStr == "cm. " )
      {
      m_MetaImage.DistanceUnits(MET_DISTANCE_UNITS_CM);
      }
    else
      {
      m_MetaImage.DistanceUnits( metaDataStr.c_str() );
      }
    }

  if ( ExposeMetaData< std::string >(metaDict, ITK_ExperimentDate, metaDataStr) )
    {
    m_MetaImage.AcquisitionDate( metaDataStr.c_str() );
    }

  // Save out the metadatadictionary key/value pairs as part of
  // the metaio header.
  std::vector< std::string > keys = metaDict.GetKeys();
  std::vector< std::string >::const_iterator keyIt;
  for ( keyIt = keys.begin(); keyIt != keys.end(); ++keyIt )
    {
    if(*keyIt == ITK_ExperimentDate ||
       *keyIt == ITK_VoxelUnits)
      {
      continue;
      }
    // try for common scalar types
    std::ostringstream strs;
    double dval=0.0;
    float fval=0.0F;
    long lval=0L;
    unsigned long ulval=0L;
    long long llval=0LL;
    unsigned long long ullval=0uLL;
    int ival=0;
    unsigned uval=0;
    short shval=0;
    unsigned short ushval=0;
    char cval=0;
    unsigned char ucval=0;
    bool bval=false;
    std::string value="";
    if(ExposeMetaData< std::string >(metaDict, *keyIt, value))
      {
      strs << value;
      }
    else if(ExposeMetaData<double>(metaDict,*keyIt,dval))
      {
      strs << dval;
      }
    else if(ExposeMetaData<float>(metaDict,*keyIt,fval))
      {
      strs << fval;
      }
    else if(ExposeMetaData<long>(metaDict,*keyIt,lval))
      {
      strs << lval;
      }
    else if(ExposeMetaData<unsigned long>(metaDict,*keyIt,ulval))
      {
      strs << ulval;
      }
    else if(ExposeMetaData<long long>(metaDict,*keyIt,llval))
      {
      strs << llval;
      }
    else if(ExposeMetaData<unsigned long long>(metaDict,*keyIt,ullval))
      {
      strs << ullval;
      }
    else if(ExposeMetaData<int>(metaDict,*keyIt,ival))
      {
      strs << ival;
      }
    else if(ExposeMetaData<unsigned int>(metaDict,*keyIt,uval))
      {
      strs << uval;
      }
    else if(ExposeMetaData<short>(metaDict,*keyIt,shval))
      {
      strs << shval;
      }
    else if(ExposeMetaData<unsigned short>(metaDict,*keyIt,ushval))
      {
      strs << ushval;
      }
    else if(ExposeMetaData<char>(metaDict,*keyIt,cval))
      {
      strs << cval;
      }
    else if(ExposeMetaData<unsigned char>(metaDict,*keyIt,ucval))
      {
      strs << ucval;
      }
    else if(ExposeMetaData<bool>(metaDict,*keyIt,bval))
      {
      strs << bval;
      }

    value = strs.str();

    if (value == "" )
      {
      // if the value is an empty string then the resulting entry in
      // the header will not be able to be read the the metaIO
      // library, which results is a unreadable/corrupt file.
      itkWarningMacro("Unsupported or empty metaData item "
                      << *keyIt << " of type "
                      << metaDict[*keyIt]->GetMetaDataObjectTypeName()
                      << "found, won't be written to image file");

      // so this entry should be skipped.
      continue;
      }

    // Rolling this back out so that the tests pass.
    // The meta image AddUserField requires control of the memory space.
    m_MetaImage.AddUserField( (*keyIt).c_str(), MET_STRING, static_cast<int>( value.size() ), value.c_str(), true, -1 );
    }

}

/**
 *
 */
void
MetaImageIO
::Write(const void *buffer)
{
  const unsigned int numberOfDimensions = this->GetNumberOfDimensions();

  bool binaryData = true;

  if ( this->GetFileType() == ASCII )
    {
    binaryData = false;
    }

  int nChannels = this->GetNumberOfComponents();

  MET_ValueEnumType eType = MET_OTHER;
  switch ( m_ComponentType )
    {
    default:
    case UNKNOWNCOMPONENTTYPE:
      eType = MET_OTHER;
      break;
    case CHAR:
      eType = MET_CHAR;
      break;
    case UCHAR:
      eType = MET_UCHAR;
      break;
    case SHORT:
      eType = MET_SHORT;
      break;
    case USHORT:
      eType = MET_USHORT;
      break;
    case LONG:
      if ( sizeof( long ) == MET_ValueTypeSize[MET_LONG] )
        {
        eType = MET_LONG;
        }
      else if ( sizeof( long ) == MET_ValueTypeSize[MET_INT] )
        {
        eType = MET_INT;
        }
      else if ( sizeof( long ) == MET_ValueTypeSize[MET_LONG_LONG] )
        {
        eType = MET_LONG_LONG;
        }
      break;
    case ULONG:
      if ( sizeof( long ) == MET_ValueTypeSize[MET_LONG] )
        {
        eType = MET_ULONG;
        }
      else if ( sizeof( long ) == MET_ValueTypeSize[MET_INT] )
        {
        eType = MET_UINT;
        }
      else if ( sizeof( long ) == MET_ValueTypeSize[MET_LONG_LONG] )
        {
        eType = MET_ULONG_LONG;
        }
      break;
    case LONGLONG:

      if ( sizeof( long long ) == MET_ValueTypeSize[MET_LONG_LONG] )
        {
        eType = MET_LONG_LONG;
        }
      break;
    case ULONGLONG:
      if ( sizeof( long long) == MET_ValueTypeSize[MET_ULONG_LONG] )
        {
        eType = MET_ULONG_LONG;
        }
      break;
    case INT:
      eType = MET_INT;
      if ( sizeof( int ) == MET_ValueTypeSize[MET_INT] )
        {
        eType = MET_INT;
        }
      else if ( sizeof( int ) == MET_ValueTypeSize[MET_LONG] )
        {
        eType = MET_LONG;
        }
      break;
    case UINT:
      if ( sizeof( int ) == MET_ValueTypeSize[MET_INT] )
        {
        eType = MET_UINT;
        }
      else if ( sizeof( int ) == MET_ValueTypeSize[MET_LONG] )
        {
        eType = MET_ULONG;
        }
      break;
    case FLOAT:
      if ( sizeof( float ) == MET_ValueTypeSize[MET_FLOAT] )
        {
        eType = MET_FLOAT;
        }
      else if ( sizeof( float ) == MET_ValueTypeSize[MET_DOUBLE] )
        {
        eType = MET_DOUBLE;
        }
      break;
    case DOUBLE:
      if ( sizeof( double ) == MET_ValueTypeSize[MET_DOUBLE] )
        {
        eType = MET_DOUBLE;
        }
      else if ( sizeof( double ) == MET_ValueTypeSize[MET_FLOAT] )
        {
        eType = MET_FLOAT;
        }
      break;
    }

  int *        dSize = new int[numberOfDimensions];
  double *     eSpacing = new double[numberOfDimensions];
  double *     eOrigin = new double[numberOfDimensions];
  for ( unsigned int ii = 0; ii < numberOfDimensions; ++ii )
    {
    dSize[ii] = this->GetDimensions(ii);
    eSpacing[ii] = this->GetSpacing(ii);
    eOrigin[ii] = this->GetOrigin(ii);
    }

  m_MetaImage.InitializeEssential( numberOfDimensions, dSize, eSpacing, eType, nChannels,
                                   const_cast< void * >( buffer ) );
  m_MetaImage.Position(eOrigin);
  m_MetaImage.BinaryData(binaryData);

  //Write the image Information
  this->WriteImageInformation();

  if ( numberOfDimensions == 3 )
    {
    SpatialOrientation::ValidCoordinateOrientationFlags coordOrient =
      SpatialOrientation::ITK_COORDINATE_ORIENTATION_INVALID;
    std::vector< double > dirx, diry, dirz;
    SpatialOrientationAdapter::DirectionType dir;
    dirx = this->GetDirection(0);
    diry = this->GetDirection(1);
    dirz = this->GetDirection(2);
    for ( unsigned ii = 0; ii < 3; ii++ )
      {
      dir[ii][0] = dirx[ii];
      dir[ii][1] = diry[ii];
      dir[ii][2] = dirz[ii];
      }
    coordOrient = SpatialOrientationAdapter().FromDirectionCosines(dir);

    switch ( coordOrient )
      {
      default:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
        {
        m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_RL);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
        {
        m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_LR);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
        {
        m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_AP);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
        {
        m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_PA);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
        {
        m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_IS);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
        {
        m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_SI);
        break;
        }
      }
    switch ( coordOrient )
      {
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
        {
        m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_RL);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
        {
        m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_LR);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
        {
        m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_AP);
        break;
        }
      default:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
        {
        m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_PA);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
        {
        m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_IS);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
        {
        m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_SI);
        break;
        }
      }
    switch ( coordOrient )
      {
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
        {
        m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_RL);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
        {
        m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_LR);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
        {
        m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_AP);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
        {
        m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_PA);
        break;
        }
      default:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
        {
        m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_IS);
        break;
        }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
        {
        m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_SI);
        break;
        }
      }
    }
  // Propagage direction cosine information.
  double *transformMatrix = static_cast< double * >( malloc( numberOfDimensions * numberOfDimensions * sizeof( double ) ) );
  if (transformMatrix)
    {
    for ( unsigned int ii = 0; ii < numberOfDimensions; ++ii )
      {
      for ( unsigned int jj = 0; jj < numberOfDimensions; ++jj )
        {
        transformMatrix[ii * numberOfDimensions + jj] =
          this->GetDirection(ii)[jj];
        }
      }
    m_MetaImage.TransformMatrix(transformMatrix);
    free(transformMatrix);
    }

  m_MetaImage.CompressedData(m_UseCompression);

  // this is a check to see if we are actually streaming
  // we initialize with m_IORegion to match dimensions
  ImageIORegion largestRegion(m_IORegion);
  for ( unsigned int ii = 0; ii < numberOfDimensions; ++ii )
    {
    largestRegion.SetIndex( ii, 0 );
    largestRegion.SetSize( ii, this->GetDimensions(ii) );
    }

  if ( m_UseCompression && ( largestRegion != m_IORegion ) )
    {
    std::cout << "Compression in use: cannot stream the file writing" << std::endl;
    }
  else if (  largestRegion != m_IORegion )
    {
    int *indexMin = new int[numberOfDimensions];
    int *indexMax = new int[numberOfDimensions];
    for ( unsigned int ii = 0; ii < numberOfDimensions; ++ii )
      {
      // the dimensions of m_IORegion should match out requested
      // dimensions, but ImageIORegion will throw an
      // exception if out of bounds
      indexMin[ii] = m_IORegion.GetIndex()[ii];
      indexMax[ii] = m_IORegion.GetIndex()[ii] + m_IORegion.GetSize()[ii] - 1;
      }

    if ( !m_MetaImage.WriteROI( indexMin, indexMax, m_FileName.c_str() ) )
      {
      delete[] dSize;
      delete[] eSpacing;
      delete[] eOrigin;
      delete[] indexMin;
      delete[] indexMax;
      itkExceptionMacro( "File ROI cannot be written: "
                         << this->GetFileName()
                         << std::endl
                         << "Reason: "
                         << itksys::SystemTools::GetLastSystemError() );
      }

    delete[] indexMin;
    delete[] indexMax;
    }
  else
    {
    if ( !m_MetaImage.Write( m_FileName.c_str() ) )
      {
      delete[] dSize;
      delete[] eSpacing;
      delete[] eOrigin;
      itkExceptionMacro( "File cannot be written: "
                         << this->GetFileName()
                         << std::endl
                         << "Reason: "
                         << itksys::SystemTools::GetLastSystemError() );
      }
    }

  delete[] dSize;
  delete[] eSpacing;
  delete[] eOrigin;
}

/** Given a requested region, determine what could be the region that we can
 * read from the file. This is called the streamable region, which will be
 * smaller than the LargestPossibleRegion and greater or equal to the
 * RequestedRegion */
ImageIORegion
MetaImageIO
::GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requestedRegion) const
{
  //
  // The default implementations determines that the streamable region is
  // equal to the largest possible region of the image.
  //
  ImageIORegion streamableRegion(this->m_NumberOfDimensions);

  if ( !m_UseStreamedReading )
    {
    for ( unsigned int i = 0; i < this->m_NumberOfDimensions; i++ )
      {
      streamableRegion.SetSize(i, this->m_Dimensions[i]);
      streamableRegion.SetIndex(i, 0);
      }
    }
  else
    {
    streamableRegion = requestedRegion;
    }

  return streamableRegion;
}

unsigned int
MetaImageIO::GetActualNumberOfSplitsForWriting(unsigned int numberOfRequestedSplits,
                                               const ImageIORegion & pasteRegion,
                                               const ImageIORegion & largestPossibleRegion)
{
  if ( this->GetUseCompression() )
    {
    // we can not stream or paste with compression
    if ( pasteRegion != largestPossibleRegion )
      {
      itkExceptionMacro( "Pasting and compression is not supported! Can't write:" << this->GetFileName() );
      }
    else if ( numberOfRequestedSplits != 1 )
      {
      itkDebugMacro("Requested streaming and compression");
      itkDebugMacro("Meta IO is not streaming now!");
      }
    return 1;
    }

  if ( !itksys::SystemTools::FileExists( m_FileName.c_str() ) )
    {
    // file doesn't exits so we don't have potential problems
    }
  else if ( pasteRegion != largestPossibleRegion )
    {
    // we are going to be pasting (may be streaming too)

    // need to check to see if the file is compatible
    std::string errorMessage;
    Pointer     headerImageIOReader = Self::New();

    try
      {
      headerImageIOReader->SetFileName( m_FileName.c_str() );
      headerImageIOReader->ReadImageInformation();
      }
    catch ( ... )
      {
      errorMessage = "Unable to read information from file: " + m_FileName;
      }

    // we now need to check that the following match:
    // 1)file is not compressed
    // 2)pixel type
    // 3)dimensions
    // 4)size/origin/spacing
    // 5)direction cosines
    //

    if ( errorMessage.size() )
      {
      // 0) Can't read file
      }
    // 1)file is not compressed
    else if ( headerImageIOReader->m_MetaImage.CompressedData() )
      {
      errorMessage = "File is compressed: " + m_FileName;
      }
    // 2)pixel type
    // this->GetPixelType() is not verified because the metaio file format
    // stores all multi-component types as arrays, so it does not
    // distinguish between pixel types. Also as long as the compoent
    // and number of compoents match we should be able to paste, that
    // is the numbers should be the same it is just the interpretation
    // that is not matching
    else if ( headerImageIOReader->GetNumberOfComponents() != this->GetNumberOfComponents()
              || headerImageIOReader->GetComponentType() != this->GetComponentType() )
      {
      errorMessage = "Component type does not match in file: " + m_FileName;
      }
    // 3)dimensions/size
    else if ( headerImageIOReader->GetNumberOfDimensions() != this->GetNumberOfDimensions() )
      {
      errorMessage = "Dimensions does not match in file: " + m_FileName;
      }
    else
      {
      for ( unsigned int i = 0; i < this->GetNumberOfDimensions(); ++i )
        {
        // 4)size/origin/spacing
        if ( headerImageIOReader->GetDimensions(i) != this->GetDimensions(i)
             || Math::NotExactlyEquals(headerImageIOReader->GetSpacing(i), this->GetSpacing(i))
             || Math::NotExactlyEquals(headerImageIOReader->GetOrigin(i), this->GetOrigin(i)) )
          {
          errorMessage = "Size, spacing or origin does not match in file: " + m_FileName;
          break;
          }
        // 5)direction cosines
        if ( headerImageIOReader->GetDirection(i) != this->GetDirection(i) )
          {
          errorMessage = "Direction cosines does not match in file: " + m_FileName;
          break;
          }
        }
      }

    if ( errorMessage.size() )
      {
      itkExceptionMacro("Unable to paste because pasting file exists and is different. " << errorMessage);
      }
    else if ( headerImageIOReader->GetPixelType() != this->GetPixelType() )
      {
      // since there is currently poor support for pixel types in
      // MetaIO we will just warn when it does not match
      itkWarningMacro("Pixel types does not match file, but component type and number of components do.");
      }
    }
  else if ( numberOfRequestedSplits != 1 )
    {
    // we are going be streaming

    // need to remove the file incase the file doesn't match our
    // current header/meta data information
    if ( !itksys::SystemTools::RemoveFile( m_FileName.c_str() ) )
      {
      itkExceptionMacro("Unable to remove file for streaming: " << m_FileName);
      }
    }

  return GetActualNumberOfSplitsForWritingCanStreamWrite(numberOfRequestedSplits, pasteRegion);
}

ImageIORegion
MetaImageIO::GetSplitRegionForWriting( unsigned int ithPiece,
                                       unsigned int numberOfActualSplits,
                                       const ImageIORegion & pasteRegion,
                                       const ImageIORegion & itkNotUsed(largestPossibleRegion) )
{
  return GetSplitRegionForWritingCanStreamWrite(ithPiece, numberOfActualSplits, pasteRegion);
}

void MetaImageIO::SetDefaultDoublePrecision(unsigned int precision)
{
  m_DefaultDoublePrecision = precision;
}

unsigned int MetaImageIO::GetDefaultDoublePrecision()
{
  return m_DefaultDoublePrecision;
}

} // end namespace itk
