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
#include "itkVTKImageIO.h"
#include "itkByteSwapper.h"

#include "itksys/SystemTools.hxx"


namespace itk
{
VTKImageIO::VTKImageIO()
{
  this->SetNumberOfDimensions(2);
  m_ByteOrder = LittleEndian;
  m_FileType = Binary;
  m_HeaderSize = 0;

  this->AddSupportedReadExtension(".vtk");

  this->AddSupportedWriteExtension(".vtk");
}

VTKImageIO::~VTKImageIO()
{}

int VTKImageIO::GetNextLine(std::ifstream& ifs, std::string& line, bool lowerCase, SizeValueType count)
{
  // The terminal condition for this recursive calls
  if(count >5)
    {
    itkExceptionMacro(<<"Error of GetNextLine due to consecutive 5 empty lines in the given .*vtk file ");
    }

  // Get a next line from a given *.vtk file
  std::getline(ifs, line);

  // Check the End-of-File of the file
  if(ifs.eof())
    {
    itkExceptionMacro(<<"Premature EOF in reading a line");
    }

  // Convert characters of the line to lowercas
  if(lowerCase)
    {
    std::transform(line.begin(), line.end(), line.begin(), ::tolower);
    }

  // Check an empty line with the size of a read line from *.vtk file
  if(line.size() < 1)
    {
    return GetNextLine(ifs, line, lowerCase, ++count);
    }

  return 1;
}

bool VTKImageIO::CanReadFile(const char *filename)
{
  std::ifstream file;
  std::string   fname(filename);

  if ( fname.find(".vtk") >= fname.length() )
    {
    return false;
    }

  try
    {
    this->OpenFileForReading( file, fname );
    }
  catch ( ... )
    {
    return false;
    }

  // Check to see if its a vtk structured points file
  this->GetNextLine(file, fname);
  this->GetNextLine(file, fname);
  this->GetNextLine(file, fname);
  this->GetNextLine(file, fname);

  if ( fname.find("structured_points") < fname.length() )
    {
    return true;
    }
  else
    {
    return false;
    }
}


bool VTKImageIO::CanStreamRead(void)
{
  bool canStreamRead = true;
  canStreamRead &= this->GetPixelType() != ImageIOBase::SYMMETRICSECONDRANKTENSOR;
  canStreamRead &= this->GetFileType() != ASCII;
  return canStreamRead;
}

bool VTKImageIO::CanStreamWrite(void)
{
  bool canStreamWrite = true;
  canStreamWrite &= this->GetPixelType() != ImageIOBase::SYMMETRICSECONDRANKTENSOR;
  canStreamWrite &= this->GetFileType() != ASCII;
  return canStreamWrite;
}

void VTKImageIO::SetPixelTypeFromString(const std::string & pixelType)
{
  ImageIOBase::IOComponentType compType = GetComponentTypeFromString(pixelType);
  if (compType == UNKNOWNCOMPONENTTYPE)
    {
    if ( pixelType.find("vtktypeuint64") < pixelType.length() )
      {
      SetComponentType(ULONGLONG);
      }
    else if ( pixelType.find("vtktypeint64") < pixelType.length() )
      {
      SetComponentType(LONGLONG);
      }
    else
      {
      itkExceptionMacro(<< "Unrecognized pixel type");
      }
    }
  else
    {
    SetComponentType(compType);
    }
}

std::string VTKImageIO::GetComponentTypeAsString(IOComponentType t)
{
  if (t == ULONGLONG)
    {
    return "vtktypeuint64";
    }
  else if (t == LONGLONG)
    {
    return "vtktypeint64";
    }
  return ImageIOBase::GetComponentTypeAsString(t);
}

void VTKImageIO::InternalReadImageInformation(std::ifstream & file)
{
  std::string text;

  this->OpenFileForReading( file, m_FileName );

  this->GetNextLine(file, text);
  this->GetNextLine(file, text);
  this->GetNextLine(file, text);

  if ( text.find("ascii") < text.length() )
    {
    this->SetFileTypeToASCII();
    }
  else if ( text.find("binary") < text.length() )
    {
    this->SetFileTypeToBinary();
    }
  else
    {
    itkExceptionMacro(<< "Unrecognized type");
    }

  this->GetNextLine(file, text);

  if ( text.find("structured_points") >= text.length() )
    {
    itkExceptionMacro(<< "Not structured points, can't read");
    }

  this->GetNextLine(file, text);

  this->SetNumberOfDimensions(3);
  this->SetSpacing(0, 1.0);
  this->SetSpacing(1, 1.0);
  this->SetSpacing(2, 1.0);
  this->SetOrigin(0, 0.0);
  this->SetOrigin(1, 0.0);
  this->SetOrigin(2, 0.0);

  if ( text.find("dimensions") < text.length() )
    {
    unsigned int dims[3];
    sscanf(text.c_str(), "%*s %u %u %u", dims, dims + 1, dims + 2);
    if ( dims[1] <= 1 && dims[2] <= 1 )
      {
      this->SetNumberOfDimensions(2);
      }
    if ( dims[2] <= 1 )
      {
      this->SetNumberOfDimensions(2);
      }
    else
      {
      this->SetNumberOfDimensions(3);
      }
    for ( unsigned int i = 0; i < this->GetNumberOfDimensions(); i++ )
      {
      this->SetDimensions(i, dims[i]);
      }
    }
  else
    {
    itkExceptionMacro(<< "No dimensions defined");
    }

  for ( bool readAttribute = false; !readAttribute; )
    {
    this->GetNextLine(file, text);

    if ( text.find("spacing") < text.length()
         || text.find("aspect_ratio") < text.length() )
      {
      double spacing[3];
      sscanf(text.c_str(), "%*s %lf %lf %lf", spacing, spacing + 1, spacing + 2);
      for ( unsigned int i = 0; i < m_NumberOfDimensions; i++ )
        {
        this->SetSpacing(i, spacing[i]);
        }
      }

    else if ( text.find("origin") < text.length() )
      {
      double origin[3];
      sscanf(text.c_str(), "%*s %lf %lf %lf", origin, origin + 1, origin + 2);
      for ( unsigned int i = 0; i < m_NumberOfDimensions; i++ )
        {
        this->SetOrigin(i, origin[i]);
        }
      }

    else if ( text.find("vector") < text.length() )
      {
      readAttribute = true;

      this->SetNumberOfComponents(3);
      this->SetPixelType(VECTOR);
      char pixelType[256];
      sscanf(text.c_str(), "%*s %*s %s", pixelType);
      text = pixelType;

      this->SetPixelTypeFromString(text);
      }

    else if ( text.find("color_scalars") < text.length() )
      {
      readAttribute = true;

      unsigned int numComp = 1;
      sscanf(text.c_str(), "%*s %*s %u", &numComp);
      if ( numComp == 1 )
        {
        this->SetPixelType(SCALAR);
        }
      else if ( numComp == 3 )
        {
        this->SetPixelType(RGB);
        }
      else if ( numComp == 4 )
        {
        this->SetPixelType(RGBA);
        }
      else
        {
        SetPixelType(VECTOR);
        }
      if ( this->GetFileType() == ASCII )
        {
        this->SetNumberOfComponents(numComp);
        this->SetComponentType(FLOAT);
        }
      else
        {
        this->SetNumberOfComponents(numComp);
        this->SetComponentType(UCHAR);
        }
      }

    else if ( text.find("scalars") < text.length() )
      {
      readAttribute = true;

      char pixelType[256];
      unsigned int  numComp = 1;
      // numComp is optional
      sscanf(text.c_str(), "%*s %*s %s %u", pixelType, &numComp);
      text = pixelType;
      if ( numComp == 1 )
        {
        this->SetPixelType(SCALAR);
        }
      else
        {
        this->SetPixelType(VECTOR);
        }
      this->SetPixelTypeFromString(text);
      this->SetNumberOfComponents(numComp);

      // maybe "LOOKUP_TABLE default"
      std::streampos pos = file.tellg();

      this->GetNextLine(file, text);
      if ( !( text.find("lookup_table") < text.length() ) )
        {
        // it was data and not table
        file.seekg(pos);
        }
      } //found scalars

    else if ( text.find("tensors") < text.length() )
      {
      readAttribute = true;

      char pixelType[256];
      sscanf(text.c_str(), "%*s %*s %s", pixelType);
      text = pixelType;
      this->SetPixelType(SYMMETRICSECONDRANKTENSOR);
      this->SetNumberOfComponents(6);
      this->SetPixelTypeFromString(text);
      }

    if ( !file.good() )
      {
      itkExceptionMacro(<< "Error reading header");
      }
    }

  // set the header size based on how much we just read
  this->m_HeaderSize = static_cast< SizeType >( file.tellg() );
}

void VTKImageIO::ReadHeaderSize(std::ifstream & file)
{
  std::string text;

  this->OpenFileForReading( file, m_FileName );

  this->GetNextLine(file, text); // HEADER
  this->GetNextLine(file, text); // TITLE
  this->GetNextLine(file, text); // DATA TYPE
  this->GetNextLine(file, text); // GEOMEETRY/TOPOLOGY TYPE

  this->GetNextLine(file, text); // DIMENSIONS

  for ( bool readAttribute = false; !readAttribute; )
    {
    this->GetNextLine(file, text); // SPACING|ORIGIN|COLOR_SCALARS|SCALARS|VECTOR|TENSORS

    if ( text.find("scalars") < text.length()
         || text.find("vector") < text.length()
         || text.find("color_scalars") < text.length()
         || text.find("tensors") < text.length() )
      {
      readAttribute = true;

      // maybe "LOOKUP_TABLE default"
      std::streampos pos = file.tellg();
      this->GetNextLine(file, text);

      if ( !( text.find("lookup_table") < text.length() ) )
        {
        // it was data and not table
        file.seekg(pos);
        }
      else
            {}
      } //found scalars
    }

  if ( file.fail() )
    {
    itkExceptionMacro(<< "Failed reading header information");
    }

  // set the header size based on how much we just read
  this->m_HeaderSize = static_cast< SizeType >( file.tellg() );
}

namespace
{
template <typename TComponent>
void ReadTensorBuffer( std::istream& is, TComponent *buffer,
  const ImageIOBase::SizeType num )
{
  typedef typename itk::NumericTraits<TComponent>::PrintType PrintType;
  PrintType temp;
  TComponent *ptr = buffer;
  ImageIOBase::SizeType i=0;
  // More than the resulting components because of symmetry.
  const ImageIOBase::SizeType fileComponents = num / 6 * 9;
  while( i < fileComponents )
    {
    // First row: hit hit hit
    is >> temp;
    *ptr = static_cast<TComponent>( temp );
    ++ptr;
    is >> temp;
    *ptr = static_cast<TComponent>( temp );
    ++ptr;
    is >> temp;
    *ptr = static_cast<TComponent>( temp );
    ++ptr;
    // Second row: skip hit hit
    is >> temp;
    is >> temp;
    *ptr = static_cast<TComponent>( temp );
    ++ptr;
    is >> temp;
    *ptr = static_cast<TComponent>( temp );
    ++ptr;
    // Third row: skip skip hit
    is >> temp;
    is >> temp;
    is >> temp;
    *ptr = static_cast<TComponent>( temp );
    ++ptr;
    i += 9;
    }
}
} // end anonymous namespace

void VTKImageIO::ReadBufferAsASCII( std::istream & is, void * buffer, IOComponentType ctype,
                                     const ImageIOBase::SizeType numComp)
{
  if( this->GetPixelType() == ImageIOBase::SYMMETRICSECONDRANKTENSOR )
    {
    if( this->GetNumberOfComponents() != 6 )
      {
      itkExceptionMacro(<< "itk::ERROR: VTKImageIO: Unsupported number of components in tensor.");
      }

    switch( ctype )
      {
      case FLOAT :
        {
        float * buf = reinterpret_cast<float *>(buffer);
        ReadTensorBuffer( is, buf, numComp );
        }
        break;

      case DOUBLE :
        {
        double * buf = reinterpret_cast<double *>(buffer);
        ReadTensorBuffer( is, buf, numComp );
        }
        break;

      default :
        itkExceptionMacro(<< "Per the vtk file format standard, only reading of float and double tensors is supported.");
      }
    }
  else
    {
    this->ImageIOBase::ReadBufferAsASCII( is, buffer, ctype, numComp );
    }
}

void VTKImageIO::ReadSymmetricTensorBufferAsBinary( std::istream & is,
  void * buffer,
  StreamingImageIOBase::SizeType num )
{
  std::streamsize bytesRemaining = num;
  const SizeType  componentSize = this->GetComponentSize();
  SizeType        pixelSize = componentSize * 6;

  if( this->GetNumberOfComponents() != 6 )
    {
    itkExceptionMacro(<< "Unsupported tensor dimension.");
    }
  while( bytesRemaining )
    {
    // row 1
    is.read( static_cast< char * >(buffer), 3 * componentSize );
    buffer = static_cast< char * >(buffer) + 3 * componentSize;
    // row 2
    is.seekg( componentSize, std::ios::cur );
    is.read( static_cast< char * >(buffer), 2 * componentSize );
    buffer = static_cast< char * >(buffer) + 2 * componentSize;
    // row 3
    is.seekg( 2 * componentSize, std::ios::cur );
    is.read( static_cast< char * >(buffer), componentSize );
    buffer = static_cast< char * >(buffer) + componentSize;
    bytesRemaining -= pixelSize;
    }

  if( is.fail() )
    {
    itkExceptionMacro(<< "Failure during writing of file.");
    }
}

void VTKImageIO::Read(void *buffer)
{
  std::ifstream file;

  if ( this->RequestedToStream() )
    {
    itkAssertOrThrowMacro(m_FileType != ASCII, "Can not stream with ASCII type files");

    if( this->GetPixelType() == ImageIOBase::SYMMETRICSECONDRANKTENSOR )
      {
      itkExceptionMacro(<< "Cannot stream read binary second rank tensors.");
      }

    // open and stream read
    this->OpenFileForReading( file, m_FileName );

    itkAssertOrThrowMacro(this->GetHeaderSize() != 0, "Header size is unknown when it shouldn't be!");
    this->StreamReadBufferAsBinary(file, buffer);

    switch ( this->GetComponentSize() )
      {
      case 1:
        break;
      case 2:
        ByteSwapper< uint16_t >::SwapRangeFromSystemToBigEndian( (uint16_t *)buffer, this->GetIORegionSizeInComponents() );
        break;
      case 4:
        ByteSwapper< uint32_t >::SwapRangeFromSystemToBigEndian( (uint32_t *)buffer, this->GetIORegionSizeInComponents() );
        break;
      case 8:
        ByteSwapper< uint64_t >::SwapRangeFromSystemToBigEndian( (uint64_t *)buffer, this->GetIORegionSizeInComponents() );
        break;
      default:
        itkExceptionMacro(<< "Unknown component size" << this->GetComponentSize());
      }
    }
  else
    {
    // open the file
    this->OpenFileForReading( file, m_FileName );

    itkAssertOrThrowMacro(this->GetHeaderSize() != 0, "Header size is unknown when it shouldn't be!");

    if ( file.fail() )
      {
      itkExceptionMacro(<< "Failed seeking to data position");
      }

    // seek pass the header
    std::streampos dataPos = static_cast< std::streampos >( this->GetHeaderSize() );
    file.seekg(dataPos, std::ios::beg);

    //We are positioned at the data. The data is read depending on whether
    //it is ASCII or binary.
    if ( m_FileType == ASCII )
      {
      this->ReadBufferAsASCII( file, buffer, this->GetComponentType(),
                               this->GetImageSizeInComponents()
                               );
      }
    else
      {
      // read the image
      if( this->GetPixelType() == ImageIOBase::SYMMETRICSECONDRANKTENSOR )
        {
        this->ReadSymmetricTensorBufferAsBinary( file, buffer, this->GetImageSizeInBytes() );
        }
      else
        {
        this->ReadBufferAsBinary( file, buffer, this->GetImageSizeInBytes() );
        }

      switch ( this->GetComponentSize() )
        {
        case 1:
          break;
        case 2:
          ByteSwapper< uint16_t >::SwapRangeFromSystemToBigEndian( (uint16_t *)buffer, this->GetImageSizeInComponents() );
          break;
        case 4:
          ByteSwapper< uint32_t >::SwapRangeFromSystemToBigEndian( (uint32_t *)buffer, this->GetImageSizeInComponents() );
          break;
        case 8:
          ByteSwapper< uint64_t >::SwapRangeFromSystemToBigEndian( (uint64_t *)buffer, this->GetImageSizeInComponents() );
          break;
        default:
          itkExceptionMacro(<< "Unknown component size" << this->GetComponentSize());
        }
      }
    }
}

void VTKImageIO::ReadImageInformation()
{
  std::ifstream file;

  this->InternalReadImageInformation(file);
}

bool VTKImageIO::CanWriteFile(const char *name)
{
  return itksys::SystemTools::GetFilenameLastExtension(name) == ".vtk";
}

void VTKImageIO::WriteImageInformation( const void *itkNotUsed(buffer) )
{
  std::ofstream file;

  // this will truncate the file
  this->OpenFileForWriting( file, m_FileName, true );

  // Check the image region for proper dimensions, etc.
  unsigned int numDims = this->GetNumberOfDimensions();
  if ( numDims < 1 || numDims > 3 )
    {
    itkExceptionMacro(<< "VTK Writer can only write 1, 2 or 3-dimensional images");
    }

  // Write the VTK header information
  file << "# vtk DataFile Version 3.0\n";
  file << "VTK File Generated by Insight Segmentation and Registration Toolkit (ITK)\n";

  if ( this->GetFileType() == ASCII )
    {
    file << "ASCII\n";
    }
  else
    {
    file << "BINARY\n";
    }

  file.setf(std::ios::scientific,
            std::ios::floatfield);

  file.precision(16);

  file.flush();

  // Write characteristics of the data
  file << "DATASET STRUCTURED_POINTS\n";
  file << "DIMENSIONS "
       << this->GetDimensions(0) << " "
       << ( ( this->GetNumberOfDimensions() > 1 ) ? this->GetDimensions(1) : 1 ) << " "
       << ( ( this->GetNumberOfDimensions() > 2 ) ? this->GetDimensions(2) : 1 ) << " "
       << "\n";
  file << "SPACING "
       << this->GetSpacing(0) << " "
       << ( ( this->GetNumberOfDimensions() > 1 ) ? this->GetSpacing(1) : 1.0 ) << " "
       << ( ( this->GetNumberOfDimensions() > 2 ) ? this->GetSpacing(2) : 1.0 ) << " "
       << "\n";
  file << "ORIGIN "
       << this->GetOrigin(0) << " "
       << ( ( this->GetNumberOfDimensions() > 1 ) ? this->GetOrigin(1) : 0.0 ) << " "
       << ( ( this->GetNumberOfDimensions() > 2 ) ? this->GetOrigin(2) : 0.0 ) << " "
       << "\n";

  file << "POINT_DATA " << this->GetImageSizeInPixels() << "\n";

  // NOTE: we don't write out RGB pixel types with ascii due to complication of
  // the required datatypes and the different ranges
  if ( ( ( this->GetPixelType() == RGB && this->GetNumberOfComponents() == 3 )
         || ( this->GetPixelType() == RGBA && this->GetNumberOfComponents() == 4 ) )
       &&
       ( this->GetComponentType() == UCHAR )
       && ( this->GetFileType() == Binary ) )
    {
    file << "COLOR_SCALARS color_scalars" << " "
         << this->GetNumberOfComponents() << "\n";
    }
  // Prefer the VECTORS representation when possible:
  else if ( this->GetPixelType() == VECTOR && this->GetNumberOfComponents() == 3 )
    {
    file << "VECTORS vectors "
         << this->GetComponentTypeAsString(m_ComponentType) << "\n";
    }
  else if( this->GetPixelType() == SYMMETRICSECONDRANKTENSOR )
    {
    file << "TENSORS tensors "
         << this->GetComponentTypeAsString(m_ComponentType) << "\n";
    }
  else
    {
    // According to VTK documentation number of components should in
    // range (1,4):
    // todo this should be asserted or checked earlier!
    file << "SCALARS scalars "
         << this->GetComponentTypeAsString(m_ComponentType) << " "
         << this->GetNumberOfComponents() << "\n"
         << "LOOKUP_TABLE default\n";
    }

  // set the header size based on how much we just wrote
  this->m_HeaderSize = static_cast< SizeType >( file.tellp() );
}

namespace
{
template<typename TComponent>
void WriteTensorBuffer(std::ostream & os, const TComponent * buffer, const ImageIOBase::SizeType num,
                       const ImageIOBase::SizeType components )
{
  const TComponent * ptr = buffer;

  typedef typename itk::NumericTraits<TComponent>::PrintType PrintType;

  ImageIOBase::SizeType i = 0;
  if( components == 3 )
    {
    PrintType zero( itk::NumericTraits<TComponent>::ZeroValue() );
    PrintType e12;
    while( i < num )
      {
      // row 1
      os << PrintType(*ptr++) << ' ';
      e12 = *ptr++;
      os << e12 << ' ';
      os << zero << '\n';
      // row 2
      os << e12 << ' ';
      os << PrintType(*ptr++) << ' ';
      os << zero << '\n';
      // row 3
      os << zero << ' ' << zero << ' ' << zero << "\n\n";
      i += 3;
      }
    }
  else if( components == 6 )
    {
    PrintType e12;
    PrintType e13;
    PrintType e23;
    while( i < num )
      {
      // row 1
      os << PrintType(*ptr++) << ' ';
      e12 = *ptr++;
      os << e12 << ' ';
      e13 = *ptr++;
      os << e13 << '\n';
      // row 2
      os << e12 << ' ';
      os << PrintType(*ptr++) << ' ';
      e23 = *ptr++;
      os << e23 << '\n';
      // row 3
      os << e13 << ' ';
      os << e23 << ' ';
      os << PrintType(*ptr++) << "\n\n";
      i += 6;
      }
    }
  else
    {
    ::itk::ExceptionObject e_(__FILE__, __LINE__,
                              "itk::ERROR: VTKImageIO: Unsupported number of components in tensor.",
                              ITK_LOCATION);
    throw e_;
    }
}

} // end anonymous namespace

void VTKImageIO::WriteBufferAsASCII( std::ostream & os, const void * buffer, IOComponentType ctype,
                                      const ImageIOBase::SizeType numComp)
{
  if( this->GetPixelType() == ImageIOBase::SYMMETRICSECONDRANKTENSOR )
    {
    switch( ctype )
      {
      case FLOAT :
        {
        typedef const float * Type;
        Type buf = reinterpret_cast<Type>(buffer);
        WriteTensorBuffer( os, buf, numComp, this->GetNumberOfComponents() );
        }
        break;

      case DOUBLE :
        {
        typedef const double * Type;
        Type buf = reinterpret_cast<Type>(buffer);
        WriteTensorBuffer( os, buf, numComp, this->GetNumberOfComponents() );
        }
        break;

      default :
        itkExceptionMacro(<< "Per the vtk file format standard, only writing of float and double tensors is supported.");
      }
    }
  else
    {
    this->ImageIOBase::WriteBufferAsASCII( os, buffer, ctype, numComp );
    }
}

#define WriteVTKImageBinaryBlockMACRO(storageType)                                                     \
{                                                                                                      \
   const ImageIOBase::BufferSizeType numbytes =                                                        \
                            static_cast< ImageIOBase::BufferSizeType >( this->GetImageSizeInBytes() ); \
   const ImageIOBase::BufferSizeType numberImageComponents =                                           \
                       static_cast< ImageIOBase::BufferSizeType >( this->GetImageSizeInComponents() ); \
   const bool isSymmetricSecondRankTensor=                                                             \
                                   ( this->GetPixelType() == ImageIOBase::SYMMETRICSECONDRANKTENSOR ); \
   storageType *tempmemory = new storageType[numberImageComponents];                                   \
   memcpy(tempmemory, buffer, numbytes );                                                              \
   ByteSwapper< storageType >::SwapRangeFromSystemToBigEndian(  tempmemory , numberImageComponents );  \
   /* write the image */                                                                               \
   if( isSymmetricSecondRankTensor )                                                                   \
     {                                                                                                 \
     this->WriteSymmetricTensorBufferAsBinary( file, tempmemory, numbytes );                           \
     }                                                                                                 \
   else                                                                                                \
     {                                                                                                 \
     if ( !this->WriteBufferAsBinary( file, tempmemory, numbytes ) )                                   \
       {                                                                                               \
       itkExceptionMacro(<< "Could not write file: " << m_FileName);                                   \
       }                                                                                               \
     }                                                                                                 \
   delete[] tempmemory;                                                                                \
}                                                                                                      \

#define StreamWriteVTKImageBinaryBlockMACRO(storageType)                                               \
{                                                                                                      \
   const ImageIOBase::BufferSizeType numbytes =                                                        \
                    static_cast< ImageIOBase::BufferSizeType >( this->GetIORegionSizeInBytes() );      \
   const ImageIOBase::BufferSizeType numberImageComponents =                                           \
                    static_cast< ImageIOBase::BufferSizeType >( this->GetIORegionSizeInComponents() ); \
   storageType *tempmemory = new storageType[numberImageComponents];                                   \
   memcpy(tempmemory, buffer, numbytes );                                                              \
   ByteSwapper< storageType >::SwapRangeFromSystemToBigEndian(  tempmemory , numberImageComponents );  \
   /* write the image */                                                                               \
   this->StreamWriteBufferAsBinary( file, tempmemory);                                                 \
   delete[] tempmemory;                                                                                \
}                                                                                                      \

void VTKImageIO::WriteSymmetricTensorBufferAsBinary(std::ostream & os, const void * buffer,
                                                     StreamingImageIOBase::SizeType num)
{
  std::streamsize bytesRemaining = num;
  const SizeType  componentSize = this->GetComponentSize();
  SizeType        pixelSize;
  char            zero[1024];

  memset( zero, 0, 1024 );

  switch( this->GetNumberOfComponents() )
    {
    case 3 :
      {
      pixelSize = componentSize * 3;
      while( bytesRemaining )
        {
        // row 1
        os.write( static_cast< const char * >(buffer), 2 * componentSize );
        os.write( zero, componentSize );
        buffer = static_cast< const char * >(buffer) + componentSize;
        // row 2
        os.write( static_cast< const char * >(buffer), 2 * componentSize );
        buffer = static_cast< const char * >(buffer) + 2 * componentSize;
        os.write( zero, componentSize );
        // row 3
        os.write( zero, 3 * componentSize );
        bytesRemaining -= pixelSize;
        }

      break;
      }
    case 6 :
      {
      pixelSize = componentSize * 6;
      while( bytesRemaining )
        {
        // row 1
        os.write( static_cast< const char * >(buffer), 3 * componentSize );
        buffer = static_cast< const char * >(buffer) + componentSize;
        // row 2
        os.write( static_cast< const char * >(buffer), componentSize );
        buffer = static_cast< const char * >(buffer) + 2 * componentSize;
        os.write( static_cast< const char * >(buffer), 2 * componentSize );
        buffer = static_cast< const char * >(buffer) - componentSize;
        // row 3
        os.write( static_cast< const char * >(buffer), componentSize );
        buffer = static_cast< const char * >(buffer) + 2 * componentSize;
        os.write( static_cast< const char * >(buffer), 2 * componentSize );
        buffer = static_cast< const char * >(buffer) + 2 * componentSize;
        bytesRemaining -= pixelSize;
        }

      break;
      }
    default :
      itkExceptionMacro(<< "Unsupported tensor dimension.");
    }

  if( os.fail() )
    {
    itkExceptionMacro(<< "Failure during writing of file.");
    }
}

void VTKImageIO::Write(const void *buffer)
{
  if ( this->RequestedToStream() )
    {
    itkAssertOrThrowMacro(m_FileType != ASCII, "Can not stream with ASCII type files");

    if( this->GetPixelType() == ImageIOBase::SYMMETRICSECONDRANKTENSOR )
      {
      itkExceptionMacro(<< "Cannot stream write binary second rank tensors.");
      }

    std::ofstream file;

    // we assume that GetActualNumberOfSplitsForWriting is called before
    // this methods and it will remove the file if a new header needs to
    // be written
    if ( !itksys::SystemTools::FileExists( m_FileName.c_str() ) )
      {
      this->WriteImageInformation(buffer);

      // open and stream write
      this->OpenFileForWriting( file, m_FileName, false );

      // write one byte at the end of the file to allocate (this is a
      // nifty trick which should not write the entire size of the file
      // just allocate it, if the system supports sparse files)
      std::streampos seekPos = this->GetImageSizeInBytes() + this->GetHeaderSize() - 1;
      file.seekp(seekPos, std::ios::cur);
      file.write("\0", 1);
      file.seekp(0);
      }
    else
      {
      // must always recheck the header size incase something has changed
      std::ifstream ifile;
      this->ReadHeaderSize(ifile);

      itkAssertOrThrowMacro(this->GetHeaderSize() != 0, "Header size is unknown when it shouldn't be!");

      // open and stream write
      this->OpenFileForWriting( file, m_FileName, false );
      }

    // the binary data must be written in big endian format
    if ( !ByteSwapper< uint16_t >::SystemIsBigEndian() )
      {
      // only swap  when needed
      switch ( this->GetComponentSize() )
        {
        case 1:
          StreamWriteVTKImageBinaryBlockMACRO(char)
          break;
        case 2:
          StreamWriteVTKImageBinaryBlockMACRO(uint16_t)
          break;
        case 4:
          StreamWriteVTKImageBinaryBlockMACRO(uint32_t)
          break;
        case 8:
          StreamWriteVTKImageBinaryBlockMACRO(uint64_t)
          break;
        default:
          itkExceptionMacro(<< "Unknown component size" << this->GetComponentSize());
        }
      }
    else
      {
        this->StreamWriteBufferAsBinary(file, buffer);
      }
    }
  else
    {
    // this will truncate file and write header
    this->WriteImageInformation(buffer);

    std::ofstream file;
    // open the file
    this->OpenFileForWriting( file, m_FileName, false );

    itkAssertOrThrowMacro(this->GetHeaderSize() != 0, "Header size is unknown when it shouldn't be!");

    // seek pass the header
    std::streampos dataPos = static_cast< std::streampos >( this->GetHeaderSize() );
    file.seekp(dataPos, std::ios::beg);

    if ( file.fail() )
      {
      itkExceptionMacro(<< "Failed seeking to data position");
      }

    // Write the actual pixel data
    if ( m_FileType == ASCII )
      {
      this->WriteBufferAsASCII( file, buffer, this->GetComponentType(),
                                this->GetImageSizeInComponents()
                                );
      }
    else //binary
      {
      // the binary data must be written in big endian format
      if ( !ByteSwapper< uint16_t >::SystemIsBigEndian() )
        {
        // only swap  when needed
        switch ( this->GetComponentSize() )
          {
          case 1:
            WriteVTKImageBinaryBlockMACRO(char)
            break;
          case 2:
            WriteVTKImageBinaryBlockMACRO(uint16_t)
            break;
          case 4:
            WriteVTKImageBinaryBlockMACRO(uint32_t)
            break;
          case 8:
            WriteVTKImageBinaryBlockMACRO(uint64_t)
            break;
          default:
            itkExceptionMacro(<< "Unknown component size" << this->GetComponentSize());
          }
        }
      else
        {
        // write the image
        if( this->GetPixelType() == ImageIOBase::SYMMETRICSECONDRANKTENSOR )
          {
          this->WriteSymmetricTensorBufferAsBinary( file, buffer, this->GetImageSizeInBytes() );
          }
        else
          {
          if ( !this->WriteBufferAsBinary( file, buffer, this->GetImageSizeInBytes() ) )
            {
            itkExceptionMacro(<< "Could not write file: " << m_FileName);
            }
          }
        }
      }
    }
}

ImageIOBase::SizeType VTKImageIO::GetIORegionSizeInPixels() const
{
  return m_IORegion.GetNumberOfPixels();
}

ImageIOBase::SizeType VTKImageIO::GetIORegionSizeInComponents() const
{
  return ( this->GetIORegionSizeInPixels() * m_NumberOfComponents );
}

ImageIOBase::SizeType VTKImageIO::GetIORegionSizeInBytes() const
{
  return ( this->GetIORegionSizeInComponents() * this->GetComponentSize() );
}

void VTKImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk
