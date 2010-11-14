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
#include "itkVTKImageIO2.h"
#include "itkByteSwapper.h"

#include "itksys/ios/sstream"
#include "itksys/SystemTools.hxx"

#include <stdio.h>
#include <string.h>

namespace itk
{
VTKImageIO2::VTKImageIO2()
{
  this->SetNumberOfDimensions(2);
  m_ByteOrder = LittleEndian;
  m_FileType = Binary;
  m_HeaderSize = 0;

  this->AddSupportedReadExtension(".vtk");

  this->AddSupportedWriteExtension(".vtk");
}

VTKImageIO2::~VTKImageIO2()
{}

bool VTKImageIO2::CanReadFile(const char *filename)
{
  std::ifstream file;
  char          buffer[256];
  std::string   fname(filename);

  if ( fname.find(".vtk") >= fname.length() )
    {
    return false;
    }

  try
    {
    this->OpenFileForReading(file, filename);
    }
  catch ( ... )
    {
    return false;
    }

  // Check to see if its a vtk structured points file
  file.getline(buffer, 255);
  file.getline(buffer, 255);
  file.getline(buffer, 255);
  file.getline(buffer, 255);

  fname = buffer;

  if ( fname.find("STRUCTURED_POINTS") < fname.length()
       || fname.find("structured_points") < fname.length() )
    {
    return true;
    }
  else
    {
    return false;
    }
}

void VTKImageIO2::SetPixelTypeFromString(const std::string & pixelType)
{
  if ( pixelType.find("float") < pixelType.length() )
    {
    SetComponentType(FLOAT);
    }
  else if ( pixelType.find("double") < pixelType.length() )
    {
    SetComponentType(DOUBLE);
    }
  else if ( pixelType.find("unsigned_char") < pixelType.length() )
    {
    SetComponentType(UCHAR);
    }
  else if ( pixelType.find("char") < pixelType.length() )
    {
    SetComponentType(CHAR);
    }
  else if ( pixelType.find("unsigned_short") < pixelType.length() )
    {
    SetComponentType(USHORT);
    }
  else if ( pixelType.find("short") < pixelType.length() )
    {
    SetComponentType(SHORT);
    }
  else if ( pixelType.find("unsigned_int") < pixelType.length() )
    {
    SetComponentType(UINT);
    }
  else if ( pixelType.find("int") < pixelType.length() )
    {
    SetComponentType(INT);
    }
  else if ( pixelType.find("unsigned_long") < pixelType.length() )
    {
    SetComponentType(ULONG);
    }
  else if ( pixelType.find("long") < pixelType.length() )
    {
    SetComponentType(LONG);
    }
  else
    {
    itkExceptionMacro(<< "Unrecognized type");
    }
}

void VTKImageIO2::InternalReadImageInformation(std::ifstream & file)
{
  char        line[255];
  std::string text;

  this->OpenFileForReading( file, m_FileName.c_str() );

  file.getline(line, 255);
  file.getline(line, 255);
  file.getline(line, 255);
  text = line;
  if ( text.find("ASCII") < text.length()
       || text.find("ascii") < text.length() )
    {
    this->SetFileTypeToASCII();
    }
  else if ( text.find("BINARY") < text.length()
            || text.find("binary") < text.length() )
    {
    this->SetFileTypeToBinary();
    }
  else
    {
    itkExceptionMacro(<< "Unrecognized type");
    }
  file.getline(line, 255);
  text = line;
  if ( text.find("STRUCTURED_POINTS") >= text.length()
       && text.find("structured_points") >= text.length() )
    {
    itkExceptionMacro(<< "Not structured points, can't read");
    }

  file.getline(line, 255);
  text = line;

  // set values in case we don't find them
  this->SetNumberOfDimensions(3);
  this->SetSpacing(0, 1.0);
  this->SetSpacing(1, 1.0);
  this->SetSpacing(2, 1.0);
  this->SetOrigin(0, 0.0);
  this->SetOrigin(1, 0.0);
  this->SetOrigin(2, 0.0);

  if ( text.find("DIMENSIONS") < text.length()
       || text.find("dimensions") < text.length() )
    {
    unsigned int dims[3];
    sscanf(line, "%*s %d %d %d", dims, dims + 1, dims + 2);
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

  for ( bool readScalars = false; !readScalars; )
    {
    file.getline(line, 255);
    text = line;

    if ( text.find("SPACING") < text.length()
         || text.find("spacing") < text.length()
         || text.find("ASPECT_RATIO") < text.length()
         || text.find("aspect_ratio") < text.length() )
      {
      double spacing[3];
      sscanf(line, "%*s %lf %lf %lf", spacing, spacing + 1, spacing + 2);
      for ( unsigned int i = 0; i < m_NumberOfDimensions; i++ )
        {
        this->SetSpacing(i, spacing[i]);
        }
      }

    else if ( text.find("ORIGIN") < text.length()
              || text.find("origin") < text.length() )
      {
      double origin[3];
      sscanf(line, "%*s %lf %lf %lf", origin, origin + 1, origin + 2);
      for ( unsigned int i = 0; i < m_NumberOfDimensions; i++ )
        {
        this->SetOrigin(i, origin[i]);
        }
      }

    else if ( text.find("VECTOR") < text.length()
              || text.find("vector") < text.length() )
      {
      readScalars = true;

      this->SetNumberOfComponents(3);
      this->SetPixelType(VECTOR);
      char pixelType[256];
      sscanf(line, "%*s %*s %s", pixelType);
      text = pixelType;

      this->SetPixelTypeFromString(text);
      }

    else if ( text.find("COLOR_SCALARS") < text.length()
              || text.find("color_scalars") < text.length() )
      {
      readScalars = true;

      int numComp = 1;
      sscanf(line, "%*s %*s %d", &numComp);
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

    else if ( text.find("SCALARS") < text.length()
              || text.find("scalars") < text.length() )
      {
      readScalars = true;

      char pixelType[256];
      int  numComp = 1;
      // numComp is optional
      sscanf(line, "%*s %*s %s %d", pixelType, &numComp);
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
      file.getline(line, 255);
      text = line;
      if ( !( text.find("LOOKUP_TABLE") < text.length()
              || text.find("lookup_table") < text.length() ) )
        {
        // it was data and not table
        file.seekg(pos);
        }
      } //found scalars

    if ( !file.good() )
      {
      itkExceptionMacro(<< "Error reading header");
      }
    }

  // set the header size based on how much we just read
  this->m_HeaderSize = static_cast< SizeType >( file.tellg() );
}

void VTKImageIO2::ReadHeaderSize(std::ifstream & file)
{
  char        line[255];
  std::string text;

  this->OpenFileForReading( file, m_FileName.c_str() );

  file.getline(line, 255); // HEADER
  file.getline(line, 255); // TITLE
  file.getline(line, 255); // DATA TYPE
  file.getline(line, 255); // GEOMEETRY/TOPOLOGY TYPE

  file.getline(line, 255); // DIMENSIONS

  for ( bool readScalars = false; !readScalars; )
    {
    file.getline(line, 255); // SPACING|ORIGIN|COLOR_SCALARS|SCALARS|VECTOR
    text = line;

    if ( text.find("SCALARS") < text.length()
         || text.find("scalars") < text.length()
         || text.find("VECTOR") < text.length()
         || text.find("vector") < text.length()
         || text.find("COLOR_SCALARS") < text.length()
         || text.find("color_scalars") < text.length() )
      {
      readScalars = true;

      // maybe "LOOKUP_TABLE default"
      std::streampos pos = file.tellg();
      file.getline(line, 255);
      text = line;
      if ( !( text.find("LOOKUP_TABLE") < text.length()
              || text.find("lookup_table") < text.length() ) )
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

void VTKImageIO2::Read(void *buffer)
{
  std::ifstream file;

  if ( this->RequestedToStream() )
    {
    itkAssertOrThrowMacro(m_FileType != ASCII, "Can not stream with ASCII type files");

    // open and stream read
    this->OpenFileForReading( file, this->m_FileName.c_str() );

    itkAssertOrThrowMacro(this->GetHeaderSize() != 0, "Header size is unknown when it shouldn't be!");
    this->StreamReadBufferAsBinary(file, buffer);
    }
  else
    {
    // open the file
    this->OpenFileForReading( file, this->m_FileName.c_str() );

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
                               this->GetImageSizeInComponents() );
      }
    else
      {
      // read the image
      this->ReadBufferAsBinary( file, buffer, this->GetImageSizeInBytes() );

      int size = this->GetComponentSize();
      switch ( size )
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
          itkExceptionMacro(<< "Unknown component size" << size);
        }
      }
    }
}

void VTKImageIO2::ReadImageInformation()
{
  std::ifstream file;

  this->InternalReadImageInformation(file);
}

bool VTKImageIO2::CanWriteFile(const char *name)
{
  std::string filename = name;

  if ( filename != ""
       && filename.find(".vtk") < filename.length() )
    {
    return true;
    }
  return false;
}

void VTKImageIO2::WriteImageInformation( const void *itkNotUsed(buffer) )
{
  std::ofstream file;

  // this will truncate the file
  this->OpenFileForWriting(file, m_FileName.c_str(), true);

  // Check the image region for proper dimensions, etc.
  unsigned int numDims = this->GetNumberOfDimensions();
  if ( numDims < 1 || numDims > 3 )
    {
    itkExceptionMacro(<< "VTK Writer can only write 1, 2 or 3-dimensional images");
    return;
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

  file.setf(itksys_ios::ios::scientific,
            itksys_ios::ios::floatfield);

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
  else
    {
    // According to VTK documentation number of components should in
    // range (1,4):
    // todo this should be asserted or checked ealier!
    file << "SCALARS scalars "
         << this->GetComponentTypeAsString(m_ComponentType) << " "
         << this->GetNumberOfComponents() << "\n"
         << "LOOKUP_TABLE default\n";
    }

  // set the header size based on how much we just wrote
  this->m_HeaderSize = static_cast< SizeType >( file.tellp() );
}

void VTKImageIO2::Write(const void *buffer)
{
  if ( this->RequestedToStream() )
    {
    itkAssertOrThrowMacro(m_FileType != ASCII, "Can not stream with ASCII type files");

    std::ofstream file;

    // we assume that GetActualNumberOfSplitsForWriting is called before
    // this methods and it will remove the file if a new header needs to
    // be written
    if ( !itksys::SystemTools::FileExists( m_FileName.c_str() ) )
      {
      this->WriteImageInformation(buffer);

      // open and stream write
      this->OpenFileForWriting(file, this->m_FileName.c_str(), false);

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
      this->OpenFileForWriting(file, this->m_FileName.c_str(), false);
      }

    this->StreamWriteBufferAsBinary(file, buffer);
    }

  else
    {
    // this will truncate file and write header
    this->WriteImageInformation(buffer);

    std::ofstream file;
    // open the file
    this->OpenFileForWriting(file, this->m_FileName.c_str(), false);

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
                                this->GetImageSizeInComponents() );
      }
    else //binary
      {
      // the binary data must be written in big endian format
      if ( !ByteSwapper< uint16_t >::SystemIsBigEndian() )
        {
        // only swap  when needed
        const ImageIOBase::BufferSizeType numbytes =
          static_cast< ImageIOBase::BufferSizeType >( this->GetImageSizeInBytes() );

        char *tempmemory = new char[numbytes];
        memcpy(tempmemory, buffer, numbytes);
        switch ( this->GetComponentSize() )
          {
          case 1:
            break;
          case 2:
            ByteSwapper< uint16_t >::SwapRangeFromSystemToBigEndian( (uint16_t *)( tempmemory ),
                                                                     static_cast< BufferSizeType >( this->
                                                                                                    GetImageSizeInComponents() ) );
            break;
          case 4:
            ByteSwapper< uint32_t >::SwapRangeFromSystemToBigEndian( (uint32_t *)( tempmemory ),
                                                                     static_cast< BufferSizeType >( this->
                                                                                                    GetImageSizeInComponents() ) );
            break;
          case 8:
            ByteSwapper< uint64_t >::SwapRangeFromSystemToBigEndian( (uint64_t *)( tempmemory ),
                                                                     static_cast< BufferSizeType >( this->
                                                                                                    GetImageSizeInComponents() ) );
            break;
          }

        // write the image
        if ( !this->WriteBufferAsBinary( file, tempmemory, this->GetImageSizeInBytes() ) )
          {
          itkExceptionMacro(<< "Could not write file: " << m_FileName);
          }
        delete[] tempmemory;
        }
      else
        {
        // write the image
        if ( !this->WriteBufferAsBinary( file, buffer, this->GetImageSizeInBytes() ) )
          {
          itkExceptionMacro(<< "Could not write file: " << m_FileName);
          }
        }
      }
    }
}

void VTKImageIO2::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk
