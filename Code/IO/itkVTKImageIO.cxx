/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVTKImageIO.h"
#include <stdio.h>
#include "itkByteSwapper.h"

namespace itk
{

VTKImageIO::VTKImageIO()
{
  this->SetNumberOfDimensions(2);
  m_ByteOrder = LittleEndian;
  m_FileType = Binary;
  
}

VTKImageIO::~VTKImageIO()
{
}

bool VTKImageIO::OpenVTKFileForReading(std::ifstream& os, 
                                       const char* filename)
                                       
{
  // Make sure that we have a file to 
  if ( filename == "" )
    {
    itkExceptionMacro(<<"A FileName must be specified.");
    return false;
    }

  // Close file from any previous image
  if ( os.is_open() )
    {
    os.close();
    }
  
  // Open the new file for reading
  itkDebugMacro(<< "Initialize: opening file " << filename);

  // Actually open the file
  int openMode = std::ios::in;

#ifdef _WIN32
  openMode |= std::ios::binary;
#endif
#if defined(__GNUC__) && __GNUC__ >= 3
  os.open(filename, static_cast<std::ios_base::openmode>(openMode));
#elif defined (__MWERKS__)
  os.open(filename, static_cast<std::ios_base::openmode>(openMode));
#else
  os.open(filename, openMode);
#endif
  if ( os.fail() )
    {
    itkExceptionMacro(<< "Could not open file for reading: " << filename);
    return false;
    }

  return true;
}

bool VTKImageIO::OpenVTKFileForWriting(std::ofstream& os, 
                                       const char* filename)
                                       
{
  // Make sure that we have a file to 
  if ( filename == "" )
    {
    itkExceptionMacro(<<"A FileName must be specified.");
    return false;
    }

  // Create the file. This is required on some older sgi's
  std::ofstream tFile(filename,std::ios::out);
  tFile.close();                    

  // Close file from any previous image
  if ( os.is_open() )
    {
    os.close();
    }
  
  // Open the new file for writing
  itkDebugMacro(<< "Initialize: opening file " << filename);

  // Actually open the file
  int openMode = std::ios::out;

#ifdef _WIN32
  openMode |= std::ios::binary;
#endif
#if defined(__GNUC__) && __GNUC__ >= 3
  os.open(filename, static_cast<std::ios_base::openmode>(openMode));
#elif defined (__MWERKS__)
  os.open(filename, static_cast<std::ios_base::openmode>(openMode));
#else
  os.open(filename, openMode);
#endif
  if ( os.fail() )
    {
    itkExceptionMacro(<< "Could not open file for writing: " << filename);
    return false;
    }

  return true;
}


bool VTKImageIO::CanReadFile(const char* filename) 
{ 
  std::ifstream file;
  char buffer[256];
  std::string fname(filename);

  if ( fname.find(".vtk") >= fname.length() )
    {
    return false;
    }

  if ( ! this->OpenVTKFileForReading(file, filename) )
    {
    return false;
    }

  // Check to see if its a vtk structured points file
  file.getline(buffer,255);
  file.getline(buffer,255);
  file.getline(buffer,255);
  file.getline(buffer,255);

  fname = buffer;

  if ( fname.find("STRUCTURED_POINTS") < fname.length() ||
       fname.find("structured_points") < fname.length() )
    {
    return true;
    }
  else
    {
    return false;
    }
}
  
 
void VTKImageIO::InternalReadImageInformation(std::ifstream& file)
{
  char line[255];
  std::string text;

  if ( ! this->OpenVTKFileForReading(file, m_FileName.c_str()) )
    {
    itkExceptionMacro(<< "Cannot read requested file");
    }

  file.getline(line,255);
  file.getline(line,255);
  file.getline(line,255);
  text = line;
  if ( text.find("ASCII") < text.length() || 
       text.find("ascii") < text.length() )
    {
    this->SetFileTypeToASCII();
    }
  else if ( text.find("BINARY") < text.length() ||
            text.find("binary") < text.length() )
    {
    this->SetFileTypeToBinary();
    }
  else
    {
    itkExceptionMacro(<< "Unrecognized type");
    }
  file.getline(line,255);
  text = line;
  if ( text.find("STRUCTURED_POINTS") >= text.length() &&
       text.find("structured_points") >= text.length() )
    {
    itkExceptionMacro(<< "Not structured points, can't read");
    }

  //extract dimensions, spacing, origin
  unsigned int dims[3];
  float spacing[3];
  float origin[3];
  file.getline(line,255);
  text = line;

  // set values in case we don't find them
  this->SetNumberOfDimensions(3);
  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;
  m_Spacing[2] = 1.0;
  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;
  m_Origin[2] = 1.0;

  if ( text.find("DIMENSIONS") < text.length() || 
       text.find("dimensions") < text.length() )
    {
    sscanf(line, "%*s %d %d %d", dims, dims+1, dims+2);
    if ( dims[2] <= 1 )
      {
      this->SetNumberOfDimensions(2);
      }
    else
      {
      this->SetNumberOfDimensions(3);
      }
    for ( unsigned int i=0; i < m_NumberOfDimensions; i++ )
      {
      m_Dimensions[i] = dims[i];
      }
    }
  else
    {
    itkExceptionMacro(<<"No dimensions defined");
    }

  for ( bool readScalars=false; !readScalars; )
    {
    file.getline(line,255);
    text = line;

    if ( text.find("SPACING") < text.length() || 
         text.find("spacing") < text.length() )
      {
      sscanf(line, "%*s %f %f %f", spacing, spacing+1, spacing+2);
      for ( unsigned int i=0; i < m_NumberOfDimensions; i++ )
        {
        m_Spacing[i] = spacing[i];
        }
      }

    else if ( text.find("ORIGIN") < text.length() || 
              text.find("origin") < text.length() )
      {
      sscanf(line, "%*s %f %f %f", origin, origin+1, origin+2);
      for ( unsigned int i=0; i < m_NumberOfDimensions; i++ )
        {
        m_Origin[i] = origin[i];
        }
      }

     else if ( text.find("COLOR_SCALARS") < text.length() || 
              text.find("color_scalars") < text.length() )
      {
      readScalars = true;

      int numComp = 1;
      sscanf(line, "%*s %*s %d", &numComp);
      if ( this->GetFileType() == ASCII )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(FLOAT);
        SetComponentType(FLOAT);
        }
      else
        {
        SetNumberOfComponents(numComp);
        SetPixelType(UCHAR);
        SetComponentType(UCHAR);
        }
      }

    else if ( text.find("SCALARS") < text.length() || 
              text.find("scalars") < text.length() )
      {
      readScalars = true;

      char pixelType[256];
      int numComp = 1;
      sscanf(line, "%*s %*s %s %d", pixelType, &numComp);
      text = pixelType;
      if ( text.find("float") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(FLOAT);
        SetComponentType(FLOAT);
        }
      else if ( text.find("double") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(DOUBLE);
        SetComponentType(DOUBLE);
        }
      else if ( text.find("unsigned_char") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(UCHAR);
        SetComponentType(UCHAR);
        }
      else if ( text.find("char") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(CHAR);
        SetComponentType(CHAR);
        }
      else if ( text.find("unsigned_short") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(USHORT);
        SetComponentType(USHORT);
        }
      else if ( text.find("short") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(SHORT);
        SetComponentType(SHORT);
        }
      else if ( text.find("unsigned_int") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(UINT);
        SetComponentType(UINT);
        }
      else if ( text.find("int") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(INT);
        SetComponentType(INT);
        }
      else if ( text.find("unsigned_long") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(ULONG);
        SetComponentType(ULONG);
        }
      else if ( text.find("long") < text.length() )
        {
        SetNumberOfComponents(numComp);
        SetPixelType(LONG);
        SetComponentType(LONG);
        }
      else
        {
        itkExceptionMacro(<<"Unrecognized type");
        }
      file.getline(line,255);
      text = line;

      }//found scalars
    
      }
}

void VTKImageIO::Read(void* buffer)
{
  std::ifstream file;

  this->InternalReadImageInformation(file);
  
  //We are positioned at the data. The data is read depending on whether 
  //it is ASCII or binary.
  if ( m_FileType == ASCII )
    {
    this->ReadBufferAsASCII(file, buffer, this->GetComponentType(),
                            this->GetImageSizeInComponents());
    }
  else
    {
    file.read(static_cast<char*>(buffer), this->GetImageSizeInBytes());
    int size = this->GetPixelSize();
    switch( size )
      {
      case 2:
        ByteSwapper<short>::SwapRangeFromSystemToBigEndian((short *)buffer, this->GetImageSizeInComponents() );
        break;
      case 4:
        ByteSwapper<float>::SwapRangeFromSystemToBigEndian((float *)buffer, this->GetImageSizeInComponents() );
        break;
      case 6:
        ByteSwapper<double>::SwapRangeFromSystemToBigEndian((double *)buffer, this->GetImageSizeInComponents() );
        break;
      }
   }
}

void VTKImageIO::ReadImageInformation()
{
  std::ifstream file;
  this->InternalReadImageInformation(file);
}

bool VTKImageIO::CanWriteFile( const char* name )
{
  std::string filename = name;
  if ( filename != "" &&
       filename.find(".vtk") < filename.length() )
    {
    return true;
    }
  return false;
}

void VTKImageIO::Write(const void* buffer)
{
  std::ofstream file;
  if ( ! this->OpenVTKFileForWriting(file,m_FileName.c_str()) )
    {
    return;
    }

  // Check the image region for proper dimensions, etc.
  unsigned int numDims = this->GetNumberOfDimensions();
  if ( numDims < 2 || numDims > 3 )
    {
    itkExceptionMacro(<<"VTK Writer can only write 2 or 3-dimensional images");
    return;
    }
  ImageIORegion ioRegion = this->GetIORegion();

  // Write the VTK header information
  file << "# vtk DataFile Version 3.0\n"; 
  file << "VTK File Generated by Insight Segmentation and Registration Toolkit (ITK)\n";

  if ( this->GetFileType() == ASCII ) { file << "ASCII\n"; }
  else { file << "BINARY\n"; }

  // Write characteristics of the data
  file << "DATASET STRUCTURED_POINTS\n";
  if ( numDims == 2 )
    {
    file << "DIMENSIONS " << this->GetDimensions(0) << " "
         << this->GetDimensions(1) << " 1\n";
    file << "SPACING " << m_Spacing[0] << " " << m_Spacing[1] << " 1.0\n";
    file << "ORIGIN " << m_Origin[0] << " " << m_Origin[1] << " 0.0\n";
    }
  else //numDims == 3
    {
    file << "DIMENSIONS " << this->GetDimensions(0) << " "
         << this->GetDimensions(1) << " " << this->GetDimensions(2) << "\n";
    file << "SPACING " << m_Spacing[0] << " " 
         << m_Spacing[1] << " " << m_Spacing[2] << "\n";
    file << "ORIGIN " << m_Origin[0] << " "
         << m_Origin[1] << " " << m_Origin[2] << "\n";
    }

  file << "POINT_DATA " << this->GetImageSizeInPixels() << "\n";
  file << "SCALARS scalars " 
       << this->ReturnTypeAsString(this->GetComponentType()) << " "
       << this->GetNumberOfComponents() << "\n";
  file << "LOOKUP_TABLE default\n";

  // Write the actual pixel data
  if ( m_FileType == ASCII )
    {
    this->WriteBufferAsASCII(file, buffer, this->GetComponentType(),
                            this->GetImageSizeInComponents());
    }
  else //binary
  {
    int size = this->GetPixelSize();
    const unsigned long int numbytes=this->GetImageSizeInBytes();
    char * tempmemory=new char[numbytes];
    memcpy(tempmemory,buffer,numbytes);
    switch( size )
    {
      case 2:
        {
          ByteSwapper<short>::SwapRangeFromSystemToBigEndian(reinterpret_cast<short *>(tempmemory), this->GetImageSizeInComponents() );
        }
        break;
      case 4:
        {
          ByteSwapper<float>::SwapRangeFromSystemToBigEndian(reinterpret_cast<float *>(tempmemory), this->GetImageSizeInComponents() );
        }
        break;
      case 6:
        {
          ByteSwapper<double>::SwapRangeFromSystemToBigEndian(reinterpret_cast<double *>(tempmemory), this->GetImageSizeInComponents() );
        }
        break;
    }
    file.write(static_cast<const char*>(tempmemory), this->GetImageSizeInBytes());
    delete [] tempmemory;
  }
}

void VTKImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


} // end namespace itk
