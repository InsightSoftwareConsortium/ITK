/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioRadImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBioRadImageIO.h"
#include "itkByteSwapper.h"
#include <itksys/SystemTools.hxx>
#include <string.h>  // for strncpy

#define BIORAD_HEADER_LENGTH    76
#define BIORAD_NOTE_LENGTH      96
#define BIORAD_NOTE_TEXT_LENGTH 80
#define BIORAD_RGB_LUT_LENGTH   768
#define BIORAD_FILE_ID_OFFSET   54
#define BIORAD_MAGIC_NUMBER     12345

namespace itk
{

struct bioradheader {
    unsigned short nx, ny;      // 0   2*2  image width and height in pixels
    unsigned short npic;        // 4   2    number of images in file
    unsigned short ramp1_min, ramp1_max; // 6   2*2  LUT1 ramp min. and max.
    char notes[4];              // 10  4    no notes=0; has notes=non zero
    short byte_format;          // 14  2    bytes=TRUE(1); words=FALSE(0)
    short image_number;         // 16  2    image number within file
    char filename[32];          // 18  32   file name
    short merged;               // 50  2    merged format
    unsigned short color1;      // 52  2    LUT1 color status
    unsigned short file_id;     // 54  2    valid .PIC file=12345
    short ramp2_min, ramp2_max; // 56  2*2  LUT2 ramp min. and max.
    unsigned short color2;      // 60  2    LUT2 color status
    short edited;               // 62  2    image has been edited=TRUE(1)
    short lens;                 // 64  2    Integer part of lens magnification
    char mag_factor[4];         // 66  4    4 byte real mag. factor (old ver.)
    unsigned char reserved[6];  // 70  6    NOT USED (old ver.=real lens mag.)
};

BioRadImageIO::BioRadImageIO()
{
  this->SetNumberOfDimensions(3);
  m_PixelType = SCALAR;
  m_ComponentType = UCHAR;
  m_ByteOrder = LittleEndian;
  m_FileType = Binary;
  m_NumberOfComponents = 1; // default
}

BioRadImageIO::~BioRadImageIO()
{
}

bool BioRadImageIO::OpenBioRadFileForReading(std::ifstream& os, 
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
  os.open( filename, std::ios::in | std::ios::binary );

  if ( os.fail() )
    {
    return false;
    }

  return true;
}


bool BioRadImageIO::OpenBioRadFileForWriting(std::ofstream& os, 
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
  
  // Open the new file for writing
  itkDebugMacro(<< "Initialize: opening file " << filename);

#ifdef __sgi
  // Create the file. This is required on some older sgi's
  std::ofstream tFile(filename,std::ios::out);
  tFile.close();                    
#endif

  // Actually open the file
  os.open( filename, std::ios::out | std::ios::binary );

  if( os.fail() )
    {
    itkExceptionMacro(<< "Could not open file for writing: " << filename);
    return false;
    }

  return true;
}

// This method will only test if the header looks like a
// BioRad image file.
bool BioRadImageIO::CanReadFile(const char* filename) 
{ 
  std::ifstream file;
  std::string fname(filename);

  if( fname == "" )
    {
    itkDebugMacro(<<"No filename specified.");
    return false;
    }

  bool extensionFound = false;
  std::string::size_type sprPos = fname.rfind(".pic");
  if ((sprPos != std::string::npos)
      && (sprPos == fname.length() - 4))
    {
    extensionFound = true;
    }
  sprPos = fname.rfind(".PIC");
  if ((sprPos != std::string::npos)
      && (sprPos == fname.length() - 4))
    {
    extensionFound = true;
    }

  if( !extensionFound )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  if ( !this->OpenBioRadFileForReading(file, filename))
    {
    return false;
    }

  // Check to see if its a BioRad file
  unsigned short file_id;
  file.seekg(BIORAD_FILE_ID_OFFSET, std::ios::beg );
  file.read((char*)(&file_id),2);
  ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&file_id);

  itkDebugMacro(<< "Magic number: " << file_id);

  file.close();
  return file_id == BIORAD_MAGIC_NUMBER;
}
  

void BioRadImageIO::Read(void* buffer)
{
  std::ifstream file;

  //read header information file:
  this->OpenBioRadFileForReading(file, m_FileName.c_str());
  file.seekg(BIORAD_HEADER_LENGTH, std::ios::beg);

  if( !this->ReadBufferAsBinary( file, buffer, this->GetImageSizeInBytes()) )
    {
    itkExceptionMacro(<<"Read failed: Wanted " << this->GetImageSizeInBytes()
                      << " bytes, but read " << file.gcount() << " bytes.");
    }

  //byte swapping depending on pixel type:
  if(this->GetComponentType() == USHORT)
    {
    ByteSwapper<unsigned short>::SwapRangeFromSystemToLittleEndian(
        reinterpret_cast<unsigned short *>(buffer),
        this->GetImageSizeInComponents() );
    }

  //closing file:
  file.close();
}

void BioRadImageIO::InternalReadImageInformation(std::ifstream& file)
{
  //read .pic file (header)
  if ( ! this->OpenBioRadFileForReading(file, m_FileName.c_str()) )
    {
    itkExceptionMacro(<< "Cannot read requested file");
    }

  // Find info...
  bioradheader h, *p;
  p = &h;
  if( sizeof(h) != BIORAD_HEADER_LENGTH )
    {
    itkExceptionMacro(<< "Problem of alignement on your plateform");
    }
  file.seekg(0, std::ios::beg);
  file.read((char*)p, BIORAD_HEADER_LENGTH);
  ByteSwapper<unsigned short>::
    SwapRangeFromSystemToLittleEndian((unsigned short*)p, BIORAD_HEADER_LENGTH/2);
  itkDebugMacro(<< "Magic number: " << h.file_id);

  // Set dim X,Y,Z
  m_Dimensions[0] = h.nx;
  itkDebugMacro(<< "h.nx: " << h.nx);
  m_Dimensions[1] = h.ny;
  itkDebugMacro(<< "h.ny: " << h.ny);
  if( h.npic != 1 )
    {
    this->SetNumberOfDimensions(3);
    m_Dimensions[2] = h.npic;
    itkDebugMacro(<< "h.npic: " << h.npic);
    }
  else
    {
    this->SetNumberOfDimensions(2);
    }

  // These are not specified by the format, but we can deduce them:
  // pixel size = scale_factor/lens/mag_factor
  ByteSwapper<unsigned short>::SwapRangeFromSystemToLittleEndian((unsigned short*)&h.mag_factor,2);
  float mag_factor = *((float*)(h.mag_factor));
  ByteSwapper<float>::SwapFromSystemToLittleEndian(&mag_factor);
  itkDebugMacro(<< "Mag Factor: " << mag_factor);
  itkDebugMacro(<< "Lens: " << h.lens);
  m_Spacing[0] = m_Spacing[1] = mag_factor/h.lens;
  if (m_NumberOfDimensions == 3)
    {
    m_Spacing[2] = m_Spacing[0];
    }

  // Check the pixel size:
  itkDebugMacro(<< "Byte Format: " << h.byte_format);
  if(h.byte_format == 1)
    {
    SetComponentType(UCHAR);
    }
  else
    {
    // sometime the file set an erronous value for byte_format, check the size
    // of the file in this case, since byte_format = 1 seems to be the default
    file.seekg(0, std::ios::end);
    long gcount = static_cast<long>(file.tellg()) - BIORAD_HEADER_LENGTH;
    if( gcount == h.nx*h.ny*h.npic )
      {
      itkWarningMacro( << 
        "File is declared as two bytes but really is only one byte");
      SetComponentType(UCHAR);
      }
    else if( gcount == h.nx*h.ny*h.npic*2 )
      {
      SetComponentType(USHORT);
      }
    else
      {
      SetComponentType(UNKNOWNCOMPONENTTYPE);
      itkExceptionMacro(<< "Cannot read requested file");
      }
    }
}

void BioRadImageIO::ReadImageInformation()
{
  std::ifstream file;
  this->InternalReadImageInformation(file);
  file.close();
}


bool BioRadImageIO::CanWriteFile( const char* name )
{
  std::string filename = name;

  if(  filename == "" )
    {
    itkDebugMacro(<<"No filename specified.");
    return false;
    }

  bool extensionFound = false;
  std::string::size_type sprPos = filename.rfind(".pic");
  if ((sprPos != std::string::npos)
      && (sprPos == filename.length() - 4))
    {
    extensionFound = true;
    }

  if( !extensionFound )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  return true;
}


void BioRadImageIO::Write(const void* buffer)
{
  std::ofstream file;
  if ( !this->OpenBioRadFileForWriting(file, m_FileName.c_str()) )
    {
    return;
    }

  // Check the image region for proper dimensions, etc.
  unsigned int numDims = this->GetNumberOfDimensions();
  if ( numDims != 3 && numDims != 2 )
    {
    itkExceptionMacro(<<"BioRad Writer can only write 2 or 3-dimensional images");
    return;
    }

  // Write the BioRad header information
  bioradheader header, *p;
  p = &header;
  if( sizeof(header) != BIORAD_HEADER_LENGTH )
    {
    itkExceptionMacro(<< "Problem of alignement on your plateform");
    }
  memset(p,0,BIORAD_HEADER_LENGTH); // Set everything to zero
  // In particular `notes' needs to be set to zero to indicate there is no notes
  header.nx = m_Dimensions[0];
  header.ny = m_Dimensions[1];
  if( m_NumberOfDimensions == 3 )
    {
    header.npic = m_Dimensions[2];
    }
  else
    {
    header.npic = 1;
    }
  header.file_id = BIORAD_MAGIC_NUMBER;

  // Always say that image was not edited:
  header.edited = 0;

  // Default dummy values:
  header.lens = 1;
  // Set the if file is in byte format or not:
  switch(this->GetComponentType())
    {
    case UCHAR:
      header.byte_format = 1;
      header.ramp1_min = 0;
      header.ramp1_max = 255;
      header.ramp2_min = 0;
      header.ramp2_max = 255;
      break;
    case USHORT:
      header.byte_format = 0;
      header.ramp1_min = 0;
      header.ramp1_max = 65535;
      header.ramp2_min = 0;
      header.ramp2_max = 65535;
      break;
    default:
      itkExceptionMacro(<<"Component type not supported.");
      return;
    }
  // write the actual header
  ByteSwapper<unsigned short>::SwapRangeFromSystemToLittleEndian(
    reinterpret_cast<unsigned short*>(p), BIORAD_HEADER_LENGTH/2);
  // To be able to deduce pixel spacing:
  float mag_factor = m_Spacing[0];
  ByteSwapper<float>::SwapFromSystemToLittleEndian(&mag_factor);
  memcpy(&header.mag_factor, (char*)(&mag_factor), 4);
  // Set the filename
  // NOTES: This is not very clear what should be written here, some files
  // have either:
  // 1. FILENAME.PIC
  // 2. FILENAME.pic
  // 3. FileName.pic
  // or simply
  // 4. FileName
  std::string filename = itksys::SystemTools::GetFilenameName(m_FileName);
  // The buffer is at most 32 bytes:
  strncpy(header.filename, filename.c_str(), sizeof(header.filename)); 
  file.write((char*)p, BIORAD_HEADER_LENGTH);

  //preparation for writing buffer:
  const unsigned long numberOfBytes      = this->GetImageSizeInBytes();
  const unsigned long numberOfComponents = this->GetImageSizeInComponents();

  char *tempmemory = new char[numberOfBytes];
  memcpy(tempmemory,buffer,numberOfBytes);
  if(this->GetComponentType() == USHORT)
    {
    ByteSwapper<unsigned short>::SwapRangeFromSystemToBigEndian(
        reinterpret_cast<unsigned short*>(tempmemory), numberOfComponents );
    }
  
  // Write the actual pixel data
  file.write( static_cast<const char *>(tempmemory) , numberOfBytes );
  delete [] tempmemory;
  file.close();
}

void BioRadImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


} // end namespace itk
