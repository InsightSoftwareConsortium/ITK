/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStimulateImageIO.cxx
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
#include "itkStimulateImageIO.h"
#include <stdio.h>
#include <string.h>
#include "itkByteSwapper.h"

namespace itk
{

StimulateImageIO::StimulateImageIO()
{
  this->SetNumberOfDimensions(4);
  m_ByteOrder = BigEndian;
  m_FileType = Binary;
 
}

StimulateImageIO::~StimulateImageIO()
{
}

bool StimulateImageIO::OpenStimulateFileForReading(std::ifstream& os, 
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
    itkExceptionMacro(<< "Could not open file for reading: " << filename);
    return false;
    }

  return true;
}


bool StimulateImageIO::OpenStimulateFileForWriting(std::ofstream& os, 
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
//Stimulate image file.
bool StimulateImageIO::CanReadFile(const char* filename) 
{ 
  std::ifstream file;
  char buffer[256];
  std::string fname(filename);

  // First check the extension
  if ( fname.find(".spr") >= fname.length())
    {
       itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  if ( ! this->OpenStimulateFileForReading(file, filename))
    {
    return false;
    }

  // Check to see if its a spr file
  file.getline(buffer,255);

  fname = buffer;

  if ( fname.find("numDim:") < fname.length() ||
       fname.find("dim:") < fname.length() ||
       fname.find("dataType:") < fname.length() )
    {
    return true;
    }
  else
    {
    return false;
    }
}
  

void StimulateImageIO::Read(void* buffer)
{
  std::ifstream file;

  //read data information file:
  this->InternalReadImageInformation(file);
  
  //read data file, need another filename
  std::ifstream file_data;
  std::string data_filename = m_FileName;

  //determine datafile given sp filename
  data_filename.replace(data_filename.length() - 3, 3, "sdt" );

  if ( !this->OpenStimulateFileForReading(file_data, data_filename.c_str()))
    return;

  file_data.read((char*)buffer, this->GetImageSizeInBytes());
  if ( file_data.fail() )
  {
    itkExceptionMacro(<<"Read failed: Wanted " 
                        << this->GetImageSizeInBytes()
                        << " bytes, but read " 
                        << file_data.gcount() << " bytes.");
  }

  //byte swapping depending on pixel type:
  switch (this->GetComponentType())
  {
    case CHAR:
      ByteSwapper<char>::SwapRangeFromSystemToBigEndian((char *)buffer, this->GetImageSizeInComponents() );
    break;
    case SHORT:
      ByteSwapper<short>::SwapRangeFromSystemToBigEndian((short *)buffer, this->GetImageSizeInComponents() );
    break;
   case INT:
      ByteSwapper<int>::SwapRangeFromSystemToBigEndian((int *)buffer, this->GetImageSizeInComponents() );
    break;
   case FLOAT:
      ByteSwapper<float>::SwapRangeFromSystemToBigEndian((float *)buffer, this->GetImageSizeInComponents() );
    break;
   case DOUBLE:
      ByteSwapper<double>::SwapRangeFromSystemToBigEndian((double *)buffer, this->GetImageSizeInComponents() );
    break;
    default:
      ;
  }

  //closing files:
  file.close();
  file_data.close();
}

void StimulateImageIO::InternalReadImageInformation(std::ifstream& file)
{
  char line[255];
  std::string text;

  //we don't need for now to read .sdt file, only .spr
  if ( ! this->OpenStimulateFileForReading(file, m_FileName.c_str()) )
    {
    itkExceptionMacro(<< "Cannot read requested file");
    }

  //extract dimensions, spacing, origin
  unsigned int dim;
  unsigned int dims[4];
  float spacing[4];
  float origin[4];
  float extent[4];
  float fov[4];
  
  // set values in case we don't find them
  this->SetNumberOfDimensions(4);
  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;
  m_Spacing[2] = 1.0;
  m_Spacing[3] = 1.0;

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;
  m_Origin[2] = 0.0;
  m_Origin[3] = 0.0;

  char pixelType[256];
  float range[2];
  
  float thresh;   
  char fidName[256] = "";   
  char orient[256] = "";

  bool fov_specified = false;
  bool origin_specified = false;
  bool spacing_specified = false;
  do {
    file.getline(line,255);
    text = line;

    if ( text.find("numDim") < text.length())
    {
      sscanf(line, "%*s %d", &dim);
      this->SetNumberOfDimensions(dim);
    }
    else if ( text.find("dim") < text.length())
    {
      sscanf(line, "%*s %d %d %d %d", dims, dims+1, dims+2 , dims+3);
      if ( dims[3] <= 1 )
      {
        this->SetNumberOfDimensions(3);
      }
      if ( dims[2] <= 1 )
      {
        this->SetNumberOfDimensions(2);
      }
      for ( unsigned int i=0; i < m_NumberOfDimensions; i++ )
      {
        m_Dimensions[i] = dims[i];
      }
    }
    else if ( text.find("origin") < text.length())
    {
      //origin
      //Position of the center of the first voxel. One value for each dimension. If
      //the origin is not specified, but the fov is, then the image is assumed
      //to be centered:

      sscanf(line, "%*s %f %f %f %f", origin, origin+1, origin+2 , origin+3);
      for ( unsigned int i=0; i < m_NumberOfDimensions; i++ )
      {
        m_Origin[i] = origin[i];
      }
      origin_specified = true;
    }
    else if ( text.find("extent") < text.length())
    {
      sscanf(line, "%*s %f %f %f %f", extent, extent+1, extent+2 , extent+3);
      for ( unsigned int i=0; i < m_NumberOfDimensions; i++ )
      {
        m_Extent[i] = extent[i];
      }
    }
    else if ( text.find("fov") < text.length())
    {
      //fov
      //Field of view: The distance between the outside edges of the first and last
      //voxel along each dimension; one value for each dimension. If the fov is not
      //specified it is calculated according to: fov = interval * dim

      sscanf(line, "%*s %f %f %f %f", fov, fov+1, fov+2 , fov+3);
      fov_specified = true;
    }
    else if ( text.find("interval") < text.length())
    {
      //interval
      //The center to center distance between adjacent voxels along each dimension;
      //one value for each dimension. If the interval is not specified it is
      //calculated according to: interval = fov / dim*/

      sscanf(line, "%*s %f %f %f %f", spacing, spacing+1, spacing+2 , spacing+3);
      for ( unsigned int i=0; i < m_NumberOfDimensions; i++ )
      {
        m_Spacing[i] = spacing[i];
      }
      spacing_specified = true;
    }
    else if ( text.find("dataType") < text.length())
    {
    sscanf(line, "%*s %s", pixelType);
      text = pixelType;
      if ( text.find("BYTE") < text.length() )
        {
        SetPixelType(CHAR);
        SetComponentType(CHAR);
        }
      else if ( text.find("WORD") < text.length() )
        {
        SetPixelType(SHORT);
        SetComponentType(SHORT);
        }
      else if ( text.find("LWORD") < text.length() )
        {
        SetPixelType(INT);
        SetComponentType(INT);
        }
      else if ( text.find("REAL") < text.length() )
        {
        SetPixelType(FLOAT);
        SetComponentType(FLOAT);
        }
      else if ( text.find("COMPLEX") < text.length() )
        {
        SetPixelType(DOUBLE);
        SetComponentType(DOUBLE);
        }
      else
        {
        itkExceptionMacro(<<"Unrecognized type");
        }
      }//found scalars
    else if ( text.find("displayRange") < text.length())
    {
      //displayRange:
      //Two values giving the low_value and high_value. Voxel values below the
      //low_value will be displayed as black and voxels with values above the
      //high_value will be displayed as white. Voxels with values within the display
      //range are displayed with a grey value that is scaled linearly between the
      //low_value and high_value.

      sscanf(line, "%*s %f %f", range, range+1);
      m_DisplayRange[0] = range[0];
      m_DisplayRange[1] = range[1];
    }
    else if ( text.find("fidName") < text.length())
    {
      //Not used
      //This is a bit tricky to get the value as there is sometime no black space
      //only a ':' separate field from value, we assume there is no other ':'
      char *pch;
      pch = strchr(line,':');
      sscanf(++pch, "%s", m_FidName);  //delete any blank space left
    }
    else if ( text.find("sdtOrient") < text.length())
    {
      //Not used now, but later when  ITK Dictionary will be ready
      //don't know for now the format in which to save this.
      //This is a bit tricky to get the value as there is sometime no black space
      //only a ':' separate field from value, we assume there is no other ':'
      char *pch;
      pch = strchr(line,':');
      sscanf(++pch, "%s", m_SdtOrient);  //delete any blank space left
    }
    else if ( text.find("dsplyThres") < text.length())
    {
      //not used
      sscanf(line, "%*s %f", &thresh);
     m_DisplayThresh = thresh;
    }
    else if ( text.find("endian") < text.length())
    {
    //not used
    //endian:ieee-be      -> BigEndian / LittleEndian: ieee-le
   }
    else if ( text.find("mapParmFileName") < text.length())
   {
    //not used
      //mapParmFileName:study1.mp
    }
    else if ( text.find("mapTypeName") < text.length())
    {
    //not used
    //mapTypeName:t-val
    }
    else if ( text.find("stimFileName") < text.length())
    {
    //not used
    //stimFileName:cf021012-1-5.sdt
    }
    else if ( text.find("mapConf") < text.length())
    {
    //not used
    //mapConf:  0.95000
    }
    else if ( text.find("periodStr") < text.length())
    {
    //not used
    //periodStr:  -1.00000 ...
    }
  } while (!file.eof() );


  //compute any missing informations:
  if( !spacing_specified && fov_specified) {
    for ( unsigned int i=0; i < m_NumberOfDimensions; i++ )
    {
      m_Spacing[i] = fov[i]/dims[i];
    }
  }
  if( !origin_specified && fov_specified) {
    for ( unsigned int i=0; i < m_NumberOfDimensions; i++ )
    {
      m_Origin[i] = (m_Spacing[i] - fov[i])/2.;
    }
  }
}

void StimulateImageIO::ReadImageInformation()
{
  std::ifstream file;
  this->InternalReadImageInformation(file);
}


bool StimulateImageIO::CanWriteFile( const char* name )
{
  std::string filename = name;
  if ( filename != "" &&
       filename.find(".spr") < filename.length() )
    {
    return true;
    }
  return false;
}



void StimulateImageIO::Write(const void* buffer)
{
  std::ofstream file;
  if ( ! this->OpenStimulateFileForWriting(file,m_FileName.c_str()) )
    {
    return;
    }

  // Check the image region for proper dimensions, etc.
  unsigned int numDims = this->GetNumberOfDimensions();
  if ( numDims < 2 || numDims > 4 )
    {
    itkExceptionMacro(<<"Stimulate Writer can only write 2,3 or 4-dimensional images");
    return;
    }
  ImageIORegion ioRegion = this->GetIORegion();

  // Write the VTK header information
  file << "numDim: " <<  this->GetNumberOfDimensions();

  // Write characteristics of the data
  file << "\ndim:";
  for(unsigned int i=0; i < m_NumberOfDimensions; i++)
  {
    file << " " << m_Dimensions[i];
  }

  file << "\norigin: ";
  for(unsigned int i=0; i < m_NumberOfDimensions; i++)
  {
    file << " " << m_Origin[i] ;
  }

  file << "\nextent: ";
  for(unsigned int i=0; i < m_NumberOfDimensions; i++)
  {
    file << " " << m_Extent[i];
  }

  file << "\nfov: ";
  for(unsigned int i=0; i < m_NumberOfDimensions; i++)
  {
    file << " " << m_Spacing[i]*m_Dimensions[i]; //fov = interval * dim
  }

  file << "\ninterval: ";
  for(unsigned int i=0; i < m_NumberOfDimensions; i++)
  {
    file << " " << m_Spacing[i];
  }

  //preparation for writing buffer:
  const unsigned long numberOfBytes      = this->GetImageSizeInBytes();
  const unsigned long numberOfComponents = this->GetImageSizeInComponents();

  file << "\ndataType: ";
  switch(this->GetComponentType()) {
    case CHAR:
    file << "BYTE";
      ByteSwapper<char>::SwapRangeFromSystemToBigEndian((char *)buffer, numberOfComponents );
    break;
  case SHORT:
    file << "WORD";
      ByteSwapper<short>::SwapRangeFromSystemToBigEndian((short *)buffer, numberOfComponents );
    break;
  case INT:
    file << "LWORD";
      ByteSwapper<int>::SwapRangeFromSystemToBigEndian((int *)buffer, numberOfComponents );
    break;
  case FLOAT:
    file << "REAL";
      ByteSwapper<float>::SwapRangeFromSystemToBigEndian((float *)buffer, numberOfComponents );
    break;
  case DOUBLE:
    file << "COMPLEX";
      ByteSwapper<double>::SwapRangeFromSystemToBigEndian((double *)buffer, numberOfComponents );
    break;
  default:
    ;
  }

  file << "\ndisplayRange: " << m_DisplayRange[0] << " " << m_DisplayRange[1];

  file << "\nfidName: " << m_FidName;
  
  file << "\nsdtOrient: " << m_SdtOrient;
  
  file << "\ndsplyThres: " << m_DisplayThresh;
  
  //Last carrier return:
  file << "\n";

  //read data file, need another filename
  std::ofstream file_data;
  std::string data_filename = m_FileName;

  //determine datafile given spr filename
  data_filename.replace(data_filename.length() - 3, 3, "sdt" );

  if ( ! this->OpenStimulateFileForWriting(file_data, data_filename.c_str()) )
    {
    return;
    }

  // Write the actual pixel data
  file_data.write( (char *)buffer , numberOfBytes );

  file.close();
  file_data.close();
}

void StimulateImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


} // end namespace itk
