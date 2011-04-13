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
#include "itkFileListVideoIO.h"

#include "itkImage.h"
#include "itkImportImageFilter.h"
#include "itkImageIOFactory.h"

namespace itk
{

///////////////////////////////////////////////////////////////////////////////
// Constructor, Destructor, and Print
//


//
// Constructor
//
FileListVideoIO::FileListVideoIO()
{
  this->ResetMembers();
}


//
// Destructor
//
FileListVideoIO::~FileListVideoIO()
{
  this->FinishReadingOrWriting();
}


//
// PrintSelf
//
void FileListVideoIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  if (!this->m_ImageIO.IsNull())
    {
    os << indent << "Internal ImageIO:" << std::endl;
    this->m_ImageIO->Print(os, indent.GetNextIndent());
    }
}

//
// FinishReadingOrWriting
//
void FileListVideoIO::FinishReadingOrWriting()
{
  this->ResetMembers();

}


///////////////////////////////////////////////////////////////////////////////
// Member Accessors
//


//
// GetPositionInMSec
//
double FileListVideoIO::GetPositionInMSec()
{
  return this->m_PositionInMSec;
}

//
// GetRatio
//
double FileListVideoIO::GetRatio()
{
  return this->m_Ratio;
}

//
// GetFrameTotal
//
unsigned long FileListVideoIO::GetFrameTotal()
{
  return this->m_FrameTotal;
}

//
// GetFpS
//
double FileListVideoIO::GetFpS()
{
  return this->m_FpS;
}

//
// GetCurrentFrame
//
unsigned long FileListVideoIO::GetCurrentFrame()
{
  return this->m_CurrentFrame;
}

//
// GetIFrameInterval
//
unsigned int FileListVideoIO::GetIFrameInterval()
{
  return this->m_IFrameInterval;
}

//
// GetLastIFrame
//
unsigned long FileListVideoIO::GetLastIFrame()
{
  return this->m_LastIFrame;
}


//
// SetFileName -- Split list based on ';'
//
void FileListVideoIO::SetFileName(const char* fileList)
{
  this->m_FileNames = SplitFileNames(fileList);

  // Set the number of frames
  this->m_FrameTotal = this->m_FileNames.size();
}

///////////////////////////////////////////////////////////////////////////////
// Read related methods
//


//
// SetReadFromFile
//
void FileListVideoIO::SetReadFromFile()
{
  if (!this->m_ReaderOpen && !this->m_WriterOpen)
    {
    this->m_ReadType = ReadFromFile;
    }
  else
    {
    itkExceptionMacro("Cannot change read type while reader is open");
    }
}

//
// SetReadFromCamera
//
void FileListVideoIO::SetReadFromCamera()
{
  itkExceptionMacro("Read From Camera is not supported by this VideoIO");
}

//
// CanReadFile
//
bool FileListVideoIO::CanReadFile(const char* filename)
{
  // Make sure file names have been specified
  std::vector<std::string> fileList = SplitFileNames(filename);
  if (fileList.size() == 0)
    {
    return false;
    }

  // Make sure all file names have the same extension
  for (unsigned int i = 1; i < fileList.size(); ++i)
    {
    int prevExtPos = fileList[i-1].rfind(".");
    int extPos = fileList[i].rfind(".");
    if (prevExtPos == -1 || extPos == -1)
      {
      return false;
      }
    std::string prevExt = fileList[i-1].substr(prevExtPos + 1, fileList[i-1].length()-prevExtPos-1);
    std::string ext = fileList[i].substr(extPos + 1, fileList[i].length()-extPos-1);

    if (strcmp(prevExt.c_str(), ext.c_str()))
      {
      return false;
      }
    }

  // Make sure we can instantiate an ImageIO to read the first file
  ImageIOBase::Pointer ioTemp = ImageIOFactory::CreateImageIO(
      fileList[0].c_str(), ImageIOFactory::ReadMode);
  if (ioTemp.IsNull())
    {
    return false;
    }

  return true;
}

//
// CanReadCamera
//
bool FileListVideoIO::CanReadCamera( unsigned long cameraID )
{
  return false;
}


//
// ReadImageInformation
//
void FileListVideoIO::ReadImageInformation()
{
  // Open capture from a file
  if (this->m_ReadType == ReadFromFile)
    {

    // Make sure file can be read
    std::string filename = this->m_FileNames[0];
    if (!this->CanReadFile(filename.c_str()))
      {
      itkExceptionMacro(<< "Cannot Read File: " << filename);
      }


    // Open the video file
    ImageIOBase::Pointer localIO = ImageIOFactory::CreateImageIO(
      this->m_FileNames[0].c_str(), ImageIOFactory::ReadMode);

    localIO->SetFileName(this->m_FileNames[0].c_str());
    localIO->ReadImageInformation();

    // No I-Frame issues to worry about
    this->m_IFrameInterval = 1;
    this->m_LastIFrame = this->m_FrameTotal;

    // Fill Dimensions and Origin
    this->m_Dimensions.clear();
    this->m_Origin.clear();
    this->SetNumberOfDimensions(localIO->GetNumberOfDimensions());
    for (unsigned int i = 0; i < localIO->GetNumberOfDimensions(); ++i)
      {
      this->m_Dimensions.push_back(localIO->GetDimensions(i));
      this->m_Origin.push_back(localIO->GetOrigin(i));
      }

    // Get other image info
    this->m_NumberOfComponents = localIO->GetNumberOfComponents();
    this->m_ComponentType = localIO->GetComponentType();
    this->m_PixelType = localIO->GetPixelType();

    // Make up an arbitrary FpS of 1
    this->m_FpS = 1;

    }

  // Open capture from a camera
  else if (this->m_ReadType == ReadFromCamera)
    {
    itkExceptionMacro("FileListVideoIO cannot read from a camera");
    }

  // Should never get here
  else
    {
    itkExceptionMacro("Invalid Read Type... How did we get here?");
    }
}

//
// Read
//
void FileListVideoIO::Read(void *buffer)
{
  // Make sure we've already called ReadImageInformation (dimensions are non-zero)
  if (this->m_Dimensions[0] == 0)
    {
    itkExceptionMacro(<< "Cannot read frame with zero dimension. May need to call ReadImageInformation");
    }

  // If video is not already open, open it and keep it open
  if (!this->m_ReaderOpen)
    {
    this->OpenReader();
    }

  // Read the desired frame
  this->m_ImageIO->SetFileName(this->m_FileNames[this->m_CurrentFrame]);
  this->m_ImageIO->Read(buffer);

  // Move on to the next frame
  if (this->m_CurrentFrame < this->m_FrameTotal - 1)
    {
    this->m_CurrentFrame++;
    }
}


//
// SetNextFrameToRead
//
bool FileListVideoIO::SetNextFrameToRead(unsigned long frameNumber)
{

  if (frameNumber >= this->m_FrameTotal)
    {
    return false;
    }

  this->m_CurrentFrame = frameNumber;
  return true;
}



///////////////////////////////////////////////////////////////////////////////
// Write related methods
//


//
// CanWriteFile
//
bool FileListVideoIO::CanWriteFile(const char* filename)
{

  // Make sure file names have been specified
  std::vector<std::string> fileList = SplitFileNames(filename);
  if (fileList.size() == 0)
    {
    return false;
    }

  // Make sure all file names have the same extension
  for (unsigned int i = 1; i < fileList.size(); ++i)
    {
    int prevExtPos = fileList[i-1].rfind(".");
    int extPos = fileList[i].rfind(".");
    if (prevExtPos == -1 || extPos == -1)
      {
      return false;
      }
    std::string prevExt = fileList[i-1].substr(prevExtPos + 1, fileList[i-1].length()-prevExtPos-1);
    std::string ext = fileList[i].substr(extPos + 1, fileList[i].length()-extPos-1);

    if (strcmp(prevExt.c_str(), ext.c_str()))
      {
      return false;
      }
    }

  // Make sure we can instantiate an ImageIO to write the first file
  ImageIOBase::Pointer ioTemp = ImageIOFactory::CreateImageIO(
      fileList[0].c_str(), ImageIOFactory::WriteMode);
  if (ioTemp.IsNull())
    {
    return false;
    }

  return true;
}

//
// WriteImageInformation
//
void FileListVideoIO::WriteImageInformation()
{
  if (!this->m_WriterOpen)
    {
    this->OpenWriter();
    }
  this->m_ImageIO->SetFileName(this->m_FileNames[this->m_CurrentFrame]);
  this->m_ImageIO->WriteImageInformation();
}

//
// SetWriterParameters
//
void FileListVideoIO::SetWriterParameters(double fps, std::vector<SizeValueType> dim,
                                        const char* fourCC, unsigned int nChannels,
                                        IOComponentType componentType)
{
  this->m_Dimensions.clear();
  this->m_Origin.clear();
  this->m_NumberOfComponents = nChannels;
  for (unsigned int i = 0; i < dim.size(); ++i)
    {
    this->m_Dimensions.push_back(dim[i]);
    this->m_Origin.push_back(0);
    }
  
}

//
// Write
//
void FileListVideoIO::Write(const void *buffer)
{

  // Make sure parameters are specified
  if (this->m_Dimensions.size() == 0)
    {
    itkExceptionMacro("Can not write with empty parameters. You probably need to call SetWriterParameters");
    }

  // If the writer isn't open yet, open it
  if (!this->m_WriterOpen)
    {
    this->OpenWriter();
    }

  // Set the properties to the ImageIO
  this->m_ImageIO->SetNumberOfDimensions(this->m_Dimensions.size());
  for (unsigned int i = 0; i < this->m_Dimensions.size(); ++i)
    {
    this->m_ImageIO->SetDimensions(i, this->m_Dimensions[i]);
    this->m_ImageIO->SetOrigin(i, this->m_Origin[i]);
    }
  this->m_ImageIO->SetNumberOfComponents(this->m_NumberOfComponents);

  // Write with the ImageIO
  this->m_ImageIO->SetFileName(this->m_FileNames[this->m_CurrentFrame]);
  this->m_ImageIO->Write(buffer);

  // Move to the next frame
  if (this->m_CurrentFrame < this->m_FrameTotal-1)
    {
    this->m_CurrentFrame++;
    }

}


///////////////////////////////////////////////////////////////////////////////
// Protected methods
//

//
// OpenReader
//
void FileListVideoIO::OpenReader()
{
  if (this->m_ReaderOpen)
    {
    itkExceptionMacro("Can not open reader while video is already open for reading");
    }

  if (this->m_WriterOpen)
    {
    itkExceptionMacro("Can not open reader while video is already open for writing");
    }

  // Make sure FileNames have been specified
  if (this->m_FileNames.size() == 0)
    {
    itkExceptionMacro("Can not open reader without file names set");
    }

  // If neither reader nor writer is currently open, open the reader
  if (this->m_ReadType == ReadFromFile)
    {
    // Use the ImageIOFactory to instantiate a working ImageIO
    this->m_ImageIO = ImageIOFactory::CreateImageIO(
      this->m_FileNames[0].c_str(), ImageIOFactory::ReadMode);
    if (!this->m_ImageIO.IsNull())
      {
      this->m_ReaderOpen = true;
      }
    else
      {
      itkExceptionMacro("Video failed to open");
      }
    }
  else if (this->m_ReadType == ReadFromCamera)
    {
    itkExceptionMacro("FileListVideoIO doesn't support reading from camera");
    }
}

//
// OpenWriter
//
void FileListVideoIO::OpenWriter()
{
  if (this->m_WriterOpen)
    {
    itkExceptionMacro("Can not open writer while video is already open for writing");
    }

  if (this->m_ReaderOpen)
    {
    itkExceptionMacro("Can not open writer while video is already open for reading");
    }

  // Make sure FileNames have been specified
  if (this->m_FileNames.size() == 0)
    {
    itkExceptionMacro("Can not open reader without file names set");
    }

  // If neither reader nor writer is currently open, open the writer
  this->m_ImageIO = ImageIOFactory::CreateImageIO(
    this->m_FileNames[0].c_str(), ImageIOFactory::WriteMode);
  if (!this->m_ImageIO.IsNull())
    {
    this->m_WriterOpen = true;
    }
  else
    {
    itkExceptionMacro("File failed to open for writing");
    }
}

//
// SplitFileNames ';'
//
std::vector<std::string> FileListVideoIO::SplitFileNames(const char* fileList)
{
  std::string str = fileList;

  std::vector<std::string> out;

  int pos = 0;
  int len = str.length();
  while (pos != -1 && len > 0)
    {
    // Get the substring
    str = str.substr(pos, len);

    // Update pos
    pos = str.find(',');

    // Add the filename to the list
    out.push_back(str.substr(0,pos));

    // Move past the delimiter
    if (pos != -1)
      {
      pos++;
      }
    len -= pos;
    }

  return out;
}


//
// ResetMembers
//
void FileListVideoIO::ResetMembers()
{
  this->m_ImageIO = NULL;
  this->m_FileNames.clear();
  this->m_WriterOpen = false;
  this->m_ReaderOpen = false;
  this->m_FpS = 0;
  this->m_Dimensions.clear();
  this->m_FrameTotal = 0;
  this->m_CurrentFrame = 0;
  this->m_NumberOfComponents = 0;
  this->m_IFrameInterval = 0;
  this->m_LastIFrame = 0;

  // Default to reading from a file
  this->m_ReadType = ReadFromFile;

  // Members from ImageIOBase
  this->m_PixelType = SCALAR;
  this->m_ComponentType = UCHAR;
  this->SetNumberOfDimensions(2);
  this->m_Spacing[0] = 1.0;
  this->m_Spacing[1] = 1.0;
  this->m_Origin[0] = 0.0;
  this->m_Origin[1] = 0.0;
}


} // end namespace itk
