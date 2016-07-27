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

#include "itkImageIOFactory.h"

namespace itk
{

FileListVideoIO::FileListVideoIO()
{
  this->ResetMembers();
}

FileListVideoIO::~FileListVideoIO()
{
  this->FinishReadingOrWriting();
}

void FileListVideoIO::FinishReadingOrWriting()
{
  this->ResetMembers();
}

void FileListVideoIO::SetFileName(const std::string& fileList)
{
  this->SetFileName(fileList.c_str() );
}

void FileListVideoIO::SetFileName(const char* fileList)
{
  m_FileNames = this->SplitFileNames(fileList);

  // Set the number of frames
  m_FrameTotal = static_cast<SizeValueType>( m_FileNames.size() );

  this->Modified();
}

std::vector<std::string>
FileListVideoIO
::SplitFileNames(const std::string& fileList)
{
  std::vector<std::string> out;

  size_t pos = 0;
  size_t len = fileList.length();
  while (pos != std::string::npos && len > 0)
    {
    // Get the substring
    std::string str = fileList.substr(pos, len);

    // Update pos
    pos = str.find(',');

    // Add the filename to the list
    out.push_back( str.substr(0,pos) );

    // Move past the delimiter
    if (pos != std::string::npos)
      {
      pos++;
      }
    len -= pos;
    }

  return out;
}

void FileListVideoIO::SetReadFromFile()
{
  if (!m_ReaderOpen && !m_WriterOpen)
    {
    m_ReadType = ReadFromFile;
    }
  else
    {
    itkExceptionMacro("Cannot change read type while reader is open");
    }
}

void FileListVideoIO::SetReadFromCamera()
{
  itkExceptionMacro("Read From Camera is not supported by this VideoIO");
}

bool FileListVideoIO::CanReadFile(const char* filename)
{
  // Make sure file names have been specified
  std::string strFileName = filename;
  std::vector<std::string> fileList = this->SplitFileNames(strFileName);
  if (fileList.empty())
    {
    return false;
    }

  // Make sure all file names have the same extension
  if( !VerifyExtensions(fileList) )
    {
    return false;
    }

  // Make sure we can instantiate an ImageIO to read the first file
  ImageIOBase::Pointer ioTemp = ImageIOFactory::CreateImageIO(
      fileList[0].c_str(), ImageIOFactory::ReadMode);
  if (ioTemp.IsNull() )
    {
    return false;
    }

  return true;
}

bool FileListVideoIO::CanReadCamera( itk::SizeValueType itkNotUsed(cameraID) )const
{
  return false;
}

void FileListVideoIO::ReadImageInformation()
{
  // Open from a file
  if (m_ReadType == ReadFromFile)
    {

    // Make sure file can be read
    std::string filename = m_FileNames[0];
    if (!this->CanReadFile(filename.c_str() ) )
      {
      itkExceptionMacro(<< "Cannot Read File: " << filename);
      }

    // Open the internal IO (don't use local so that spacing, origin, direction
    // can be retrieved)
    if (!m_ReaderOpen)
      {
      this->OpenReader();
      }

    m_ImageIO->SetFileName( m_FileNames[0].c_str() );
    m_ImageIO->ReadImageInformation();

    // No I-Frame issues to worry about
    m_IFrameInterval = 1;
    m_LastIFrame = m_FrameTotal-1;

    // Fill dimensions and origin
    unsigned int numberOfDimensions = m_ImageIO->GetNumberOfDimensions();
    this->SetNumberOfDimensions( numberOfDimensions );

    for (unsigned int i = 0; i < numberOfDimensions; ++i)
      {
      m_Dimensions[i] = m_ImageIO->GetDimensions(i);
      m_Origin.push_back( m_ImageIO->GetOrigin(i) );
      }

    // Get other image info
    m_NumberOfComponents = m_ImageIO->GetNumberOfComponents();
    m_ComponentType = m_ImageIO->GetComponentType();
    m_PixelType = m_ImageIO->GetPixelType();

    // Make up an arbitrary FramesPerSecond of 1
    m_FramesPerSecond = 1;

    }

  // Open capture from a camera
  else if (m_ReadType == ReadFromCamera)
    {
    itkExceptionMacro("FileListVideoIO cannot read from a camera");
    }

  // Should never get here
  else
    {
    itkExceptionMacro("Invalid Read Type... How did we get here?");
    }
}

void FileListVideoIO::Read(void *buffer)
{
  // Make sure we've already called ReadImageInformation (dimensions are
  // non-zero)
  if (m_Dimensions[0] == 0)
    {
    itkExceptionMacro(<< "Cannot read frame with zero dimension. May need to call ReadImageInformation");
    }

  // If video is not already open, open it and keep it open
  if (!m_ReaderOpen)
    {
    this->OpenReader();
    }

  // Read the desired frame
  m_ImageIO->SetFileName(m_FileNames[m_CurrentFrame]);
  m_ImageIO->Read(buffer);

  // Move on to the next frame
  if (m_CurrentFrame < m_FrameTotal - 1)
    {
    m_CurrentFrame++;
    }
}

bool FileListVideoIO::SetNextFrameToRead(FrameOffsetType frameNumber)
{
  if (frameNumber >= m_FrameTotal)
    {
    return false;
    }

  m_CurrentFrame = frameNumber;
  return true;
}

double FileListVideoIO::GetSpacing(unsigned int i) const
{
  if (!m_ReaderOpen)
    {
    itkExceptionMacro("Can't get Spacing without ImageIO open for reading");
    }
  return m_ImageIO->GetSpacing(i);
}

double FileListVideoIO::GetOrigin(unsigned int i) const
{
  if (!m_ReaderOpen)
    {
    itkExceptionMacro("Can't get Origin without ImageIO open for reading");
    }
  return m_ImageIO->GetOrigin(i);
}

std::vector< double > FileListVideoIO::GetDirection(unsigned int i) const
{
  if (!m_ReaderOpen)
    {
    itkExceptionMacro("Can't get Direction without ImageIO open for reading");
    }
  return m_ImageIO->GetDirection(i);
}

bool FileListVideoIO::CanWriteFile(const char* filename)
{
  // Make sure file names have been specified
  std::vector<std::string> fileList = this->SplitFileNames(filename);
  if (fileList.empty())
    {
    return false;
    }

  // Make sure all file names have the same extension
  if( !VerifyExtensions(fileList) )
    {
    return false;
    }

  // Make sure we can instantiate an ImageIO to write the first file
  ImageIOBase::Pointer ioTemp = ImageIOFactory::CreateImageIO(
      fileList[0].c_str(), ImageIOFactory::WriteMode);
  if (ioTemp.IsNull() )
    {
    return false;
    }

  return true;
}

void FileListVideoIO::WriteImageInformation()
{
  if (!m_WriterOpen)
    {
    this->OpenWriter();
    }
  m_ImageIO->SetFileName(m_FileNames[m_CurrentFrame]);
  m_ImageIO->WriteImageInformation();
}

void FileListVideoIO::SetWriterParameters(
  TemporalRatioType framesPerSecond,
  const std::vector<SizeValueType>& dim,
  const char* itkNotUsed(fourCC),
  unsigned int nChannels,
  IOComponentType itkNotUsed(componentType) )
{
  m_Dimensions.clear();
  m_Origin.clear();
  m_NumberOfComponents = nChannels;
  for (size_t i = 0; i < dim.size(); ++i)
    {
    m_Dimensions.push_back(dim[i]);
    m_Origin.push_back(0);
    }

  // Set FramesPerSecond even though we're not going to use it
  m_FramesPerSecond = framesPerSecond;

}

void FileListVideoIO::Write(const void *buffer)
{

  // Make sure parameters are specified
  if (m_Dimensions.empty())
    {
    itkExceptionMacro("Cannot write with empty parameters. You probably need to call SetWriterParameters");
    }

  // If the writer isn't open yet, open it
  if (!m_WriterOpen)
    {
    this->OpenWriter();
    }

  // Set the properties to the ImageIO
  m_ImageIO->SetNumberOfDimensions( static_cast<unsigned int>( m_Dimensions.size() ) );
  for (size_t i = 0; i < m_Dimensions.size(); ++i)
    {
    m_ImageIO->SetDimensions(static_cast<unsigned int>( i ), m_Dimensions[i]);
    m_ImageIO->SetOrigin(static_cast<unsigned int>( i ), m_Origin[i]);
    }
  m_ImageIO->SetNumberOfComponents(m_NumberOfComponents);

  // Write with the ImageIO
  m_ImageIO->SetFileName(m_FileNames[m_CurrentFrame]);
  m_ImageIO->Write(buffer);

  // Move to the next frame
  if (m_CurrentFrame < m_FrameTotal-1)
    {
    m_CurrentFrame++;
    }

}

void FileListVideoIO::OpenReader()
{
  if (m_ReaderOpen)
    {
    itkExceptionMacro("Cannot open reader while video is already open for reading");
    }

  if (m_WriterOpen)
    {
    itkExceptionMacro("Cannot open reader while video is already open for writing");
    }

  // Make sure FileNames have been specified
  if (m_FileNames.empty())
    {
    itkExceptionMacro("Cannot open reader without file names set");
    }

  // If neither reader nor writer is currently open, open the reader
  if (m_ReadType == ReadFromFile)
    {
    // Use the ImageIOFactory to instantiate a working ImageIO
    m_ImageIO = ImageIOFactory::CreateImageIO(
        m_FileNames[0].c_str(), ImageIOFactory::ReadMode);
    if (!m_ImageIO.IsNull() )
      {
      m_ReaderOpen = true;
      }
    else
      {
      itkExceptionMacro("Video failed to open");
      }
    }
  else if (m_ReadType == ReadFromCamera)
    {
    itkExceptionMacro("FileListVideoIO doesn't support reading from camera");
    }
  else
    {
    itkExceptionMacro("FileListVideoIO doesn't support reading from unknown "
                      "ReadType");
    }
}

void FileListVideoIO::OpenWriter()
{
  if (m_WriterOpen)
    {
    itkExceptionMacro("Cannot open writer while video is already open for writing");
    }

  if (m_ReaderOpen)
    {
    itkExceptionMacro("Cannot open writer while video is already open for reading");
    }

  // Make sure FileNames have been specified
  if (m_FileNames.size() == 0)
    {
    itkExceptionMacro("Cannot open reader without file names set");
    }

  // If neither reader nor writer is currently open, open the writer
  m_ImageIO = ImageIOFactory::CreateImageIO(
      m_FileNames[0].c_str(), ImageIOFactory::WriteMode);
  if (!m_ImageIO.IsNull() )
    {
    m_WriterOpen = true;
    }
  else
    {
    itkExceptionMacro("File failed to open for writing");
    }
}

void FileListVideoIO::ResetMembers()
{
  m_ImageIO = ITK_NULLPTR;
  m_FileNames.clear();
  m_WriterOpen = false;
  m_ReaderOpen = false;
  m_FramesPerSecond = 0;
  m_Dimensions.clear();
  m_FrameTotal = 0;
  m_CurrentFrame = 0;
  m_NumberOfComponents = 0;
  m_IFrameInterval = 0;
  m_LastIFrame = 0;

  // Default to reading from a file
  m_ReadType = ReadFromFile;

  // Members from ImageIOBase
  m_PixelType = SCALAR;
  m_ComponentType = UCHAR;
  this->SetNumberOfDimensions(2);
  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;
  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;
}

bool
FileListVideoIO
::VerifyExtensions( const std::vector<std::string>& fileList ) const
{
  for (size_t i = 1; i < fileList.size(); ++i)
    {
    size_t prevExtPos = fileList[i-1].rfind(".");
    size_t extPos = fileList[i].rfind(".");
    if (prevExtPos == std::string::npos || extPos == std::string::npos)
      {
      return false;
      }
    std::string prevExt =
      fileList[i-1].substr( prevExtPos + 1,
                            fileList[i-1].length()-prevExtPos-1 );
    std::string ext =
      fileList[i].substr( extPos + 1,
                          fileList[i].length()-extPos-1 );

    if (strcmp(prevExt.c_str(), ext.c_str() ) )
      {
      return false;
      }
    }
  return true;
}

void FileListVideoIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if ( !m_ImageIO.IsNull() )
    {
    os << indent << "Internal ImageIO:" << std::endl;
    m_ImageIO->Print( os, indent.GetNextIndent() );
    }

  os << indent << "Image filenames:" << std::endl;
  std::vector<std::string>::const_iterator it = m_FileNames.begin();
  while( it != m_FileNames.end() )
    {
    os << indent << *it << std::endl;
    ++it;
    }

}

} // end namespace itk
