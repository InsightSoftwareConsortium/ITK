/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkFDFImageIO.h"
#include "itkFDFCommonImageIO.h"

#include "itkByteSwapper.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "vnl/vnl_matrix.h"
#include <cstdio>
#include <fstream>

namespace itk
{

bool
FDFImageIO::CanReadFile(const char * file)
{
  this->SetFileName(file);

  // First check the extension
  std::string filename = file;
  if (filename.empty())
  {
    itkDebugMacro(<< "No filename specified.");
    return false;
  }

  bool                   extensionFound = false;
  std::string::size_type FDFPos = filename.rfind(".fdf");
  if ((FDFPos != std::string::npos) && (FDFPos == filename.length() - 4))
  {
    extensionFound = true;
  }

  FDFPos = filename.rfind(".FDF");
  if ((FDFPos != std::string::npos) && (FDFPos == filename.length() - 4))
  {
    extensionFound = true;
  }

  if (!extensionFound)
  {
    itkDebugMacro(<< "The filename extension is not recognized");
    return false;
  }

  std::ifstream inFile;
  inFile.open(m_FileName.c_str(), std::ios::in | std::ios::binary);
  if (!inFile)
  {
    ExceptionObject exception(__FILE__, __LINE__);
    std::string     msg = "File \"" + m_FileName + "\" cannot be read.";
    exception.SetDescription(msg.c_str());
    throw exception;
  }

  // Check for a neccessary header variable


  return true;
}

void
FDFImageIO::ReadImageInformation()
{
  if (!this->CanReadFile(m_FileName.c_str()))
    RAISE_EXCEPTION();

  std::string              line;
  std::vector<std::string> tokens;
  std::string              type, name, value;

  ImageIORegion region;

  std::ifstream inFile(m_FileName.c_str(), std::ios::in | std::ios::binary);

  // Check if there was an error opening the file
  if (!inFile)
  {
    std::cout << "Unable to open the file\n";
    RAISE_EXCEPTION();
  }

  this->SetFileTypeToBinary();

  while (getline(inFile, line, '\n'))
  {
    if (line == "\0")
    {
      break;
    }

    // Formats the lines in the FDF header such as removing whitespace between {}
    line = ParseLine(line);
    Tokenize(line, tokens, " ;");

    if (tokens.size() == 4)
    {
      type = tokens[0];
      name = tokens[1];
      value = tokens[3];

      if (name == "spatial_rank")
      {
        this->m_SpatialRank = value;
      }

      if (name == "matrix")
      {
        std::vector<float> dimensions;
        StringToVector(value, dimensions);

        // Set the number of dimensions
        if (this->GetNumberOfDimensions() < dimensions.size())
        {
          this->SetNumberOfDimensions(dimensions.size());
        }

        ImageIORegion::SizeType  size(dimensions.size());
        ImageIORegion::IndexType index(dimensions.size());

        for (unsigned int i = 0; i < dimensions.size(); i++)
        {
          this->SetDimensions(i, dimensions[i]);
          size[i] = dimensions[i];
          index[i] = 0;
        }

        region.SetSize(size);
        region.SetIndex(index);
        this->SetIORegion(region);
      }

      if (name == "orientation")
      {
        const unsigned      numDim(this->GetNumberOfDimensions());
        std::vector<double> orientation;
        StringToVector(value, orientation);

        vnl_matrix<double> testDirections(numDim, numDim);

        for (unsigned int i = 0; i < numDim; i++)
        {
          std::vector<double> componentVector;
          for (unsigned int j = 0; j < numDim; j++)
          {
            double val = orientation[i * numDim + j];
            testDirections(j, i) = val;
            componentVector.push_back(val);
          }
          this->SetDirection(i, componentVector);
        }
        // check for degenerate dimensions. this will happen
        // if the dimension of the image is 2 but the
        // direction matrix in the file is 3x3.
        // if direction matrix is degenerate, punt and set
        // directions to identity
        if (vnl_determinant(testDirections) == 0)
        {
          for (unsigned int i = 0; i < numDim; i++)
          {
            std::vector<double> componentVector;
            for (unsigned int j = 0; j < numDim; j++)
            {
              double val = i == j ? 1.0 : 0.0;
              componentVector.push_back(val);
            }
            this->SetDirection(i, componentVector);
          }
        }
      }
      if (name == "span")
      {
        StringToVector(value, this->m_Span);
      }

      if (name == "origin")
      {
        std::vector<float> origin;
        StringToVector(value, origin);

        if (this->GetNumberOfDimensions() < origin.size())
        {
          this->SetNumberOfDimensions(origin.size());
        }

        for (unsigned int i = 0; i < origin.size(); i++)
        {
          this->SetOrigin(i, origin[i] / 10.0);
        }
      }

      if (name == "roi")
      {
        StringToVector(value, this->m_Roi);
      }

      if (name == "location")
      {
        StringToVector(value, this->m_Location);
      }

      if (name == "bigendian")
      {
        if (value == "0")
        {
          this->SetByteOrderToLittleEndian();
        }
        else
        {
          this->SetByteOrderToBigEndian();
        }
      }

      // Get the binary data type
      if (name == "storage")
      {
        this->SetPixelType(IOPixelEnum::SCALAR);

        if (value == "double")
        {
          this->SetComponentType(IOComponentEnum::DOUBLE);
        }
        else if (value == "float")
        {
          this->SetComponentType(IOComponentEnum::FLOAT);
        }
        else if (value == "long")
        {
          this->SetComponentType(IOComponentEnum::LONG);
        }
        else if (value == "unsigned long")
        {
          this->SetComponentType(IOComponentEnum::ULONG);
        }
        else if (value == "int")
        {
          this->SetComponentType(IOComponentEnum::INT);
        }
        else if (value == "unsigned int")
        {
          this->SetComponentType(IOComponentEnum::UINT);
        }
        else if (value == "short")
        {
          this->SetComponentType(IOComponentEnum::SHORT);
        }
        else if (value == "unsigned short")
        {
          this->SetComponentType(IOComponentEnum::USHORT);
        }
        else if (value == "char")
        {
          this->SetComponentType(IOComponentEnum::CHAR);
        }
        else if (value == "unsigned char")
        {
          this->SetComponentType(IOComponentEnum::UCHAR);
        }
        else
        {
          itkExceptionMacro("Unknown component type: " << value);
        }
      }

      // Get the bits
      if (name == "bits")
      {
        ConvertFromString(value, this->m_Bits);
      }

      // Get the checksum
      if (name == "checksum")
      {
        ConvertFromString(value, this->m_Checksum);
      }
    }

    tokens.clear();
  }

  inFile.seekg(0, std::ios::end);
  long int fileSize = inFile.tellg();
  this->m_InputPosition = fileSize - this->GetImageSizeInBytes();

  for (unsigned int i = 0; i < this->GetNumberOfDimensions(); i++)
  {
    this->SetSpacing(i, (this->m_Roi[i] * 10) / this->GetDimensions(i));
  }
}


void
FDFImageIO::ReadVolume(void *)
{}

// const std::type_info& FDFImageIO::GetPixelType() const
// {
//   switch(m_PixelType)
//     {
//     case UCHAR:
//       return typeid(unsigned char);
//     case USHORT:
//       return typeid(unsigned short);
//     case CHAR:
//       return typeid(char);
//     case SHORT:
//       return typeid(short);
//     case UINT:
//       return typeid(unsigned int);
//     case INT:
//       return typeid(int);
//     case ULONG:
//       return typeid(unsigned long);
//     case LONG:
//       return typeid(long);
//     case FLOAT:
//       return typeid(float);
//     case DOUBLE:
//       return typeid(double);
//     case RGB:
//       return typeid(RGBPixel<unsigned char>);
//     case RGBA:
//       return typeid(RGBAPixel<unsigned char>);
//     default:
//     {
//     itkExceptionMacro ("Invalid type: " << m_PixelType << ", only unsigned char, unsigned short, RGB<unsigned char>
//     are allowed."); return this->ConvertToTypeInfo(m_PixelType);
//     }
//     case UNKNOWN:
//       itkExceptionMacro ("Unknown pixel type: " << m_PixelType);
//     }
//   return typeid(ImageIOBase::UnknownType);
// }

// unsigned int FDFImageIO::GetComponentSize() const
// {
//     switch(m_PixelType)
//     {
//     case UCHAR:
//       return sizeof(unsigned char);
//     case USHORT:
//       return sizeof(unsigned short);
//     case CHAR:
//       return sizeof(char);
//     case SHORT:
//       return sizeof(short);
//     case UINT:
//       return sizeof(unsigned int);
//     case INT:
//       return sizeof(int);
//     case ULONG:
//       return sizeof(unsigned long);
//     case LONG:
//       return sizeof(long);
//     case FLOAT:
//       return sizeof(float);
//     case DOUBLE:
//       return sizeof(double);
//     case RGB:
//       return sizeof(unsigned char);
//     case RGBA:
//       return sizeof(unsigned char);
//     case UNKNOWNPIXELTYPE:
//     default:
//     {
//     itkExceptionMacro ("Invalid type: " << m_PixelType
//                        << ", only unsigned char and unsigned short are allowed.");
//     return 0;
//     }
//     }
//
//     return 1;
// }

void
FDFImageIO::Read(void * buffer)
{
  std::ifstream inFile(m_FileName.c_str(), std::ios::in | std::ios::binary);

  // Check if there was an error opening the file
  if (!inFile)
  {
    RAISE_EXCEPTION();
  }

  inFile.seekg(this->m_InputPosition);

  if (!inFile)
  {
    RAISE_EXCEPTION();
  }

  auto * p = static_cast<char *>(buffer);

  inFile.read(p, this->GetImageSizeInBytes());

  bool success = !inFile.bad();
  inFile.close();
  if (!success)
  {
    itkExceptionMacro("Error reading image data.");
  }

  this->SwapBytesIfNecessary(buffer, this->GetImageSizeInPixels());
}


FDFImageIO::FDFImageIO() = default;

FDFImageIO::~FDFImageIO() = default;

void
FDFImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  //   os << indent << "PixelType " << m_PixelType << "\n";
  //   os << indent << "Start of image in bytes from start of file " << this->m_InputPosition << "\n";
  //   os << indent << "Number of pixels in image: " << this->GetImageSizeInPixels() << "\n";
  //   os << indent << "Image size in bytes: " << this->GetImageSizeInBytes() << "\n";
  //   os << indent << "Checksum: " << this->checksum << "\n";
  //   os << indent << "Spatial Rank: " << this->spatial_rank << "\n";
  //   os << indent << "Bits: " << this->bits << "\n";
  //   os << indent; PrintVector(os, "Matrix", this->matrix);
  //   os << indent; PrintVector(os, "Location", this->location);
  //   os << indent; PrintVector(os, "ROI", this->roi);
  //   os << indent; PrintVector(os, "Span", this->span);
}

bool
FDFImageIO::CanWriteFile(const char * /* name */)
{
  // not possible to write a fdf file
  return false;
}

void
FDFImageIO::SwapBytesIfNecessary(void * buffer, unsigned long numberOfPixels)
{
  switch (this->GetComponentType())
  {
    case IOComponentEnum::CHAR:
    {
      if (this->m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<char>::SwapRangeFromSystemToLittleEndian((char *)buffer, numberOfPixels);
      }
      else if (this->m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<char>::SwapRangeFromSystemToBigEndian((char *)buffer, numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::FLOAT:
    {
      if (this->m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<float>::SwapRangeFromSystemToLittleEndian((float *)buffer, numberOfPixels);
      }
      else if (this->m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<float>::SwapRangeFromSystemToBigEndian((float *)buffer, numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::UCHAR:
    {
      if (this->m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<unsigned char>::SwapRangeFromSystemToLittleEndian((unsigned char *)buffer, numberOfPixels);
      }
      else if (this->m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<unsigned char>::SwapRangeFromSystemToBigEndian((unsigned char *)buffer, numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::SHORT:
    {
      if (this->m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<short>::SwapRangeFromSystemToLittleEndian((short *)buffer, numberOfPixels);
      }
      else if (this->m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<short>::SwapRangeFromSystemToBigEndian((short *)buffer, numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::USHORT:
    {
      if (this->m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<unsigned short>::SwapRangeFromSystemToLittleEndian((unsigned short *)buffer, numberOfPixels);
      }
      else if (this->m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<unsigned short>::SwapRangeFromSystemToBigEndian((unsigned short *)buffer, numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::INT:
    {
      if (this->m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<int>::SwapRangeFromSystemToLittleEndian((int *)buffer, numberOfPixels);
      }
      else if (this->m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<int>::SwapRangeFromSystemToBigEndian((int *)buffer, numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::UINT:
    {
      if (this->m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<unsigned int>::SwapRangeFromSystemToLittleEndian((unsigned int *)buffer, numberOfPixels);
      }
      else if (this->m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<unsigned int>::SwapRangeFromSystemToBigEndian((unsigned int *)buffer, numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::LONG:
    {
      if (this->m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<long>::SwapRangeFromSystemToLittleEndian((long *)buffer, numberOfPixels);
      }
      else if (this->m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<long>::SwapRangeFromSystemToBigEndian((long *)buffer, numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::ULONG:
    {
      if (this->m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<unsigned long>::SwapRangeFromSystemToLittleEndian((unsigned long *)buffer, numberOfPixels);
      }
      else if (this->m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<unsigned long>::SwapRangeFromSystemToBigEndian((unsigned long *)buffer, numberOfPixels);
      }
      break;
    }
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
  }
}

void
FDFImageIO::WriteImageInformation()
{
  // not possible to write a fdf file
}

void
FDFImageIO::Write(const void * /* buffer */)
{
  // not possible to write a fdf file
}

} // end namespace itk
