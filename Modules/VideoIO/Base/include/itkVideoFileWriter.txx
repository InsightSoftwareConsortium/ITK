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

#ifndef __itkVideoFileWriter_txx
#define __itkVideoFileWriter_txx

#include "itkNumericTraits.h"

namespace itk
{

//-CONSTRUCTOR DESTRUCTOR PRINT------------------------------------------------

//
// Constructor
//
template< class TInputImage >
VideoFileWriter< TInputImage >
::VideoFileWriter()
{
  // Initialize members
  this->m_FileName = "";
  this->m_VideoIO = NULL;
  this->m_InputImage = NULL;
  this->m_FpS = 0;
  this->m_FourCC = "";
  this->m_FirstWrite = true;
}


//
// Destructor
//
template< class TInputImage >
VideoFileWriter< TInputImage >
::~VideoFileWriter()
{
  this->FinishWriting();
}


//
// PrintSelf
//
template< class TInputImage >
void
VideoFileWriter< TInputImage >
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << this->m_FileName << std::endl;
  if (!this->m_VideoIO.IsNull())
    {
    os << indent << "VideoIO:" << std::endl;
    this->m_VideoIO->Print(os, indent.GetNextIndent());
    }
}

//-PUBLIC METHODS--------------------------------------------------------------

//
// SetInput
//
template< class TInputImage >
void
VideoFileWriter< TInputImage >
::SetInput(VideoFileWriter< TInputImage >::ImageType* input)
{
  this->m_InputImage = input;
}


//
// SetVideoIO
//
template< class TInputImage >
void
VideoFileWriter< TInputImage >
::SetVideoIO(VideoIOBase* videoIO)
{
  this->m_VideoIO = videoIO;
}


//
// Write
//
template< class TInputImage >
void
VideoFileWriter< TInputImage >
::Write()
{
  // Update the input image (trigger the pipeline update)
  if (this->m_InputImage.IsNull())
    {
    itkExceptionMacro("Cannot write with no input set");
    }
  this->m_InputImage->Update();

  // If this is the first write, make sure everything is set up
  if (this->m_FirstWrite)
    {
    // Make sure FpS and FourCC have been set
    if (this->m_FpS == 0 || this->m_FourCC.length() == 0)
      {
      itkExceptionMacro("Cannot write with FpS or FourCC unset");
      }

    // Initialize writing information
    if (!this->InitializeOutputParameters())
      {
      itkExceptionMacro("Could not initialize output parameters for writing");
      }

    // Initialize VideoIO if necessary
    if (this->m_VideoIO.IsNull() && !this->InitializeVideoIO())
      {
      itkExceptionMacro("Could not create VideoIO");
      }  

    // Set output information (first frame only)
    this->m_VideoIO->SetWriterParameters(this->m_FpS, this->m_Dimensions, this->m_FourCC.c_str(),
                                         this->m_NumberOfComponents, this->m_ComponentType);
    this->m_VideoIO->SetFileName(this->m_FileName);

    // Don't do initialization next time
    this->m_FirstWrite = false;
    }

  // Write the frame to the file
  this->m_VideoIO->Write(static_cast<void*>(
    this->m_InputImage->GetPixelContainer()->GetBufferPointer()));

}


//
// FinishWriting
//
template< class TInputImage >
void
VideoFileWriter< TInputImage >
::FinishWriting()
{
  if (!this->m_VideoIO.IsNull())
    {
    this->m_VideoIO->FinishReadingOrWriting();
    this->m_FirstWrite = true;
    }
}


//-PROTECTED METHODS-----------------------------------------------------------

//
// InitializeOutputParameters
//
template< class TInputImage >
bool
VideoFileWriter< TInputImage >
::InitializeOutputParameters()
{
  // InputImage and VideoIO must be valid
  if (this->m_InputImage.IsNull())
    {
    return false;
    }

  // Set dimensions
  this->m_Dimensions.empty();
  for (unsigned int i = 0; i < this->m_InputImage->GetImageDimension(); ++i)
    {
    this->m_Dimensions.push_back(this->m_InputImage->GetLargestPossibleRegion().GetSize()[i]);
    }

  // Set NumberOfComponents. At this point, just handle RGB and RGBA non-scalar pixels
  this->m_NumberOfComponents =
    itk::NumericTraits<typename ImageType::PixelType>::MeasurementVectorType::Length;

  return true;
}



//
// InitializeVideoIO
//
template< class TInputImage >
bool
VideoFileWriter< TInputImage >
::InitializeVideoIO()
{
  if (this->m_FileName.length() != 0)
    {
    this->m_VideoIO = itk::VideoIOFactory::CreateVideoIO(
                             itk::VideoIOFactory::WriteMode, this->m_FileName.c_str());

    // Return true if a VideoIO was sucessfully created
    if (!this->m_VideoIO.IsNull())
      {
      // Get the pixel type
      this->m_ComponentType = this->m_VideoIO->GetComponentType();

      return true;
      }
    else
      {
      return false;
      }
    }
  else
    {
    return false;
    }
}




} // end namespace itk

#endif
