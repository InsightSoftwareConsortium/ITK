/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkImageFileWriter.h"
#include "itkDataObject.h"
#include "itkImageIOFactory.h"
#include "itkCommand.h"

namespace itk
{

//---------------------------------------------------------
template <class TInputImage>
ImageFileWriter<TInputImage>
::ImageFileWriter():
  m_FileName(""),m_FilePrefix(""),m_FilePattern(""),
  m_ImageIO(0), m_UserSpecifiedImageIO(false),
  m_Region(0), m_UserSpecifiedRegion(false)
{
}


//---------------------------------------------------------
template <class TInputImage>
ImageFileWriter<TInputImage>
::~ImageFileWriter()
{
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::SetInput(InputImageType *input)
{
  this->ProcessObject::SetNthInput(0, input);
}


//---------------------------------------------------------
template <class TInputImage>
ImageFileWriter<TInputImage>::InputImagePointer
ImageFileWriter<TInputImage>
::GetInput()
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(0).GetPointer());
}
  
//---------------------------------------------------------
template <class TInputImage>
ImageFileWriter<TInputImage>::InputImagePointer
ImageFileWriter<TInputImage>
::GetInput(unsigned int idx)
{
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(idx).GetPointer());
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::Write()
{
  InputImagePointer input = this->GetInput();
  if ( input == 0 ) {return;}

  // make sure the data is up-to-date
  input->Update();

  // Make sure region is within the image, crop if necessary
  ImageIORegion ioRegion(TInputImage::ImageDimension);
  ImageRegion<TInputImage::ImageDimension> region = 
        input->GetLargestPossibleRegion();
  const double *spacing = input->GetSpacing();
  const double *origin = input->GetOrigin();

  m_ImageIO->SetNumberOfDimensions(TInputImage::ImageDimension);
  for(unsigned int i=0; i<TInputImage::ImageDimension; i++)
    {
    ioRegion.SetSize(i,region.GetSize(i));
    ioRegion.SetIndex(i,region.GetIndex(i));
    m_ImageIO->SetDimensions(i,region.GetSize(i));
    m_ImageIO->SetSpacing(i,spacing[i]);
    m_ImageIO->SetOrigin(i,origin[i]);
    }

  this->Write(ioRegion);        
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::Write(const ImageIORegion &ioRegion)
{
  InputImagePointer input = this->GetInput();

  // make sure input is available
  if ( input == 0 )
    {
    itkErrorMacro(<< "No input to writer!");
    return;
    }

  // Check to see if we can write the file given the name or prefix
  //
  if ( m_FileName == "" && m_FilePrefix == "" )
    {
    itkErrorMacro(<<"No filename or file prefix specified");
    return;
    }

  if ( m_ImageIO == 0 ) //try creating via factory if not set
    {
    m_UserSpecifiedImageIO = false;
    m_ImageIO = ImageIOFactory::CreateImageIO(m_FileName.c_str());
    }
  else
    {
    m_UserSpecifiedImageIO = true;
    }
  
  if ( m_ImageIO == 0 )
    {
    itkErrorMacro(<<"No ImageIO set, or none could be created.");
    return;
    }

  m_ImageIO->SetIORegion(ioRegion);

  // make sure the data is up-to-date
  input->Update();

  // Notify start event observers
  this->InvokeEvent( StartEvent() );

  // Actually do something
  this->GenerateData();
  
  // Notify end event observers
  this->InvokeEvent( EndEvent() );

  // Release upstream data if requested
  if ( this->GetInput(0)->ShouldIReleaseData() )
    {
    this->GetInput(0)->ReleaseData();
    }
}


//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::GenerateData()
{
  InputImagePointer input = this->GetInput();

  itkDebugMacro(<<"Writing file" << m_FileName);
  
  // Make sure that the image is the right type and no more than 
  // four components.
  typedef typename InputImageType::PixelType ScalarType;

  // Set the pixel and component type; the number of components.
  m_ImageIO->SetPixelType(typeid(ScalarType));  

  // Setup the image IO for writing.
  //
  m_ImageIO->SetFileName(m_FileName.c_str());
  m_ImageIO->SetFilePrefix(m_FilePrefix.c_str());

  //okay, now extract the data as a raw buffer pointer
  void* dataPtr = (void*) input->GetBufferPointer();
  m_ImageIO->Write(dataPtr);

}


//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "File Name: " 
     << (m_FileName.data() ? m_FileName.data() : "(none)") << std::endl;
  os << indent << "File Prefix: " 
     << (m_FilePrefix.data() ? m_FilePrefix.data() : "(none)") << std::endl;
  os << indent << "File Pattern: " 
     << (m_FilePattern.data() ? m_FilePattern.data() : "(none)") << std::endl;

  os << indent << "Image IO: ";
  if ( m_ImageIO == 0 )
    {
    os << "(none)\n";
    }
  else
    {
    os << m_ImageIO << "\n";
    }
  
}

} // end namespace itk
