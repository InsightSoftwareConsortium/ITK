/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTkImageViewer2D.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTkImageViewer2D.h"

#include <tk.h>

namespace itk
{

TkImageViewer2D::TkImageViewer2D()
{
  m_Interpreter = 0;
  
  // Setup the internal pipeline.
  m_FlipFilter = FlipFilter::New();
  FlipFilter::FlipAxesArrayType axes;
  axes[0] = false;
  axes[1] = true;
  m_FlipFilter->SetFlipAxes(axes);
  m_RescaleFilter = RescaleFilter::New();
  m_RescaleFilter->SetInput(m_FlipFilter->GetOutput());
  m_RescaleFilter->SetOutputMinimum(0);
  m_RescaleFilter->SetOutputMaximum(255);
}

TkImageViewer2D::~TkImageViewer2D()
{
}

//----------------------------------------------------------------------------
void TkImageViewer2D::SetInterpreter(Tcl_Interp* interp)
{
  m_Interpreter = interp;
}

//----------------------------------------------------------------------------
Tcl_Interp* TkImageViewer2D::GetInterpreter() const
{
  return m_Interpreter;
}

//----------------------------------------------------------------------------
void TkImageViewer2D::SetImageName(const char* name)
{
  m_ImageName = name;
}

//----------------------------------------------------------------------------
const char* TkImageViewer2D::GetImageName() const
{
  return m_ImageName.c_str();
}

//----------------------------------------------------------------------------
void TkImageViewer2D::SetCanvasName(const char* name)
{
  m_CanvasName = name;
}

//----------------------------------------------------------------------------
const char* TkImageViewer2D::GetCanvasName() const
{
  return m_CanvasName.c_str();
}

//----------------------------------------------------------------------------
void TkImageViewer2D::SetInput(InputImageType* image)
{
  this->Superclass::SetNthInput(0, image);
}

//----------------------------------------------------------------------------
TkImageViewer2D::InputImageType* TkImageViewer2D::GetInput()
{
  DataObject* input = this->Superclass::GetInput(0);
  return dynamic_cast<InputImageType*>(input);
}

//----------------------------------------------------------------------------
void TkImageViewer2D::Draw()
{
  // Make sure we have an input image.
  InputImageType* input = this->GetInput();
  if(!input) { return; }
  
  // Connect our input to the internal pipeline.
  m_FlipFilter->SetInput(input);
  
  // Bring the image up to date.
  RescaleFilter::OutputImageType* image = m_RescaleFilter->GetOutput();
  image->UpdateOutputInformation();
  image->SetRequestedRegion(image->GetLargestPossibleRegion());
  image->Update();
  
  // Get the size of the image.
  itk::Size<2> size = image->GetLargestPossibleRegion().GetSize();
  int width = static_cast<int>(size[0]);
  int height = static_cast<int>(size[1]);
  
  // Setup the size 
  Tk_PhotoHandle photo =
    Tk_FindPhoto(m_Interpreter, const_cast<char*>(m_ImageName.c_str()));
  Tk_PhotoSetSize(photo, width, height);
  
  OStringStream command;
  command << m_CanvasName.c_str() << " configure -scrollregion \"1 1 "
          << width << " " << height << "\"";
  std::string cmdstr = command.str();
  char* cmd = new char[cmdstr.length()+1];
  strcpy(cmd, cmdstr.c_str());
  Tcl_GlobalEval(m_Interpreter, cmd);
  delete [] cmd;
  
  // Copy the image data to the Tk photo.
  unsigned char* buffer =
    reinterpret_cast<unsigned char*>(image->GetBufferPointer());
  
  Tk_PhotoImageBlock block;
  block.pixelPtr = buffer;
  block.width = width;
  block.height = height;
  block.pitch = width;
  block.pixelSize = 1;
  block.offset[0] = 0;
  block.offset[1] = 0;
  block.offset[2] = 0;
  block.offset[3] = 0;
#if (TK_MAJOR_VERSION == 8) && (TK_MINOR_VERSION < 4)
  Tk_PhotoPutBlock(photo, &block, 0, 0, size[0], size[1]);
#else
  Tk_PhotoPutBlock(photo, &block, 0, 0, size[0], size[1],
                   TK_PHOTO_COMPOSITE_SET);
#endif
}

} // namespace itk
