/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageViewer.txx
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
#ifndef _itkImageViewer_txx
#define _itkImageViewer_txx

#include "itkImageViewer.h"
#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkCommand.h"

namespace itk
{



/**
 * Constructor
 */
template <class TInputImage>
ImageViewer<TInputImage>
::ImageViewer():m_Window(0)
{

  m_SliceNumber = 0;   // by default start at the first slice 
  m_Direction   = 2;   // by default start with X-Y slices

  int width  = 100;
  int height = 100;
  int initialPositionX = 0;
  int initialPositionY = 0;
  const char * title = "";

  const InputImageType * image = this->GetInput();
  
  if( image )
    {
    width  = image->GetBufferedRegion().GetSize()[0];
    height = image->GetBufferedRegion().GetSize()[1];
    }
  
  m_Window = new ImageViewerWindow( width, 
                                    height,
                                    initialPositionX, 
                                    initialPositionY,
                                    title );

  typedef SimpleMemberCommand< Self > CommandType;

  CommandType::Pointer command = CommandType::New();

  command->SetCallbackFunction( this, & Self::KeyPressedCallback );
    
  m_Window->AddObserver( ImageViewerWindow::KeyPressedEvent(), 
                         command.GetPointer() );

  m_Calculator = CalculatorType::New();
  
  m_LastImageModifiedTime = 0;
}



 
/**
 * Destructor
 */
template <class TInputImage>
ImageViewer<TInputImage>
::~ImageViewer()
{
  if( m_Window )
    {
    delete m_Window;
    m_Window = 0;
    }
}




 
/**
 *
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::SetInput(const InputImageType * input )
{

  this->ProcessObject::SetNthInput(0, 
              const_cast<InputImageType *>( input ) );
  
  m_Calculator->SetImage( input );

}




template <class TInputImage>
const typename ImageViewer<TInputImage>::InputImageType * 
ImageViewer<TInputImage>
::GetInput(void)
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(0));
}




/**
 *
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << "ImageViewerWindow: " << m_Window << std::endl;
  
}




/**
 * Display the window in the graphic interface
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::PrepareBuffer()
{
  
  
  BufferPixelType * buffer =  m_Window->GetBuffer();

  const InputImageType * image = this->GetInput();

  if( m_LastImageModifiedTime < image->GetMTime() )
    {
    m_Calculator->Compute();
    m_Minimum =  m_Calculator->GetMinimum();
    m_Maximum =  m_Calculator->GetMaximum();
    m_LastImageModifiedTime = image->GetMTime();
    }


  const double factor = NumericTraits<BufferPixelType>::max() / ( m_Maximum - m_Minimum );

  typedef typename InputImageType::RegionType RegionType;
  typedef typename InputImageType::IndexType  IndexType;
  typedef typename InputImageType::SizeType   SizeType;

  RegionType region = image->GetBufferedRegion();
  SizeType   size   = region.GetSize();
  IndexType  start  = region.GetIndex();

  if( image->GetImageDimension() == 3 )
    {
    size[  m_Direction ] = 1;
    start[ m_Direction ] = m_SliceNumber;
    }

  region.SetSize( size );
  region.SetIndex( start );

  typedef ImageSliceConstIteratorWithIndex<InputImageType> IteratorType;
  IteratorType it( image, region );
  it.GoToBegin();

  switch( m_Direction )
    {
    case 0:
      it.SetFirstDirection(1);
      it.SetSecondDirection(2);
      break;
    case 1:
      it.SetFirstDirection(2);
      it.SetSecondDirection(0);
      break;
    case 2:
      it.SetFirstDirection(0);
      it.SetSecondDirection(1);
      break;
    }

  while( !it.IsAtEnd() )
    {
    while( !it.IsAtEndOfSlice() )
      {
      while( !it.IsAtEndOfLine() )
        {
        *buffer = static_cast<BufferPixelType>( factor * ( it.Get() - m_Minimum ) );
        ++buffer;
        ++it;
        }
      it.NextLine();
      }
    it.NextSlice();
    }

 
}



/**
 * Generate Output Information
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::GenerateOutputInformation(void)
{
}




/**
 * Generate Data
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::GenerateData()
{
  const InputImageType * image = this->GetInput();
  typename InputImageType::SizeType size = image->GetBufferedRegion().GetSize();
  switch( image->GetImageDimension() )
    { 
    case 2:
      m_Window->SetSize( size[0], size[1] );
      break;
    case 3:
      {
      switch( m_Direction )
        {
        case 0:
          m_Window->SetSize( size[1], size[2] );
          break;
        case 1:
          m_Window->SetSize( size[2], size[0] );
          break;
        case 2:
          m_Window->SetSize( size[0], size[1] );
          break;
        }
      }
    }

  this->PrepareBuffer();
  m_Window->CallBackDisplayFunc();
}



/**
 * Update() is an alias for Show(), it helps 
 * to keep a similar API with filters
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::Update()
{
  this->Show();
}




/**
 * Display the image
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::Show()
{

  InputImageType * nonConstImage = 
    const_cast<InputImageType *>( this->GetInput() );

  if( !nonConstImage ) 
      {
      return;
      }

  nonConstImage->SetRequestedRegionToLargestPossibleRegion();
  nonConstImage->Update();

  // Notify start event observers
  this->InvokeEvent( StartEvent() );

  // Actually do something
  this->GenerateData();
  
  // Notify end event observers
  this->InvokeEvent( EndEvent() );

  // Release upstream data if requested
  if ( nonConstImage->ShouldIReleaseData() )
    {
    nonConstImage->ReleaseData();
    }

}



/**
 *  Move to the next slice if the input is a 3D image
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::NextSlice()
{
  const InputImageType * image = this->GetInput();
  if( image->GetImageDimension() != 3 )
    {
    return ;
    }

  typename InputImageType::SizeType  size = 
                  image->GetBufferedRegion().GetSize();

  if( m_SliceNumber + 1 < size[ m_Direction ] )
    {
    this->SetSliceNumber( m_SliceNumber + 1 );
    }
  this->Update();
}
 



/**
 *  Move to the previous slice if the input is a 3D image
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::PreviousSlice()
{
  const InputImageType * image = this->GetInput();
  if( image->GetImageDimension() != 3 )
    {
    return ;
    }

  if( m_SliceNumber == 0 )
    {
    return;
    }

  this->SetSliceNumber( m_SliceNumber - 1 );
  
  this->Update();
}
 


/**
 *  Response to a Key pressed in the window
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::KeyPressedCallback()
{

  const int keyPressed      = static_cast<int>( m_Window->GetLastKeyPressed() );
  const int modifierPressed =                   m_Window->GetLastKeyModifiers();
  
  switch( keyPressed )
   {
   case 'f': // Forward
       this->NextSlice();
       break;
   case 'b': // Backward
       this->PreviousSlice();
       break;
   case 60: //  "<"
       if( modifierPressed & GLUT_ACTIVE_SHIFT )
         {
         // Half the size of the image
         }
       break;
   case 62: //  ">"
       if( modifierPressed & GLUT_ACTIVE_SHIFT )
         {
         // Double the size of the image
         }
       break;
   }
}



/**
 * Start the main glut loop
 */
template <class TInputImage>
void 
ImageViewer<TInputImage>
::StartInteraction()
{
  ImageViewerWindow::StartInteraction();
}





} // end namespace itk

#endif
