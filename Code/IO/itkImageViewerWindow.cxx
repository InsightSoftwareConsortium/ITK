/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageViewerWindow.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageViewerWindow.h"

namespace itk
{

GlutMaster::Pointer ImageViewerWindow::m_GlutMaster  = 0; // singleton



ImageViewerWindow
::ImageViewerWindow( 
              int setWidth, 
              int setHeight,
              int setInitPositionX, 
              int setInitPositionY,
              const char * title):
                  m_Width( setWidth ),
                  m_Height( setHeight ),
                  m_InitialPositionX( setInitPositionX ),
                  m_InitialPositionY( setInitPositionY ),
                  m_Buffer(0)
{

   // Initialize the singleton if necessary
  if( !m_GlutMaster )
    {
    m_GlutMaster = GlutMaster::New();
    }

  glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH | GLUT_DOUBLE);
  glutInitWindowSize( m_Width, m_Height );
  glutInitWindowPosition( m_InitialPositionX, m_InitialPositionY );
  glViewport( 0, 0, m_Width, m_Height ); 

  m_GlutMaster->CallGlutCreateWindow( title, this );

  glDisable(GL_DEPTH_TEST);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  m_Buffer = new BufferPixelType[ m_Width * m_Height ];
  m_Notifier = Object::New();
}





ImageViewerWindow
::~ImageViewerWindow()
{
  if( m_Buffer )
    {
    delete [] m_Buffer;
    }
  glutDestroyWindow( m_WindowID );
}





void 
ImageViewerWindow
::CallBackDisplayFunc(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);  
  glDrawPixels( m_Width, m_Height, 
                GL_LUMINANCE, GL_UNSIGNED_BYTE, 
                m_Buffer );
  glutSwapBuffers();
}





void 
ImageViewerWindow
::SetBufferSize(int w, int h)
{
  if( m_Width == w && m_Height == h  && m_Buffer )
    {
    return;
    }

  m_Width  = w;
  m_Height = h;
  if( m_Buffer )
    {
    delete [] m_Buffer;
    m_Buffer = new BufferPixelType[ m_Width * m_Height ];
    }
}



void 
ImageViewerWindow
::CallBackReshapeFunc(int w, int h)
{
//  Don't change m_Width, m_Height, they relate to the
//  Image, not the window.
  glViewport( 0, 0, w, h ); 
  this->CallBackDisplayFunc();
}



void 
ImageViewerWindow
::CallBackKeyboardFunc(unsigned char key, int x, int y)
{
  m_LastKeyPressed    = key;
  m_LastKeyModifiers  = glutGetModifiers();
  m_Notifier->InvokeEvent( KeyPressedEvent() );
}





void 
ImageViewerWindow
::StartInteraction()
{
  m_GlutMaster->CallGlutMainLoop();
}
   



ImageViewerWindow::BufferPixelType *
ImageViewerWindow
::GetBuffer()
{
  return m_Buffer;
}
   


void
ImageViewerWindow
::SetWindowSize( int w, int h )
{
  glutSetWindow( m_WindowID );
  glutReshapeWindow( w, h ); 
}
 


void
ImageViewerWindow
::SetWindowPosition( int x, int y )
{
  glutSetWindow( m_WindowID );
  glutPositionWindow( x, y );
}
 



unsigned char 
ImageViewerWindow
::GetLastKeyPressed() const
{
  return m_LastKeyPressed;
}



int
ImageViewerWindow
::GetLastKeyModifiers() const
{
  return m_LastKeyModifiers;
}





unsigned long
ImageViewerWindow
::AddObserver(const EventObject & event, Command * command )
{
  return  m_Notifier->AddObserver( event, command );
}




}  // end namespace itk
