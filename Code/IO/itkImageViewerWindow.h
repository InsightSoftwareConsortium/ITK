/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageViewerWindow.h
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
#ifndef __itkImageViewerWindow_h
#define __itkImageViewerWindow_h

#include "itkGlutMaster.h"

namespace itk
{


  

/** \class ImageViewerWindow
 * \brief Simple implemementation of a GLUT controled window 
 *
 * \ingroup IOFilters 
 */

class ImageViewerWindow : public GlutWindow {
public:

   ImageViewerWindow( int setWidth, 
                      int setHeight,
                      int setInitPositionX, 
                      int setInitPositionY,
                      const char * title);

   virtual ~ImageViewerWindow();

   virtual void CallBackDisplayFunc(void);
   virtual void CallBackReshapeFunc(int w, int h);   
   virtual void CallBackKeyboardFunc(unsigned char key, int x, int y);

   virtual void SetBufferSize( int w, int h );
   virtual void SetWindowSize( int w, int h );
   virtual void SetWindowPosition( int x, int y );
   virtual void SetWindowTitle( const char * title );

   /** This method initiates the event loop 
       enabling interaction with the GUI */
   static void StartInteraction();

   typedef unsigned char BufferPixelType;

   BufferPixelType * GetBuffer();

   unsigned char GetLastKeyPressed()    const;
   int           GetLastKeyModifiers()  const;

  /** Define Events for GUI interaction */
  itkEventMacro( KeyPressedEvent       , AnyEvent );
  itkEventMacro( ResizeWindowEvent     , AnyEvent );

  /** Connect an observer to the current window */
  unsigned long AddObserver(const EventObject & event, Command *);

private:

  int  m_Width;
  int  m_Height;
  int  m_InitialPositionX;
  int  m_InitialPositionY;

  static GlutMaster::Pointer  m_GlutMaster; // singleton

  BufferPixelType * m_Buffer;

  unsigned char     m_LastKeyPressed;
  int               m_LastKeyModifiers;
  
  Object::Pointer   m_Notifier;
  
};

} // end namespace itk


#endif
