////////////////////////////////////////////////////////////////
//                                                            //
// glutWindow.c++                                             //
// beta version 0.3 - 9/9/97)                                 //
//                                                            //
// George Stetten and Korin Crawford                          //
// copyright given to the public domain                       //
//                                                            //
// Please email comments to stetten@acpub.duke.edu,           //
//                                                            //
////////////////////////////////////////////////////////////////

#include "itkGlutWindow.h"

namespace itk {


GlutWindow
::GlutWindow(void)
{
}




GlutWindow
::~GlutWindow()
{
}



void 
GlutWindow
::SetWindowID(int newWindowID)
{
   m_WindowID = newWindowID;
}



int 
GlutWindow
::GetWindowID(void)
{
   return( m_WindowID );
}




} // end namespace itk


