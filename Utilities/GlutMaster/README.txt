
Taken from 

http://www.duke.edu/~stetten/GlutMaster/GlutMaster.html

-------------------------------------------------------------

*************************************************
GlutMaster: An Object Oriented Adaptation of GLUT
*************************************************

by George Stetten and Korin Crawford  (stetten@acpub.duke.edu)
   (beta version 0.3 - 9/9/97)


GLUT is a library-based GUI for OpenGL that runs on many platforms  
(see OpenGL, Programming for the X Window System, by Killgard, Addison Wesley).  
GLUT allows multiple simultaneous windows, each with its own event handling 
and OpenGL environment.

GlutMaster and GlutWindow are C++ objects that facilitate simultaneous 
operation of multiple GLUT windows.  GlutMaster and GlutWindow are 
simple and vanilla, and may provide a common platform for two programmers 
to compile independent code together.  

This directory contains a sample object, demoWindow, derived from GlutWindow.  
The files in this directory are shown below:


glutMaster.c++            glutWindow.c++         main.c++
glutMaster.h              glutWindow.h           Makefile
                               |
                               |
                          demoWindow.c++
                          demoWindow.h

Assuming access to the GLUT libraries, you should be able to type "make" 
and "run" to see two simple windows appear.  You may have to tailor 
the Makefile to your environment.


***  What are GlutMaster and GlutWindow?  ***

GLUT expects pointers to user-defined callback functions.  The GlutMaster 
object centralizes these into a single set of static callback functions 
through which events are funneled to member functions in window objects 
that inherit from GlutWindow.  You add standard GLUT callback functions to 
your own derivative of the GlutWindow class by overloading the virtual 
functions in GlutWindow.  

Refering to the main.c++ file, the "main" function declares a GlutMaster 
object and two DemoWindow objects.  The constructor of each new window calls 

      glutMaster->CallGlutCreateWindow();

which stores the address of that window in an array, permitting GlutMaster 
to send events to the active window by calling its member functions.  
Finally, GLUT is activated by

      glutMaster->CallGlutMainLoop();

To accomodate GLUT's idle function capability GlutMaster maintains two 
variables:

      currentIdleWindow 
      idleFunctionEnabled

with associated member functions to manipulate them.  The idle function
is defined by overloading

      GlutWindow::CallBackIdleFunc()

In main.c++ the idle function is activated for the second window by

      secondWindow->StartSpinning(glutMaster);

Only one window at a time can be the active idle window.





