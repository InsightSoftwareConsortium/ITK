////////////////////////////////////////////////////////////////
//                                                            //
// glutMaster.h                                               //
// beta version 0.3 - 9/9/97)                                 //
//                                                            //
// George Stetten and Korin Crawford                          //
// copyright given to the public domain                       //
//                                                            //
// Please email comments to stetten@acpub.duke.edu,           //
//                                                            //
////////////////////////////////////////////////////////////////

#ifndef __GLUT_MASTER_H__
#define __GLUT_MASTER_H__

#include <GL/glut.h>
#include "itkGlutWindow.h"
#include "itkLightObject.h"
#include "itkObjectFactory.h"


#define MAX_NUMBER_OF_WINDOWS 256 

namespace itk {

class GlutMaster : public LightObject {

   
public:
  /** Standard class typedefs */
  typedef GlutMaster                 Self;
  typedef LightObject                Superclass;
  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(GlutMaster, LightObject );


public:
  
  void  CallGlutCreateWindow(const char * setTitle, GlutWindow * glutWindow);
  void  CallGlutMainLoop(void);

  void  DisableIdleFunction(void);
  void  EnableIdleFunction(void);
  int   IdleFunctionEnabled(void);

  int   IdleSetToCurrentWindow(void);
  void  SetIdleToCurrentWindow(void);

protected:
  GlutMaster();
  ~GlutMaster();
  GlutMaster(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
 
private:

  static void CallBackDisplayFunc(void);
  static void CallBackIdleFunc(void); 
  static void CallBackKeyboardFunc(unsigned char key, int x, int y);
  static void CallBackMotionFunc(int x, int y);
  static void CallBackMouseFunc(int button, int state, int x, int y);
  static void CallBackPassiveMotionFunc(int x, int y);
  static void CallBackReshapeFunc(int w, int h); 
  static void CallBackSpecialFunc(int key, int x, int y);   
  static void CallBackVisibilityFunc(int visible);

  static int currentIdleWindow;
  static int idleFunctionEnabled;


};

} // end namespace itk

#endif



