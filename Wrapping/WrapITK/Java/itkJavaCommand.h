#ifndef itkJavaCommand_h
#define itkJavaCommand_h
#include "itkCommand.h"
typedef itk::Command itkCommand;

class itkJavaCommand : public  itk::Command
{
public:
  virtual void Execute(itk::Object *, const itk::EventObject&){ this->Execute();};
  virtual void Execute(const itk::Object *, const itk::EventObject&){ this->Execute();}; 
  virtual void Execute(){};
};
#endif
