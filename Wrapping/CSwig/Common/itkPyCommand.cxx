/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPyCommand.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkPyCommand.h"

namespace itk
{

PyCommand::PyCommand()
{
    this->obj = NULL;
}

PyCommand::~PyCommand()
{
    if (this->obj)
    {
        Py_DECREF(this->obj);
    }
    this->obj = NULL;
}
    
void PyCommand::SetCommandCallable(PyObject *obj)
{
    this->obj = obj;
}

///! Execute the callback to the Tcl interpreter.
void PyCommand::Execute(Object *, const EventObject&)
{
    this->PyExecute();
}


///! Execute the callback to the Tcl interpreter with a const LightObject
void PyCommand::Execute(const Object*, const EventObject&)
{
    this->PyExecute();

}

void PyCommand::PyExecute()
{
    PyObject *result;

    result = PyEval_CallObject(this->obj, (PyObject *)NULL);

    if (result)
    {
        Py_DECREF(result);
    }
    else
    {
        PyErr_Print();
    }
}



} // namespace itk
