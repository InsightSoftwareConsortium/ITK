/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkPyImageFilter_hxx
#define itkPyImageFilter_hxx

#include "itkPyImageFilter.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
PyImageFilter<TInputImage,TOutputImage>
::PyImageFilter()
{
    this->m_Object = ITK_NULLPTR;
}

template <class TInputImage, class TOutputImage>
PyImageFilter<TInputImage,TOutputImage>
::~PyImageFilter()
{
    if (this->m_Object)
    {
        Py_DECREF(this->m_Object);
    }
    this->m_Object = ITK_NULLPTR;
}

template <class TInputImage, class TOutputImage>
void
PyImageFilter<TInputImage,TOutputImage>
::SetPyGenerateData(PyObject *o)
{
    if (o != this->m_Object)
    {
        if (this->m_Object)
        {
            // get rid of our reference
            Py_DECREF(this->m_Object);
        }

        // store the new object
        this->m_Object = o;

        if (this->m_Object)
        {
            // take out reference (so that the calling code doesn't
            // have to keep a binding to the callable around)
            Py_INCREF(this->m_Object);
        }
    }
}


template <class TInputImage, class TOutputImage>
void
PyImageFilter<TInputImage,TOutputImage>
::GenerateData()
{
    // make sure that the CommandCallable is in fact callable
    if (!PyCallable_Check(this->m_Object))
    {
        // we throw a standard ITK exception: this makes it possible for
        // our standard Swig exception handling logic to take this
        // through to the invoking Python process
        itkExceptionMacro(<<"CommandCallable is not a callable Python object, "
                          <<"or it has not been set.");
    }
    else
    {
        PyObject *result;

        result = PyEval_CallObject(this->m_Object, (PyObject *)ITK_NULLPTR);

        if (result)
        {
            Py_DECREF(result);
        }
        else
        {
            // there was a Python error.  Clear the error by printing to stdout
            PyErr_Print();
            // make sure the invoking Python code knows there was a problem
            // by raising an exception
            itkExceptionMacro(<<"There was an error executing the "
                              <<"CommandCallable.");
        }
    }
}

} // namespace itk

#endif
