/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMException.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#include "itkFEMException.h"
#include <strstream>

namespace itk {
namespace fem {




FEMException::FEMException(const char *file, unsigned int lineNumber, std::string location) :
    ExceptionObject(file,lineNumber)
{
  SetDescription("Unhandled exception in FEM class!");
  SetLocation(location);
}




FEMExceptionIO::FEMExceptionIO(const char *file, unsigned int lineNumber, std::string location, std::string moreDescription) :
    FEMException(file,lineNumber)
{
  SetDescription("IO error in FEM class: "+moreDescription);
  SetLocation(location);
}




FEMExceptionWrongClass::FEMExceptionWrongClass(const char *file, unsigned int lineNumber, std::string location)
  : FEMException(file, lineNumber, location)
{
  SetDescription("Object was of wrong class!");
}




FEMExceptionObjectNotFound::FEMExceptionObjectNotFound(const char *file, unsigned int lineNumber, std::string location, std::string baseClassName, int GN)
  : FEMException(file, lineNumber, location)
{
  m_baseClassName=baseClassName;
  m_GN=GN;
  std::ostrstream buf;
  buf.clear();
  buf<<"Object not found ("<<m_baseClassName<<", GN="<<m_GN<<")!"<<'\0';
  SetDescription(buf.str());
}




FEMExceptionSolution::FEMExceptionSolution(const char *file, unsigned int lineNumber, std::string location, std::string moreDescription) :
  FEMException(file,lineNumber)
{
  SetDescription("Error when solving FEM problem: "+moreDescription);
  SetLocation(location);
}




}} // end namespace itk::fem
