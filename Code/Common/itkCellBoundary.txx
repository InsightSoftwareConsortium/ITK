/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellBoundary.txx
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
#ifndef _itkCellBoundary_txx
#define _itkCellBoundary_txx
#include "itkCellBoundary.h"

namespace itk
{

/**
 * This is the boundary wrapper, so of course we are a boundary!
 */
template <typename TCell>
bool
CellBoundary<TCell>
::IsBoundary(void)
{
  return true;
}


/**
 * Add a cell to the UsingCellsContainer.
 */
template <typename TCell>
void
CellBoundary<TCell>
::AddUsingCell(CellIdentifier cellId)
{
  m_UsingCells.insert(cellId);
}


/**
 * Remove a cell from the UsingCellsContainer.
 */
template <typename TCell>
void
CellBoundary<TCell>
::RemoveUsingCell(CellIdentifier cellId)
{
  m_UsingCells.erase(cellId);
}


/**
 * Test if a cell is in the UsingCellsContainer.
 */
template <typename TCell>
bool
CellBoundary<TCell>
::IsUsingCell(CellIdentifier cellId)
{
  return (m_UsingCells.count(cellId) > 0);
}


/**
 * Get the number of cells in the UsingCellsContainer.
 */
template <typename TCell>
int
CellBoundary<TCell>
::GetNumUsingCells(void)
{
  return m_UsingCells.size();
}


/**
 * Get a begin iterator for the UsingCellsContainer.
 */
template <typename TCell>
CellBoundary<TCell>::UsingCellsContainerIterator
CellBoundary<TCell>
::UsingCellsBegin(void)
{
  return m_UsingCells.begin();
}


/**
 * Get an end iterator for the UsingCellsContainer.
 */
template <typename TCell>
CellBoundary<TCell>::UsingCellsContainerIterator
CellBoundary<TCell>
::UsingCellsEnd(void)
{
  return m_UsingCells.end();
}

} // end namespace itk

#endif
