// #include "itkCellBoundary.h"

/**
 * Object factory for the boundary.
 */
template <typename TCell>
itkCellBoundary<TCell>::Pointer
itkCellBoundary<TCell>
::New(void)
{
  return new Self;
}


/**
 * This is the boundary wrapper, so of course we are a boundary!
 */
template <typename TCell>
bool
itkCellBoundary<TCell>
::IsBoundary(void)
{
  return true;
}


/**
 * Add a cell to the UsingCells set.
 */
template <typename TCell>
void
itkCellBoundary<TCell>
::AddUsingCell(CellIdentifier cellId)
{
  m_UsingCells.insert(cellId);
}


/**
 * Remove a cell from the UsingCells set.
 */
template <typename TCell>
void
itkCellBoundary<TCell>
::RemoveUsingCell(CellIdentifier cellId)
{
  m_UsingCells.erase(cellId);
}


/**
 * Test if a cell is in the UsingCells set.
 */
template <typename TCell>
bool
itkCellBoundary<TCell>
::IsUsingCell(CellIdentifier cellId)
{
  return (m_UsingCells.count(cellId) > 0);
}


/**
 * Get the number of using cells.
 */
template <typename TCell>
int
itkCellBoundary<TCell>
::GetNumUsingCells(void)
{
  return m_UsingCells.size();
}


/**
 * Get a begin iterator for the UsingCells set.
 */
template <typename TCell>
itkCellBoundary<TCell>::UsingCellsContainerIterator
itkCellBoundary<TCell>
::UsingCellsBegin(void)
{
  return m_UsingCells.begin();
}


/**
 * Get an end iterator for the UsingCells set.
 */
template <typename TCell>
itkCellBoundary<TCell>::UsingCellsContainerIterator
itkCellBoundary<TCell>
::UsingCellsEnd(void)
{
  return m_UsingCells.end();
}
