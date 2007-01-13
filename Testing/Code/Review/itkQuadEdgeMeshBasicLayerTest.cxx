// -------------------------------------------------------------------------
// itkQuadEdgeMeshBasicLayerTest.cxx
// $Revision: 1.1 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-13 12:42:15 $
// -------------------------------------------------------------------------

// Program test for the basic QE layer.
#include <iostream>
#include "itkQuadEdgeMeshBaseIterator.h"
#include "itkGeometricalQuadEdge.h"

int itkQuadEdgeMeshBasicLayerTest( int , char* [] )
{
  typedef itk::GeometricalQuadEdge< int, int, bool, bool >  PrimalType;
  typedef PrimalType::Dual                                  DualType;
  PrimalType* e[5];

  //////////////////////////////////////////////////////////
  std::cout << "Creating edges" << std::endl;
  for(int i=0; i < 5; i++)
    {
    e[i] = new PrimalType;
    }
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout << "Testing MakeEdge" << std::endl;
  for( int i=0; i < 5; i++ )
    {
    e[i]->MakeEdge();
    }
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout << "Testing ring for Org() unset values" << std::endl;
  for(int i=0; i < 5; i++)
    {
    if( e[i]->IsOrgSet() )
      {
      std::cout << "IsOrgSet() should not be set for edge number " << i
        << ". Failed" << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Setting Org() and Dest() values... " << std::endl;
  int  org[5] = { 0, 1, 2, 3, 0};
  int dest[5] = { 1, 2, 3, 0, 2};

  for(int i=0; i < 5; i++ )
    {
    e[i]->SetOrg( org[i] );
    e[i]->SetDest( dest[i] );
    }
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Testing ring for Org() set values... " << std::endl;
  for( int i=0; i < 5; i++ ) {
      if ( ! e[i]->IsOrgSet() ) {
         std::cout 
                   << "IsOrgSet() NULL value failed for edge number "
                   << i << ". Failed" << std::endl;
         return EXIT_FAILURE;
      } //fi
  } // rof
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Splicing... " << std::endl;
  e[0]->Splice( e[4] );
  e[4]->Splice( e[3]->GetSym( ) );
  e[1]->Splice( e[0]->GetSym( ) );
  e[2]->Splice( e[4]->GetSym( ) );
  e[4]->GetSym( )->Splice( e[1]->GetSym( ) );
  e[3]->Splice( e[2]->GetSym( ) );
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Testing Org() and Dest() adjacency... "
            << std::endl;
  for( int i=0; i < 5; i++ )
  {
      if ( e[i]->GetOrg() != org[i] )
      {
         std::cout << std::endl 
                   << "Erroneous GetOrg() on edge number " << i
                   << ". Was expecting " << org[i]
                   << " but got " << e[i]->GetOrg()
                   << ". Failed" << std::endl;
         return EXIT_FAILURE;
      } //fi
      if ( e[i]->GetDest() != dest[i] )
      {
         std::cout << std::endl 
                   << "Erroneous GetDest() on edge number " << i
                   << ". Was expecting " << dest[i]
                   << " but got " << e[i]->GetDest()
                   << ". Failed" << std::endl;
         return EXIT_FAILURE;
      } //fi
  } // rof
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Setting faces... " << std::endl;
  e[0]->SetLeft( 0 ); 
  e[1]->SetLeft( 0 );
  e[4]->SetRight( 0 );
  e[2]->SetLeft( 1 );
  e[3]->SetLeft( 1 );
  e[4]->SetLeft( 1 );
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  typedef PrimalType::IteratorGeom IteratorGeom;
  std::cout  << "Testing Onext iterators... " << std::endl;
  int onextDest[5][3] = { { 1, 2, 3 },
                          { 2, 0, 0 },  // Last 0 is a dummy
                          { 3, 0, 1 },
                          { 0, 2, 0 },  // Last 0 is a dummy
                          { 2, 3, 1 } };
  for( int edge = 0; edge < 5; edge++ )
  {
      int test = 0;
      for( IteratorGeom itOnext = e[edge]->BeginGeomOnext( );
           itOnext != e[edge]->EndGeomOnext( );
           itOnext++, test++ )
      {
          if ( itOnext.Value( )->GetDest( ) != onextDest[edge][test] )
          {
              std::cout << std::endl 
                        << "Erroneous GetDest() on edge number " << edge
                        << ". Was expecting " << onextDest[edge][test]
                        << " but got " << itOnext.Value( )->GetDest( )
                        << ". Failed" << std::endl;
              return EXIT_FAILURE;
          } 
      }
  }
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Testing Lnext iterators... " << std::endl;
  int lnextDest[5][3] = { { 0, 1, 2 },
                          { 1, 2, 0 },
                          { 2, 3, 0 },
                          { 3, 0, 2 },
                          { 0, 2, 3 } };
  for( int edge = 0; edge < 5; edge++ )
  {
      int test = 0;
      for( IteratorGeom itLnext = e[edge]->BeginGeomLnext( );
           itLnext != e[edge]->EndGeomLnext( );
           itLnext++, test++ )
      {
          if ( *itLnext != lnextDest[edge][test] )
          {
              std::cout << std::endl 
                        << "Erroneous GetDest() on edge number " << edge
                        << ". Was expecting " << lnextDest[edge][test]
                        << " but got " << *itLnext
                        << ". Failed" << std::endl;
              return EXIT_FAILURE;
          } 
      }
  }

  std::cout << "on Sym()... " << std::endl;
  int lnextDestOnSym[3] = { 2, 0, 1 };
  int test = 0;
  for( IteratorGeom itLnext = e[4]->GetSym()->BeginGeomLnext( );
       itLnext != e[4]->GetSym()->EndGeomLnext( );
       itLnext++, test++ )
  {
      if ( *itLnext != lnextDestOnSym[test] )
      {
          std::cout << std::endl
                    << "Erroneous GetDest() on edge number 4. "
                    << "Was expecting " << lnextDestOnSym[test]
                    << " but got " << *itLnext
                    << ". Failed" << std::endl;
          return EXIT_FAILURE;
      }
  }
  std::cout << "Passed" << std::endl;


  //////////////////////////////////////////////////////////
  std::cout  << "Testing Sym iterators... " << std::endl;
  int symDest[5][3] = { { 0, 1 },
                        { 1, 2 },
                        { 2, 3 },
                        { 3, 0 },
                        { 0, 2 } };
  for( int edge = 0; edge < 5; edge++ )
  {
      int test = 0;
      for( IteratorGeom itSym = e[edge]->BeginGeomSym( );
           itSym != e[edge]->EndGeomSym( );
           itSym++, test++ )
      {
          if ( *itSym != symDest[edge][test] )
          {
              std::cout << std::endl
                        << "Erroneous GetDest() on edge number " << edge
                        << ". Was expecting " << symDest[edge][test]
                        << " but got " << *itSym
                        << ". Failed" << std::endl;
              return EXIT_FAILURE;
          }
      }
  }
  std::cout << "Passed" << std::endl;

  return EXIT_SUCCESS;
}

// eof - itkQuadEdgeMeshBasicLayerTest.cxx
