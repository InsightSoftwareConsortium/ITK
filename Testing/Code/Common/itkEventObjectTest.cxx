/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEventObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkEventObject.h"
#include <iostream>

namespace itk{ 
  itkEventMacro( TestEvent,        UserEvent );
  itkEventMacro( TestDerivedEvent, TestEvent );
  itkEventMacro( TestOtherEvent,   AnyEvent  );
  }


int itkEventObjectTest(int, char* [] ) 
{

  // test constructor
  itk::TestEvent event;  

  itk::TestDerivedEvent derivedEvent;


  // test if the event derives 
  if( !event.CheckEvent( &derivedEvent ) )
    {
    std::cerr << "Derivation test failed " << std::endl;
    return EXIT_FAILURE;
    }

  itk::TestEvent event2;
  // test if the event matches itself 
  if( !event.CheckEvent( &event2 ) )
    {
    std::cerr << "Same class test failed " << std::endl;
    return EXIT_FAILURE;
    }


  itk::TestOtherEvent otherEvent;
   // test that it doesn't match and unrelated event
  if( event.CheckEvent( &otherEvent ) )
    {
    std::cerr << "Error: matched unrelated event" << std::endl;
    return EXIT_FAILURE;
    }


  // exercise the PrintSelf() method by calling Print()
  event.Print( std::cout );

  // exercise the GetEventName() method
  std::cout << event.GetEventName() << std::endl;

  // exercise the shift operator
  std::cout << event << std::endl;

  return EXIT_SUCCESS;

}



