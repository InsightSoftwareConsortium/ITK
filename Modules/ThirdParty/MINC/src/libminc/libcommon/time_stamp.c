/* ----------------------------- MNI Header -----------------------------------
@NAME       : time_stamp.c
@DESCRIPTION: File containing routine to create a time stamp string.
@METHOD     : 
@CREATED    : February 1, 1993 (Peter Neelin)
@MODIFIED   : 
 * $Log: time_stamp.c,v $
 * Revision 6.4  2008-01-17 02:33:02  rotor
 *  * removed all rcsids
 *  * removed a bunch of ^L's that somehow crept in
 *  * removed old (and outdated) BUGS file
 *
 * Revision 6.3  2008/01/12 19:08:14  stever
 * Add __attribute__ ((unused)) to all rcsid variables.
 *
 * Revision 6.2  2004/10/15 13:46:51  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.1  2002/01/14 21:28:26  neelin
 * Moved nd_loop, voxel_loop, ParseArgv and time_stamp from ../progs/Proglib
 * in order to include them in the main minc library.
 *
 * Revision 6.2  1999/10/19 15:57:18  neelin
 * Fixed log message containing log substitution
 *
 * Revision 6.1  1999/10/19 14:45:14  neelin
 * Fixed Log subsitutions for CVS
 *
 * Revision 6.0  1997/09/12 13:23:41  neelin
 * Release of minc version 0.6
 *
 * Revision 5.0  1997/08/21  13:24:41  neelin
 * Release of minc version 0.5
 *
 * Revision 4.0  1997/05/07  20:00:50  neelin
 * Release of minc version 0.4
 *
 * Revision 3.0  1995/05/15  19:31:35  neelin
 * Release of minc version 0.3
 *
 * Revision 2.0  1994/09/28  10:34:30  neelin
 * Release of minc version 0.2
 *
 * Revision 1.4  94/09/28  10:34:20  neelin
 * Pre-release
 * 
 * Revision 1.3  93/08/04  13:03:56  neelin
 * Added RCS Log to keep track of modifications in source
 *
@COPYRIGHT  :
              Copyright 1993 Peter Neelin, McConnell Brain Imaging Centre, 
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
---------------------------------------------------------------------------- */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>

// MS Visual Studio 12 and below.
// Defined after include of `stdio.h` in case it is already defined in that
// header file.
#if defined(_MSC_VER) && _MSC_VER <= 1800
#ifndef snprintf
#define snprintf _snprintf
#endif
#endif

#include <time.h>
#include <time_stamp.h>

/* ----------------------------- MNI Header -----------------------------------
@NAME       : time_stamp
@INPUT      : argc - number of arguments
              argv - list of arguments
@OUTPUT     : 
@RETURNS    : pointer to string containing time stamp.
@DESCRIPTION: Function to produce a time stamp string for a program.
              Returns a string of the form "date > command". The command
              is simply the concatenation of argv elements.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : February 1, 1993 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
#include <stdlib.h>

char *time_stamp(int argc, char *argv[])
{
   char *str, *the_time;
   size_t length, last;
   int i;
   static char separator[]={">>>"};
   time_t timer;

   /* Get the time, overwriting newline */
   timer = time(NULL);
   the_time = ctime(&timer);

   /* Get the total length of the string and allocate space */
   length=strlen(the_time) + strlen(separator) + 2;
   for(i=0; i<argc; i++) {
      length += strlen(argv[i]) + 1;
      if (strchr(argv[i], ' ') != NULL)
        length += 2;            /* we will need quotes! */
   }
   str = malloc(length);

   /* Copy the time and separator */
   (void) strcpy(str, the_time);
   str[strlen(str)-1]='\0';
   (void) strcat(str, separator);

   /* Copy the program name and arguments */
   for (i=0; i<argc; i++) {
      last = strlen(str);
      str[last]=' ';
      str[last+1]='\0';
      if (strchr(argv[i], ' ') != NULL) {
        (void) snprintf(&str[last+1], length - (last + 1), "\"%s\"", argv[i]);
      }
      else {
        (void) strcat(str, argv[i]);
      }
   }

   /* Add a terminating newline */
   last = strlen(str);
   str[last]='\n';
   str[last+1]='\0';


   return str;
}
