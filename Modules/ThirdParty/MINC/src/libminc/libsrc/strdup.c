#include <stdlib.h>
#include <string.h>
/* ----------------------------- MNI Header -----------------------------------
@NAME       : strdup
@INPUT      : string - string to duplicate
@OUTPUT     : (none)
@RETURNS    : Pointer to duplicate string or NULL if an error occurs
@DESCRIPTION: Makes a duplicate of a string and returns a pointer to it.
@METHOD     : VAX CC rtl does not have strdup, so we provide it here to
              be included in minc.olb.
@GLOBALS    : 
@CALLS      : 
@CREATED    : June 18, 1993 (Peter Neelin)
@MODIFIED   : 
 * $Log: strdup.c,v $
 * Revision 6.1  1999-10-19 14:45:11  neelin
 * Fixed Log subsitutions for CVS
 *
 * Revision 6.0  1997/09/12 13:24:54  neelin
 * Release of minc version 0.6
 *
 * Revision 5.0  1997/08/21  13:25:53  neelin
 * Release of minc version 0.5
 *
 * Revision 4.0  1997/05/07  20:07:52  neelin
 * Release of minc version 0.4
 *
 * Revision 3.0  1995/05/15  19:33:12  neelin
 * Release of minc version 0.3
 *
 * Revision 2.0  1994/09/28  10:38:18  neelin
 * Release of minc version 0.2
 *
 * Revision 1.3  94/09/28  10:37:36  neelin
 * Pre-release
 * 
 * Revision 1.2  93/08/11  12:06:30  neelin
 * Added RCS logging in source.
 * 
---------------------------------------------------------------------------- */
char *strdup(const char *string)
{
   int length;
   char *new_string;

   /* Get the string length */
   length = strlen(string);

   /* Allocate space */
   new_string = malloc((size_t) length+1);
   if (new_string == NULL) {
      return NULL;
   }

   /* Copy the string */
   return strcpy(new_string, string);

}
