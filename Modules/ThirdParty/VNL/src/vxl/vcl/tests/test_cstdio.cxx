#include <vcl_cstdio.h>

int test_cstdio_main(int argc,char* argv[])
{
  vcl_printf( "Hello. %d %f %03x.\n", 1, 2.0f, 3 );

  bool fail = false; // global status of the test suite

  int rc; // return code

#define ASSERT(x,y)    if (!(x)) { vcl_printf("FAIL: " y "\n"); fail=true; }
#define ASSERT1(x,y,a) if (!(x)) { vcl_printf("FAIL: " y "\n",a); fail=true; }
#define ASSERT2(x,y,a,b) if (!(x)){vcl_printf("FAIL: " y "\n",a,b);fail=true;}

  // Close the standard input. All reads from
  // stdin should fail after this.
  rc = vcl_fclose(stdin);
  ASSERT(rc==0, "couldn't close standard input")

  rc = vcl_getchar();
  ASSERT(rc==EOF, "std::getchar() read a value from a closed stream")

  ASSERT(argc>=2, "no file name given as the first command line argument")
  vcl_FILE* fh = vcl_fopen( argv[1], "r" );
  ASSERT1(fh, "couldn't open %s\n      (skipping file tests)", argv[1])

  if (fh)
  {
    rc = vcl_getc( fh );
    ASSERT(rc=='t', "first character read was not 't'")

    rc = vcl_ungetc( 'x', fh );
    ASSERT(rc=='x', "ungetc failed")
    else {
      rc = vcl_getc( fh );
      ASSERT2(rc=='x', "getc returned %d, and not %d ('x') as expected",rc,'x')
    }

    rc = vcl_fclose( fh );
    ASSERT(rc==0, "failed to close file")
  }

  return fail ? 1 : 0;
}
