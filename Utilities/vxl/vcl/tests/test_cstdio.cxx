#include <vcl_cstdio.h>

int close_stdin();

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
  rc = close_stdin();
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

// Implement this below the main tests so that the includes don't
// accidently provide something that shouldn't be provided.

// Closing the input stream using a file descriptor is not portable,
// since there *may* be some systems that don't use file
// descriptors. However, I think it should work on all the common
// platforms. If someone has a conflicting platform, perhaps they
// could conditionally define the appropriate code for their platform.

// Return 0 on success, non-zero on error.

#if defined(VCL_WIN32)
# include <io.h>
int close_stdin() { return _close(0); }
#else
# include <unistd.h>
int close_stdin() { return close(0); }
#endif
