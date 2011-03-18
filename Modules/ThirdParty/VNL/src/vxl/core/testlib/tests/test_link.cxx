// This is to make sure that the testlib functionality can be used
// even if it is not used to create a test driver. That is, we should
// be able to link against testlib even if we don't register tests or
// we have a different main() than typical test drivers.

int main()
{
  return 0;
}
