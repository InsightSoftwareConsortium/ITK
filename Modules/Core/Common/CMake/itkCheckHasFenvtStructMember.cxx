#include <fenv.h>

int
main()
{
  fenv_t fenv;
#if defined(ITK_CHECK_FENV_T_CONTROL)
  (void)sizeof(fenv.__control);
#elif defined(ITK_CHECK_FENV_T_CONTROL_WORD)
  (void)sizeof(fenv.__control_word);
#elif defined(ITK_CHECK_FENV_T_CW)
  (void)sizeof(fenv.__cw);
#else
  (void)fenv;
#  error                                                                                                               \
    "Unknown fenv_t struct member test: Make sure to specify a compile definition of the form -DITK_CHECK_FENV_T_xxx"
#endif
  return 0;
}
