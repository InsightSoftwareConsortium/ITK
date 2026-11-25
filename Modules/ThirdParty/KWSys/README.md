Kitware System Library (KWSys)
==============================

KWSys provides a platform-independent API to many common system features that
are implemented differently on every platform. The library is intended to be
shared among many projects.

How to use KWSys from ITK
-------------------------

```cpp
   #include "itksys/SystemTools.hxx"

   bool MyIsDirectory(const char* fname)
   {
     return itksys::SystemTools::FileIsDirectory(fname);
   }
```

How to modify KWSys
-------------------

When adding a method to an existing class or fixing an error/warning, make no
mention of ITK. KWSys has no knowledge of ITK headers or libraries. Please
also keep in mind that KWSys must be able to build on more platforms and
compilers than ITK. Refer to existing code for conventions to ease this task.

If you want to add a class, please contact the ITK mailing list for
discussion. Please do not add a class without permission from Kitware.
