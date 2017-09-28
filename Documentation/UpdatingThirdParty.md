Updating Third Party Projects
=============================

When updating a third party project, any changes to the imported project
itself (e.g., the `ThirdParty/VNL` directory for VNL), should go through the
[UpdateThirdPartyFromUpstream.sh] framework. This framework ensures that all
patches to the third party projects are tracked externally and available for
(preferably) upstream  or other projects also embedding the library.

Any updates to projects not listed there should first convert over to this
framework.

Updating a Project
------------------

Once converted, a project should be updated by applying patches to the
repository specified in its `UpdateFromUpstream.sh` script. Once the upstream
changes are merged, pulling the changes involves running the
`UpdateFromUpstream.sh` script. This will update the local copy of the project
to the version specified in `UpdateFromUpstream.sh` (usually a `for/foo`
branch, like `for/itk` for example, but may be `master` or any other Git
reference) and merge it into the main tree.

This requires a Git 2.5 or higher due the `worktree` tool being used to
simplify the availability of the commits to the main checkout.

Here's an example of updating the `DoubleConversion` project from tag 1.1.6 to
3.0.0, starting with updating the third-party repo

```sh
   $ cd ./Modules/ThirdParty/DoubleConversion
   $ git checkout for/itk
   $ git fetch origin
   $ git rebase --onto doubleconversion-1.1.6 doubleconversion-3.0.0
   $ git push
```

Now import into ITK

```sh
   $ cd ./Modules/ThirdParty/twisted
   $ git checkout -b update_doubleconversion
   $ ./UpdateFromUpstream.sh
```

Now you can review the change and make a merge request from the branch as normal.

Porting a Project
-----------------

When converting a project, if there are any local patches, a project should be
created on
[GitHub](https://github.com/InsightSoftwareConsortium/ITK/tree/master/Modules/ThirdParty)
to track it. If the upstream project does not use Git, it should be imported
into Git (there may be existing conversions available on GitHub already). The
project's description should indicate where the source repository lives.

Once a mirror of the project is created, a branch named `for/foo` should be
created where patches for the `foo` project will be applied (i.e., `for/itk`
for ITK's patches to the project). Usually, changes to the build system, the
source code for mangling, the addition of `.gitattributes` files, and other
changes belong here. Functional changes should be submitted upstream (but may
still be tracked so that they may be used).

The basic steps to import a project `foo` based on the tag `foo-3.0.0` looks
like this:

```sh
   $ git clone https://github.com/InsightSoftwareConsortium/foo.git
   $ cd foo/
   $ git remote add insight git@github.com:InsightSoftwareConsortium/ITK.git:Modules/ThirdParty/foo.git
   $ git push -u insight
   $ git push -u insight --tags
   $ git checkout foo-3.0.0
   $ git checkout -b for/itk
   $ git push --set-upstream insight for/itk
```

Making the initial import involves filling out the project's
`UpdateFromUpstream.sh` script in its directory. The
[UpdateFromUpstream.sh](UpdateFromUpstream.sh) script describes what is
necessary, but in a nutshell, it is basically metadata such as the name of the
project and where it goes in the importing project.

Use the instructions in the comments at the top of the
[UpdateThirdPartyFromUpstream] script to know the essential parts each third
party module's `UpdateFromUpstream.sh` script should have.

Make sure `UpdateFromUpstream.sh` is executable before commit. On Unix, run:

```sh
   $ chmod u+x UpdateFromUpstream.sh && git add -u UpdateFromUpstream.sh
```

On Windows, run:

```sh
   $ git update-index --chmod=+x UpdateFromUpstream.sh
```

Also add an entry to CMakeLists.txt` and `itk-module.cmake` as appropriate.

Process
-------

The basic process involves a second branch where the third party project's
changes are tracked. This branch has a commit for each time it has been
updated and is stripped to only contain the relevant parts (no unit tests,
documentation, etc.). This branch is then merged into the main branch as a
subdirectory using the `subtree` merge strategy.

Initial conversions will require a manual push by the maintainers since the
conversion involves a root commit which is not allowed under normal
circumstances. Please send an email to the mailing list asking for assistance
if necessary.



[UpdateThirdPartyFromUpstream.sh]: Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh
