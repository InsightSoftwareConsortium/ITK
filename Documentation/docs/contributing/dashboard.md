ITK Dashboard Scripts
=====================

This page documents how to use the ITK `dashboard` branch in [Git]. See our
[CONTRIBUTING](index.md) guide for more information.

Using the dashboard scripts
---------------------------

The `dashboard` branch contains a dashboard client helper script. Use these
commands to track it:

```bash
mkdir -p ~/Dashboards/ITKScripts
cd ~/Dashboards/ITKScripts
git init
git remote add -t dashboard origin https://itk.org/ITK.git
git pull origin
```

The `itk_common.cmake` script contains setup instructions in its top comments.

Update the dashboard branch to get the latest version of this script by simply running

```bash
git pull origin
```

Here is
[a link](https://github.com/InsightSoftwareConsortium/ITK/blob/dashboard/itk_common.cmake)
to the `itk_common.cmake` script as it appears today.

Making changes to the dashboard scripts
---------------------------------------

If you find bugs in the hooks themselves or would like to add new features, the
can be edited in the usual Git manner:

```bash
git checkout -b my_topic_branch
```

Make your edits, test it, and commit the result. Create a patch file with:

```bash
git format-patch origin/dashboard
```

And post the results to the [ITK discussion].



[ITK discussion]: https://discourse.itk.org/

[Git]: https://git-scm.com
