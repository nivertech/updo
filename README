Lightweight proper code update
==============================

Updo is an Erlang application for doing code updates without doing full OTP releases.

**NOTE: At this point, this is just an experiment!**

Updo uses OTP's release_handler to execute the update, but does not require you to create appup scripts, perfectly maintain your app files or do any of the other careful administration required for OTP releases. You just compile your code and run Updo.

When using full OTP releases you must run your node in embedded mode with a custom boot file. Updo on the other hand is used with the default interactive mode which does not need a custom boot file.

These are the steps taken by Updo:

1. Compare the loaded code with the beam files on disk to find modules that have been changed.
2. Use xref to figure out any dependencies between the changed modules (who calls whom).
3. Inspect the modules to separate supervisors, special processes like gen_server, and the rest.
4. Generate a script with appup instructions, based on the previous steps.
5. Use systools to compile this script to a lower level relup script.
6. Ask release handler to execute the low level script.

Example
-------
Say that we have a small application with the modules `foo`, `foo_sup`, `foo_server` and `foo_lib`. We also have a node up and running this application.

Then – for the sake of experimentation – we do changes to every singe module and recompile them so that the new beam files replace the old ones.

Now we can ask Updo to do a dry run of the update to see what the generated script would look like:

    1> updo:dry_run().
    [{load_module,foo,[foo_sup]},
     {update,foo_sup,supervisor},
     {update,foo_server,{advanced,[]},[foo_lib]},
     {load_module,foo_lib,[]}]

You can find the documentation for these instructions here:
http://www.erlang.org/doc/man/appup.html

If we want more detail we can ask for the low level script:

    1> updo:dry_run_low().
    [{load_object_code,{foo,[],[foo,foo_sup,foo_server,foo_lib]}},
     point_of_no_return,
     {suspend,[foo_sup]},
     {load,{foo_sup,brutal_purge,brutal_purge}},
     {load,{foo,brutal_purge,brutal_purge}},
     {code_change,up,[{foo_sup,[]}]},
     {resume,[foo_sup]},
     {suspend,[foo_server]},
     {load,{foo_lib,brutal_purge,brutal_purge}},
     {load,{foo_server,brutal_purge,brutal_purge}},
     {code_change,up,[{foo_server,[]}]},
     {resume,[foo_server]}]

To run the script and do the code update:

    16> updo:run().    
    {ok,[]}

