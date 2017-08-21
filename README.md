# supervisor
A supervisor process control system you can run as a cron job

## Why supervisor exists?
Sometimes you need to run a continuos background process even when working on a shared hosting (i.e. running the Laravel queue process).

In this cases you need a tool that keep watch on these processes to assure they get restarted in case the shut down (i.e. when you run the  `artisan queue:restart` command).

Supervisor accomplishes exactly this task. You run it by cron every minute and it will start and keep restarting any process you configured in its configuration file.

I build it using freepascal so it's easy to maintain and can be built on almost every platform that runs a web server. For convenience in the bin folder you can find a precompiled i868 Linux version.

## How does it work?
First download the precompiled version or build your own for the target platform where you will run it.

Then create a configuration ini file like the following:

```
[Default]
Command=/usr/bin/php
Parameters=/home/efesto/project/artisan queue:work

[Agora]
Command=/usr/bin/php
Parameters=/home/efesto/project/artisan queue:work --queue=agora

[Hermes]
Command=/usr/bin/php
Parameters=/home/efesto/project/artisan queue:work --queue=hermes

[Agora]
Command=/usr/bin/php
Parameters=/home/efesto/project/artisan queue:work --queue=agora
```

The configuration file format is pretty simple: just create a section for each process you want to run (the name of the section is irrelevant). Within each section create 2 string values named `Command` and `Parameter`. The first one must include the full path of the executable, the second one the parameters you want to pass it (this field can be empty).

Upload both the compiled version and the configuration file to your server and chmod the supervisor binary with the correct permissions (i.e. 755).

Lastly setup a cron job as follows:

```
* * * * * /the/upload/folder/supervisor /the/upload/folder/configuration.ini >> /dev/null 2>&1
```

That's it as long as the cron is executed your processes will keep running.

## How can I stop it?
If you need to stop supervisor (i.e. to make it reload the configuration) follow these steps:

1. Delete the cron job.
2. Upload an empty file named `supervisor.end` in the same folder of the supervisor binary.
3. Optionally close the processes (i.e. calling `artisan queue:restart`).

Within the next 90 seconds supervisor will terminate the processes and close itself.
