# sockftpd
A websocket file transfer server (daemon) for Linux and Windows.

# 1. Description
The *sockftpd* is a TCP/IP server software that is used to transfer files to a server ( typically from a web browser ),
using a fast websocket connection.

The program is written in Free Pascal, and is provided with full source code. It relies on Indy TCP/IP socket stack, in order
to ensure stable and fast transfers.

# 2. Features

- Runs both on Linux and Windows
- MSI installer script provided (Windows only)
- MySQL integration, in order to save what was uploaded by who
- Multi user configurable
- Easy configuration, via a simple "Ini" file
- Install as a Windows service
- User quotas
- User accounts supporting any password
- User accounts supporting reading only / writing only
- Maximum file size
- PUT command
- LS command ( if mysql server is provided )
- Memory / CPU lightweight
- Configurable log file path, and ability to log straight in stdout. In Linux, logging to stdout can be coloured.
- Logging level configuration ( notice, log, warning, error levels )
- Ability to configure origins from which the server accepts connections
- Ability to choose a list with "illegal" file names, which the server will reject on upload
- Ability to choose a list with "legal" only file names, which the server will accept on upload
- Ability to integrate sockftpd with any webserver, via a simple mechanism, which can server after upload the files uploaded by users

# 3. Building / Installing

## 3.1. Under windows

### 3.1.1. Build the sockftpd daemon

You must have installed on your comuter:

*free pascal compiler* (fpc.exe should be in your path, meaning that is accessible from any directory) ( tested with 2.6.4 )
*lazarus* (installed in c:\lazarus) ( tested with 1.2.6 )

*optionally*, if you want to build a "setup.exe" file, you need to have the Inno Setup installed on your computer (tested with version 5)

build instructions:

cd into project directory.

make

if you have installed the Inno Setup on your compiler, type instead of "make":

make INNO="C:\Program Files\Inno Setup 5\Compil32.exe"

where "C:\Program Files\Inno Setup 5\Compil32.exe" is the path to the executable of the Inno Setup program.

that's it.

### 3.1.2. install sockftpd daemon as a windows service

In windows, you can install the program as a windows service in two wais:

* *visual way*: Open "config.exe" from project dir. Use the interface buttons to install / uninstall windows service
* *console way*: Use "sockftpdctl.exe" program ( --install, --uninstall, --start, --stop ) to install, uninstall, start or stop the service.

### 3.1.3. build the sockftpd daemon javascript api

You must have installed on your computer:

* *nodejs*
* *typescript* ( after you install nodejs, you run from a console: npm install -g typescript )

The tsc program should be able to run in any folder of your system ( check if it's exported in PATH ).

cd to the folder api/js/

make

If everything's fine, a SockFTPD.js file should be created in your "api/js" folder.

## 3.2. Under linux

### 3.2.1. Build SockFTPD daemon for Linux

Building has been tested under linux Ubuntu 14.04 LTS. Tests for other platforms weren't made at this point, but if you know how to install packages for your distribution, everything should work fine.

You need to have installed:

- *fpc* ( use: apt-get install fpc )
- *lazarus* ( use: apt-get install lazarus )

Please check if "lazbuild" program is found in "/usr/bin/lazbuild", otherwise you might need to edit your Makefile accordingly, in order to correct paths for compilers.

cd into project folder

make

### 3.2.2. Installing SockFTPD daemon to run as a service:

sockftpdctl --install

Configuring the service startup is made via the standard commands:

/etc/init.d/sockftpd start
/etc/init.d/sockftpd stop
/etc/init.d/sockftpd restart

### 3.2.3. Building the SockFTPD javascript api

You need to have installed the packages "nodejs" and "typescript" on computer.

cd into "api/js" folder

make

That's it.

## 3.3. General configuration

The easier way to configure sockftpd daemon is via it's config program, which runs on GUI. On windows shouldn't be a problem, but on linux you mihgt need to modify the "sockftpd.ini" file directly from your favourite text editor.
