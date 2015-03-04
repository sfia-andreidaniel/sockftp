# sockftpd
A websocket file transfer server (daemon) for Linux and Windows.

# description
The *sockftpd* is a TCP/IP server software that is used to transfer files to a server ( typically from a web browser ),
using a fast websocket connection.

The program is written in Free Pascal, and is provided with full source code. It relies on Indy TCP/IP socket stack, in order
to ensure stable and fast transfers.

# features

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

# building

## under windows

sockftpd was compiled successfully for windows using the fpc 2.6.4 i386 compiler.

in order to build the javascript api for the client, you will also need:

- nodejs
- typescript compiler ( npm install typescript )

it is recommended to have these files in your path, being able to run them from any directory on the system.

after you satisfy these requirements, you can build:

- *sockftpd*, from it's project root directory:

make 

or, if you want to build sockftpd together with a setup package ( wich will be found in install/setup-sockftpd.exe ),
you can:

make INNO="C:\Program Files (x86)\Inno Setup 5\Compil32.exe"

where "C:\Program Files (x86)\Inno Setup 5\Compil32.exe" is the path to Inno Setup .exe application.

- sockftpd *javascript api* from */api/js* folder, by issuing:

make

## under linux

sockftpd has been tested on ubuntu 12.4 lts at this point, using the fpc 2.6.4 i386 compiler. you will also need nodejs,
together with the typescript command installed ( npm install -g typescript ) in order to rebuild the javascript client
api.

after you satisfy these requirements, you can build:

- *sockftpd*, from it's project root directory:

make

- sockftpd *javascript api* from */api/js* folder, by issuing:

make

# Installation

## In windows

The recommended procedure for installing in Windows is to build first the "MSI" install package. If you do that, the msi
packager will manage installation of the server program in any folder of your system, etc., and also will manage the
uninstalling process.

All you have to do is to install the free Inno Setup Compiler, open the "install/setup.ino" file, edit it's paths in order
to fit your system paths, and hit the build button. A MSI file will be generated, which you can use it for installation
afterwards, like any windows program.

After you install the MSI package, see the section "General Configuration", in order to find out how you can setup the service.

## In linux

Assuming that you managed to build the program, you must do the following steps.

TODO.

## General configuration

SockFTPD needs a root folder where it will store it's files. The default folder in is "/srv/ftp" in linux, and "C:\srv\ftp" in Windows.

In this folder, SockFTPD will create a folder for each user account you configured in the sockftpd.ini file.

In each "user home" folder, sockftpd will create a file named ".quota", where it stores the uploaded total bytes by the
respective user.

The user home folders are automatically created, but the ftp root folder will not be created automatically, it needs to be
created by you.

Basically, if you open the sockftpd.ini file, you will be able to configure the daemon in all it's aspects.

### Installing on Windows as a service

After you created the sockftpd root folder, edited your ini file, setup a quota for each user, etc, you can install
the service by using the tool called "sockftpdctl.exe":

- sockftpdctl.exe --install - to install the service
- sockftpdctl.exe --uninstall - to uninstall the service

and

- sockftpdctl.exe --start
- sockftpdctl.exe --stop

Note that the sockftpdctl.exe file must be executed with administrative privileges under windows, in order to have
access to the windows services database. In the background the tool is using two windows native commands: SC.EXE, and
NET.EXE.

A third party program called srvstart.exe is used to provide the "run as a service feature", which is automatically
copied in your program root folder. The srvstart.exe program can be downloaded from the address:

http://www.nick.rozanski.org.uk/software

A copy of the original srvstart.exe program can be located in the "bin/" folder.
