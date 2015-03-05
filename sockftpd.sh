#!/bin/bash
#
# chkconfig: 35 90 12
# description: SockFTPD server
#
# Get function from functions library

PATH=/sbin:/bin

. /lib/lsb/init-functions

# Start the service FOO
start() {
        if [ ! -f /var/lock/sockftpd ];
        then

            log_action_msg "Starting SockFTPD server..."
            %APPDIR%/sockftpd > /dev/null 2>&1 &
            ### Create the lock file ###
            echo $! > /var/lock/sockftpd
            log_action_msg "SockFTPD server started, pid: " `cat /var/lock/sockftpd`
            echo
        
        else
            
            log_action_msg "SockFTPD server appears to be allready running, pid: " `cat/var/lock/sockftpd`
            
        fi
}
# Restart the service FOO
stop() {
        if [ -f /var/lock/sockftpd ];
        then
            log_action_msg "Stopping SockFTPD server... Killing pid: " `cat "/var/lock/sockftpd"`
            kill `cat "/var/lock/sockftpd"`
            ### Now, delete the lock file ###
            rm -f /var/lock/sockftpd
            echo
        else
            log_action_msg "SockFTPD server not running"
        fi
}
### main logic ###
case "$1" in
  start)
        start
        ;;
  stop)
        stop
        ;;
  status)
        status sockftpd
        ;;
  restart|reload|condrestart)
        stop
        start
        ;;
  *)
        echo $"Usage: $0 {start|stop|restart|reload|status}"
        exit 1
esac
exit 0
