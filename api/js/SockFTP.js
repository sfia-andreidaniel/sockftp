var SockFTPTransferType;
(function (SockFTPTransferType) {
    SockFTPTransferType[SockFTPTransferType["UPLOAD"] = 0] = "UPLOAD";
    SockFTPTransferType[SockFTPTransferType["DOWNLOAD"] = 1] = "DOWNLOAD";
})(SockFTPTransferType || (SockFTPTransferType = {}));
var ConnectionState;
(function (ConnectionState) {
    ConnectionState[ConnectionState["CLOSED"] = 0] = "CLOSED";
    ConnectionState[ConnectionState["OPENED"] = 1] = "OPENED";
})(ConnectionState || (ConnectionState = {}));
var FSItem;
(function (FSItem) {
    FSItem[FSItem["FILE"] = 0] = "FILE";
    FSItem[FSItem["FOLDER"] = 1] = "FOLDER";
})(FSItem || (FSItem = {}));
var Events = (function () {
    function Events() {
        this.$EVENTS_ENABLED = true;
    }
    Events.prototype.on = function (eventName, callback) {
        this.$EVENTS_QUEUE = this.$EVENTS_QUEUE || {};
        if (!this.$EVENTS_QUEUE[eventName])
            this.$EVENTS_QUEUE[eventName] = [];
        this.$EVENTS_QUEUE[eventName].push(callback);
    };
    Events.prototype.off = function (eventName, callback) {
        if (this.$EVENTS_QUEUE && this.$EVENTS_QUEUE[eventName]) {
            for (var i = 0, len = this.$EVENTS_QUEUE[eventName].length; i < len; i++) {
                if (this.$EVENTS_QUEUE[eventName][i] == callback) {
                    this.$EVENTS_QUEUE[eventName].splice(i, 1);
                    return;
                }
            }
        }
    };
    Events.prototype.fire = function (eventName) {
        var args = [];
        for (var _i = 1; _i < arguments.length; _i++) {
            args[_i - 1] = arguments[_i];
        }
        if (this.$EVENTS_ENABLED) {
            if (this.$EVENTS_QUEUE && this.$EVENTS_QUEUE[eventName]) {
                for (var i = 0, len = this.$EVENTS_QUEUE[eventName].length; i < len; i++) {
                    this.$EVENTS_QUEUE[eventName][i].apply(this, args);
                }
            }
        }
    };
    // globally enables or disables all events fired.
    Events.prototype.setEventingState = function (enabled) {
        this.$EVENTS_ENABLED = !!enabled;
    };
    return Events;
})();
/* Standard events list that can be binded to a sockftp instance:
 *
 * log         ( type: string, ...data: any[]     ) // use this events to debug client logging
 * error       ( reason: string                   ) // use this event to fetch errors from the client. note that the "log"( type="error" ) is also triggered.
 *
 * connect                                          // fired when the connection becomes up
 * disconnect                                       // fired when the connection becomes down
 *
 * auth        ( details: SockFTPAuthDetails     ) // fired as a result of authentication.
 * transferinit( details: SockFTPTransferDetails ) // fired as a result of initiation of a transfer
 * put         ( details: SockFTPUploadDetails   ) // fired as a result of a completed upload
 * get         ( details: SockFTPDownloadDetails ) // fired as a result of a completed download
 * progress    ( details: SockFTPProgressDetails ) // fired when a progress ( upload / download ) occurs
 *
 */
var __extends = this.__extends || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var SockFTP = (function (_super) {
    __extends(SockFTP, _super);
    function SockFTP(/* host: string, port: number, userName: string, password: string */ settings) {
        _super.call(this);
        // connection settings
        this.settings = null;
        // weather authenticated or not
        this.authenticated = false;
        // the websocket connection
        this.socket = null;
        // connection state
        this.state = 0 /* CLOSED */;
        // private flag of last connection state triggered, in order to avoid firing
        // duplicate connection change events
        this.lastState = null;
        // the queue with the packets to send to the server
        this.outQueue = [];
        // the queue with current running commands on client
        this.cmdQueue = [];
        // connection internal flags
        this.checkDrain = false;
        this.packetSchedulerThreadId = null;
        this.connectionAwarenessThreadId = null;
        this.offlineAwareSeconds = 0;
        this.neverAttemptedToConnect = true;
        this.closedByMe = false;
        settings = settings || {
            "host": "127.0.0.1",
            "port": 8181,
            "user": "anonymous",
            "password": ""
        };
        this.settings = {
            "host": settings.host || "127.0.0.1",
            "port": settings.port || 8181,
            "user": settings.user || null,
            "password": settings.password || null,
            "autoAuth": typeof settings.autoAuth == 'undefined' ? true : !!settings.autoAuth,
            "reconnectTimeout": typeof settings.reconnectTimeout == 'undefined' ? 60 : settings.reconnectTimeout,
            "autoConnect": typeof settings.autoConnect == 'undefined' ? true : !!settings.autoConnect,
            "autoDisconnect": typeof settings.autoDisconnect == 'undefined' ? true : !!settings.autoDisconnect
        };
        this.authenticated = false;
        // this.connect();
        (function (me) {
            me.on('drain', function () {
                var chunk;
                if (me.outQueue.length && me.state == 1 /* OPENED */) {
                    chunk = me.outQueue.shift();
                    try {
                        me.socket.send(chunk);
                        if (me.outQueue.length == 0 && me.socket.bufferedAmount == 0) {
                            me.ondrain();
                        }
                    }
                    catch (E) {
                        me.state = 0 /* CLOSED */;
                        me.fire('state-changed');
                    }
                }
            });
            me.on('state-changed', function () {
                me.onstatechanged();
            });
        })(this);
    }
    Object.defineProperty(SockFTP.prototype, "who", {
        get: function () {
            return this.settings.user || '';
        },
        enumerable: true,
        configurable: true
    });
    SockFTP.prototype.connect = function () {
        this.neverAttemptedToConnect = false;
        this.offlineAwareSeconds = 0;
        this.closedByMe = false;
        if (this.settings.reconnectTimeout > 0) {
            this.connectionAwareness = true;
        }
        if (this.socket) {
            // won't reconnect if we're allready connected
            if (this.state == 1 /* OPENED */ && this.socket.readyState == WebSocket.OPEN) {
                return this;
            }
            // won't connect if we're allready attempting to connect
            if (this.socket.readyState == (WebSocket.CONNECTING || 0)) {
                return this;
            }
            delete this.socket;
        }
        this.socket = new WebSocket('ws://' + this.settings.host + ':' + this.settings.port + '/sockftp', 'sockftp');
        (function (me) {
            me.socket.addEventListener('open', function (evt) {
                if (me.state != 1 /* OPENED */) {
                    me.state = 1 /* OPENED */;
                    me.fire('open');
                    me.fire('state-changed');
                }
            }, false);
            me.socket.addEventListener('close', function (evt) {
                if (me.state == 1 /* OPENED */) {
                    me.state = 0 /* CLOSED */;
                    me.fire('close');
                    me.fire('state-changed');
                }
            }, false);
            me.socket.addEventListener('error', function (evt) {
                if (me.state == 1 /* OPENED */) {
                    me.state = 0 /* CLOSED */;
                    me.fire('error');
                    me.fire('state-changed');
                }
            }, false);
            me.socket.addEventListener('message', function (evt) {
                me.dispatch(evt);
            }, false);
        })(this);
        return this;
    };
    SockFTP.prototype.disconnect = function () {
        this.closedByMe = true;
        if (this.socket) {
            if (this.socket.readyState == WebSocket.CLOSING) {
                return this;
            }
            if (this.socket.readyState == WebSocket.CLOSED) {
                return this;
            }
            this.socket.close();
        }
        return this;
    };
    /* Send raw data. To not be used directly by the programmer.
       
       Trows E_INVALID_STATE exception if connection is not available in the
       moment of the sending.
     */
    SockFTP.prototype.send = function (data) {
        if (this.state == 1 /* OPENED */) {
            if (this.outQueue.length == 0 && this.socket.bufferedAmount == 0) {
                this.socket.send(data);
            }
            else {
                this.outQueue.push(data);
            }
            this.checkDrain = this.packetScheduler = true;
        }
        else {
            throw "E_INVALID_STATE";
        }
    };
    // runs the next command.
    SockFTP.prototype.next = function () {
        this.cmdQueue.shift();
        if (this.state == 1 /* OPENED */) {
            if (this.cmdQueue.length) {
                this.cmdQueue[0].init();
            }
            else {
                // if the queue becomes empty, and settings.autoDisconnect IS TRUE, we disconnect gracefully
                if (this.settings.autoDisconnect) {
                    this.disconnect();
                    this.neverAttemptedToConnect = true;
                }
            }
        }
    };
    SockFTP.prototype.addCommand = function (command, atFirst) {
        if (atFirst === void 0) { atFirst = false; }
        if (!atFirst) {
            this.cmdQueue.push(command);
        }
        else {
            this.cmdQueue.unshift(command);
        }
        if (this.settings.autoConnect && this.neverAttemptedToConnect) {
            this.connect();
        }
        if (this.state == 1 /* OPENED */) {
            if (this.cmdQueue.length == 1 || (this.cmdQueue.length && !this.cmdQueue[0].isRunning)) {
                this.cmdQueue[0].init();
            }
        }
    };
    Object.defineProperty(SockFTP.prototype, "packetScheduler", {
        get: function () {
            return this.packetSchedulerThreadId != null;
        },
        set: function (on) {
            on = !!on;
            if (on != this.packetScheduler) {
                if (on) {
                    (function (me) {
                        me.packetSchedulerThreadId = window.setInterval(function () {
                            me.packetSchedulerThread();
                        }, 30);
                    })(this);
                }
                else {
                    window.clearInterval(this.packetSchedulerThreadId);
                    this.packetSchedulerThreadId = null;
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    SockFTP.prototype.packetSchedulerThread = function () {
        if (this.checkDrain && this.state == 1 /* OPENED */ && this.packetSchedulerThreadId !== null) {
            if (this.outQueue.length == 0 && this.socket.bufferedAmount == 0) {
                this.checkDrain = false;
                this.packetScheduler = false;
                this.ondrain();
            }
            else if (this.outQueue.length && this.socket.bufferedAmount == 0) {
                this.fire('drain');
            }
        }
    };
    Object.defineProperty(SockFTP.prototype, "connectionAwareness", {
        get: function () {
            return this.connectionAwarenessThreadId != null;
        },
        set: function (on) {
            on = !!on;
            if (on != this.connectionAwareness) {
                this.offlineAwareSeconds = 0;
                if (on) {
                    (function (me) {
                        me.connectionAwarenessThreadId = window.setInterval(function () {
                            me.connectionAwarenessThread();
                        }, 1000);
                    })(this);
                }
                else {
                    window.clearInterval(this.connectionAwarenessThreadId);
                    this.connectionAwarenessThreadId = null;
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    SockFTP.prototype.connectionAwarenessThread = function () {
        if (this.settings.reconnectTimeout > 0 && this.state == 0 /* CLOSED */ && this.connectionAwarenessThreadId !== null) {
            this.offlineAwareSeconds++;
            if (this.offlineAwareSeconds > this.settings.reconnectTimeout) {
                this.connect();
            }
            else {
                this.log('reconnecting in: ' + (this.settings.reconnectTimeout - this.offlineAwareSeconds) + ' seconds');
            }
        }
        else {
            this.offlineAwareSeconds = 0;
        }
    };
    SockFTP.prototype.issueCommandId = function () {
        return ++SockFTP.$id;
    };
    SockFTP.prototype.log = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i - 0] = arguments[_i];
        }
        args.unshift('log');
        this.fire('log', args);
    };
    SockFTP.prototype.error = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i - 0] = arguments[_i];
        }
        args.unshift('error');
        this.fire('log', args);
    };
    SockFTP.prototype.warn = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i - 0] = arguments[_i];
        }
        args.unshift('warn');
        this.fire('log', args);
    };
    // handles a message from the server.
    SockFTP.prototype.dispatch = function (evt) {
        var data, i, len;
        if (evt) {
            switch (evt.type) {
                case 'message':
                    try {
                        data = JSON.parse(evt['data']);
                    }
                    catch (e) {
                        data = null;
                    }
                    if (data == null) {
                        // Failed to parse packet from server!
                        this.socket.close();
                        return;
                    }
                    if (data.id) {
                        for (i = 0, len = this.cmdQueue.length; i < len; i++) {
                            if (this.cmdQueue[i].commandID == data.id) {
                                this.cmdQueue[i].onMessage(data);
                                break;
                            }
                        }
                    }
                    else {
                        if (this.cmdQueue[0]) {
                            this.cmdQueue[0].onMessage(data);
                        }
                    }
                    break;
                default:
                    // binary message? from server? nope! we don't accept yet binary messages from server.
                    this.socket.close();
                    this.warn('Warning: Got binary message from server. Not implemented at this point');
                    break;
            }
        }
    };
    SockFTP.prototype.ls = function (path, success, error) {
        if (success === void 0) { success = null; }
        if (error === void 0) { error = null; }
        this.addCommand(new SockFTP_Command_Ls(this, path, success, error));
    };
    SockFTP.prototype.put = function (f, success, error, progress) {
        if (success === void 0) { success = null; }
        if (error === void 0) { error = null; }
        if (progress === void 0) { progress = null; }
        var command, details;
        (function (me) {
            command = new SockFTP_Command_Put(me, f, success || function () {
                me.log('PUT "' + f.name + '": OK.');
            }, error || function (reason) {
                me.error('PUT "' + f.name + '": ERROR: ' + (reason || 'Unknown upload error'));
            }, progress || function (percent, name) {
                me.log('PUT "' + f.name + '": ' + percent + '%');
            });
            me.addCommand(command);
        })(this);
        details = {
            "id": command.commandID,
            "type": 0 /* UPLOAD */,
            "name": command.fname,
            "size": command.length
        };
        try {
            this.fire('transferinit', details);
        }
        catch (E) {
        }
        return {
            "name": command.fname,
            "size": command.length,
            "type": command.type,
            "id": command.commandID,
            "ok": true
        };
    };
    // binds the uploader to a FileInput element, so that
    // any time the file changes, the file is uploaded to the server
    SockFTP.prototype.bindTo = function (input) {
        this.log('binding to: ', input);
        (function (me) {
            input.addEventListener('change', function (evt) {
                for (var i = 0, len = input.files.length; i < len; i++) {
                    me.put(input.files[i]);
                }
            }, false);
        })(this);
    };
    SockFTP.prototype.login = function (user, password, success, error) {
        this.settings.user = user || 'anonymous';
        this.settings.password = password || '';
        this.addCommand(new SockFTP_Command_Login(this, this.settings.user, this.settings.password, success, error), true);
    };
    // called each time we don't have data in the send buffer
    SockFTP.prototype.ondrain = function () {
        // forward the drain event to the current running command if any.
        // this is to ensure a smooth sending, but without interruptions in the main app thread
        var i, len;
        for (i = 0, len = this.cmdQueue.length; i < len; i++) {
            if (this.cmdQueue[i].isRunning) {
                this.cmdQueue[i].ondrain();
            }
        }
    };
    SockFTP.prototype.onstatechanged = function () {
        if (this.lastState == this.state)
            return;
        this.lastState = this.state;
        this.authenticated = false;
        if (this.state == 1 /* OPENED */) {
            if (this.settings.autoAuth && this.settings.user) {
                // send a login command
                (function (me) {
                    me.login(me.settings.user, me.settings.password || '', function () {
                        me.authenticated = true;
                        me.log('Authentication OK');
                    }, function (reason) {
                        me.state = 0 /* CLOSED */;
                        me.error('Authentication FAILED: ' + (reason || 'Unknown reason'));
                        me.onstatechanged();
                    });
                })(this);
            }
            this.warn("Client[" + this.settings.host + ":" + this.settings.port + "] -> UP");
            this.fire('connect');
            this.connectionAwareness = false;
        }
        else {
            // When the client GETS DOWN ONLY:
            this.packetScheduler = false;
            for (var i = 0, len = this.cmdQueue.length; i < len; i++) {
                // send the kill signal
                this.cmdQueue[i].kill();
            }
            while (this.cmdQueue.length > 0) {
                this.cmdQueue.shift();
            }
            // close the socket if not closed
            if (this.socket.readyState == 0 || this.socket.readyState == 1) {
                this.warn('Closing socket');
                this.socket.close();
            }
            this.warn("Client[" + this.settings.host + ":" + this.settings.port + "] -> DOWN");
            this.fire('disconnect');
            if (!this.closedByMe) {
                this.connectionAwareness = true;
            }
        }
    };
    SockFTP.prototype.getProgressDetails = function (type) {
        var i, len, result = {
            "type": type,
            "totalFilesLeft": 0,
            "totalBytesLeft": 0
        };
        for (i = 0, len = this.cmdQueue.length; i < len; i++) {
            if (this.cmdQueue[i].transferType == type) {
                switch (type) {
                    case 0 /* UPLOAD */:
                        result.totalFilesLeft++;
                        result.totalBytesLeft += (this.cmdQueue[i].length - this.cmdQueue[i].sent);
                        if (this.cmdQueue[i].isRunning) {
                            result.currentFile = this.cmdQueue[i].fname;
                            result.currentTransferSize = this.cmdQueue[i].length;
                            result.currentTransferred = this.cmdQueue[i].sent;
                            result.currentProgress = this.cmdQueue[i].percent;
                        }
                        break;
                    case 1 /* DOWNLOAD */:
                        break;
                }
            }
        }
        return result;
    };
    // command ID. Static.
    SockFTP.$id = 0;
    return SockFTP;
})(Events);
var SockFTP_Command = (function (_super) {
    __extends(SockFTP_Command, _super);
    function SockFTP_Command(client) {
        _super.call(this);
        this.onSuccess = null;
        this.onError = null;
        this.callbacksTriggered = false;
        this.doneTriggered = false;
        this.killTriggered = false;
        this.isRunning = false;
        this.transferType = null;
        this.percent = 0;
        this.client = client;
        this.commandID = client.issueCommandId();
        this.name = 'virtual';
    }
    SockFTP_Command.prototype.init = function () {
        this.isRunning = true;
    };
    // the done method can be called ONLY ONCE!
    SockFTP_Command.prototype.done = function (withError) {
        if (this.doneTriggered)
            return;
        this.doneTriggered = true;
        // does any cleanup in the Connection
        // calls the next command.
        this.client.next();
    };
    SockFTP_Command.prototype.sendText = function (data) {
        try {
            var packet = {
                cmd: this.name,
                data: data || null,
                id: this.commandID
            };
            this.client.send(JSON.stringify(packet));
        }
        catch (E) {
            this.fail('Failed to send data to network');
        }
    };
    SockFTP_Command.prototype.sendBuffer = function (data) {
        try {
            this.client.send(data);
        }
        catch (E) {
            this.fail('Failed to send data to network');
        }
    };
    SockFTP_Command.prototype.succeed = function () {
        var result = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            result[_i - 0] = arguments[_i];
        }
        if (!this.callbacksTriggered) {
            try {
                this.onSuccess.apply(this.client, result);
            }
            catch (E) {
                this.client.error('COMMAND: ' + this.name + ': Exception during succeed(): ' + E);
            }
            this.done(false);
        }
        this.callbacksTriggered = true;
        this.isRunning = false;
    };
    SockFTP_Command.prototype.fail = function (why) {
        if (!this.callbacksTriggered) {
            try {
                this.onError(why || 'Unknown error');
            }
            catch (E) {
                this.client.error('COMMAND: ' + this.name + ': Exception during fail(): ' + E);
            }
            this.done(true);
        }
        this.callbacksTriggered = true;
        this.isRunning = false;
    };
    SockFTP_Command.prototype.onMessage = function (msg) {
        if (this.callbacksTriggered) {
            throw "E_MSG_TOO_LATE";
        }
    };
    // this should be implemented on ancestors.
    SockFTP_Command.prototype.ondrain = function () {
    };
    SockFTP_Command.prototype.kill = function () {
        if (this.killTriggered)
            return;
        this.killTriggered = true;
        this.fail('Operation aborted');
    };
    return SockFTP_Command;
})(Events);
var SockFTP_Command_Login = (function (_super) {
    __extends(SockFTP_Command_Login, _super);
    function SockFTP_Command_Login(client, userName, password, success, error) {
        _super.call(this, client);
        this.userName = null;
        this.password = null;
        this.onSuccess = success;
        this.onError = error;
        this.name = 'login';
        this.userName = userName;
        this.password = password;
    }
    SockFTP_Command_Login.prototype.init = function () {
        _super.prototype.init.call(this);
        this.client.log('Sending login information...');
        this.sendText({
            "user": this.userName,
            "password": this.password
        });
    };
    SockFTP_Command_Login.prototype.onMessage = function (msg) {
        var details = {
            "ok": false,
            "user": this.userName
        };
        _super.prototype.onMessage.call(this, msg);
        if (msg && msg.ok) {
            this.client.authenticated = true;
            details.ok = true;
            try {
                this.client.fire('auth', details);
            }
            catch (E) {
            }
            this.succeed();
        }
        else if (msg && msg.error) {
            this.client.authenticated = false;
            details.error = msg.error;
            try {
                this.client.fire('auth', details);
            }
            catch (E) {
            }
            this.fail(msg.error);
        }
        else {
            details.error = 'Bad message received from server';
            try {
                this.client.fire('auth', details);
            }
            catch (E) {
            }
            this.client.authenticated = false;
            this.fail("E_BAD_MESSAGE");
        }
    };
    return SockFTP_Command_Login;
})(SockFTP_Command);
var SockFTP_Command_Put = (function (_super) {
    __extends(SockFTP_Command_Put, _super);
    function SockFTP_Command_Put(client, file, success, error, progress) {
        _super.call(this, client);
        this.file = null;
        this.sent = 0;
        this.read = 0;
        this.length = 0;
        this.type = '';
        this.fname = '';
        this.locked = false;
        this.packetSize = 32000;
        this.transferType = 0 /* UPLOAD */;
        this.progress = null;
        this.onSuccess = success;
        this.onError = error;
        this.name = 'put';
        this.file = file;
        this.sent = 0;
        this.length = this.file.size;
        this.type = this.file.type || 'application/octet-stream';
        this.fname = this.file.name;
        this.progress = progress || null;
    }
    SockFTP_Command_Put.prototype.init = function () {
        _super.prototype.init.call(this);
        this.client.log('PUT: ' + this.fname + ', length: ' + this.length + ', type: ' + this.type);
        this.sendText({
            "name": this.fname,
            "length": this.length,
            "type": this.type
        });
    };
    SockFTP_Command_Put.prototype.ondrain = function () {
        // send more bytes to server.
        if (this.locked || this.callbacksTriggered) {
            return;
        }
        this.locked = true;
        if (this.sent < this.length) {
            (function (me) {
                var reader = new FileReader();
                reader.onloadend = function (evt) {
                    if (evt.target.readyState == FileReader['DONE'] && !me.callbacksTriggered) {
                        me.sendBuffer(evt.currentTarget.result);
                        me.sent += evt.total;
                        reader = null;
                        blob = null;
                        // Update progress.
                        var progress = ~~(me.sent / (me.length / 100));
                        if (progress != me.percent) {
                            me.percent = progress;
                            if (me.progress) {
                                me.progress(me.percent, me.fname);
                            }
                        }
                        try {
                            me.client.fire('progress', me.client.getProgressDetails(0 /* UPLOAD */));
                        }
                        catch (E) {
                        }
                        me.locked = false;
                    }
                    else if (me.callbacksTriggered) {
                        me.locked = false;
                    }
                };
                var blob = me.file.slice(me.sent, me.read = Math.min(me.sent + me.packetSize, me.length));
                reader.readAsArrayBuffer(blob);
            })(this);
        }
    };
    SockFTP_Command_Put.prototype.onMessage = function (msg) {
        _super.prototype.onMessage.call(this, msg);
        var details = {
            "name": this.fname,
            "size": this.length,
            "type": this.type,
            "id": this.commandID,
            "ok": false
        };
        if (msg && msg.ok) {
            // console.warn( msg );
            details.ok = true;
            details.url = msg.file;
            this.client.log('File: ' + this.fname + ' can be accessed via url: ', "\n  ", details.url);
            try {
                this.client.fire('put', details);
            }
            catch (error) {
            }
            this.succeed();
        }
        else if (msg && msg.error) {
            details.error = msg.error;
            try {
                this.client.fire('put', details);
            }
            catch (error) {
            }
            this.fail(msg.error);
        }
        else {
            details.error = 'Bad message received from server';
            try {
                this.client.fire('put', details);
            }
            catch (error) {
            }
            this.fail("E_BAD_MESSAGE");
        }
    };
    return SockFTP_Command_Put;
})(SockFTP_Command);
var SockFTP_Command_Ls = (function (_super) {
    __extends(SockFTP_Command_Ls, _super);
    function SockFTP_Command_Ls(client, Path, success, error) {
        _super.call(this, client);
        this.name = 'ls';
        this.path = Path;
        this.onSuccess = success;
        this.onError = error;
    }
    SockFTP_Command_Ls.prototype.init = function () {
        _super.prototype.init.call(this);
        this.client.log('Fetching files list from server...');
        this.sendText({
            "path": this.path,
            "offset": 0,
            "length": 1000
        });
    };
    SockFTP_Command_Ls.prototype.onMessage = function (msg) {
        console.warn('msg: ', msg);
        _super.prototype.onMessage.call(this, msg);
        if (msg && msg.ok) {
            this.succeed(msg.data);
        }
        else if (msg && msg.error) {
            this.fail(msg.error);
        }
        else {
            this.fail("E_BAD_MESSAGE");
        }
    };
    return SockFTP_Command_Ls;
})(SockFTP_Command);
///<reference path="types.ts" />
///<reference path="Events.ts" />
///<reference path="SockFTP.ts" />
///<reference path="./SockFTP/Command.ts" />
///<reference path="./SockFTP/Command/Login.ts" />
///<reference path="./SockFTP/Command/Put.ts" />
///<reference path="./SockFTP/Command/Ls.ts" /> 
